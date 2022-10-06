{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Allocate resources which are guaranteed to be released.
--
-- For more information, see <https://github.com/snoyberg/conduit/tree/master/resourcet#readme>.
--
-- One point to note: all register cleanup actions live in the @IO@ monad, not
-- the main monad. This allows both more efficient code, and for monads to be
-- transformed.
module Control.Monad.Trans.Resource
    ( -- * Data types
      ResourceT
    , ResIO
    , ReleaseKey
      -- * Unwrap
    , runResourceT
      -- ** Check cleanup exceptions
    , runResourceTChecked
    , ResourceCleanupException (..)
      -- * Special actions
    , resourceForkWith
    , resourceForkIO
      -- * Monad transformation
    , transResourceT
    , joinResourceT
      -- * Registering/releasing
    , allocate
    , allocate_
    , register
    , release
    , unprotect
    , resourceMask
      -- * Type class/associated types
    , MonadResource (..)
    , MonadResourceBase
      -- ** Low-level
    , InvalidAccess (..)
      -- * Re-exports
    , MonadUnliftIO
      -- * Internal state
      -- $internalState
    , InternalState
    , getInternalState
    , runInternalState
    , withInternalState
    , createInternalState
    , closeInternalState
      -- * Reexport
    , MonadThrow (..)
    ) where

import qualified Data.IntMap as IntMap
import qualified Data.IORef as I
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO, withRunInIO)
import qualified Control.Exception as E

import Control.Monad.Trans.Resource.Internal

import Control.Concurrent (ThreadId, forkIO)

import Control.Monad.Catch (MonadThrow, throwM)
import Data.Acquire.Internal (ReleaseType (..))



-- | Register some action that will be called precisely once, either when
-- 'runResourceT' is called, or when the 'ReleaseKey' is passed to 'release'.
--
-- Since 0.3.0
register :: MonadResource m => IO () -> m ReleaseKey
register = liftResourceT . registerRIO

-- | Call a release action early, and deregister it from the list of cleanup
-- actions to be performed.
--
-- Since 0.3.0
release :: MonadIO m => ReleaseKey -> m ()
release (ReleaseKey istate rk) = liftIO $ release' istate rk
    (maybe (return ()) id)

-- | Unprotect resource from cleanup actions; this allows you to send
-- resource into another resourcet process and reregister it there.
-- It returns a release action that should be run in order to clean
-- resource or Nothing in case if resource is already freed.
--
-- Since 0.4.5
unprotect :: MonadIO m => ReleaseKey -> m (Maybe (IO ()))
unprotect (ReleaseKey istate rk) = liftIO $ release' istate rk return

-- | Perform some allocation, and automatically register a cleanup action.
--
-- This is almost identical to calling the allocation and then
-- @register@ing the release action, but this properly handles masking of
-- asynchronous exceptions.
--
-- Since 0.3.0
allocate :: MonadResource m
         => IO a -- ^ allocate
         -> (a -> IO ()) -- ^ free resource
         -> m (ReleaseKey, a)
allocate a = liftResourceT . allocateRIO a

-- | Perform some allocation where the return value is not required, and
-- automatically register a cleanup action.
--
-- @allocate_@ is to @allocate@ as @bracket_@ is to @bracket@
--
-- This is almost identical to calling the allocation and then
-- @register@ing the release action, but this properly handles masking of
-- asynchronous exceptions.
--
-- @since 1.2.4
allocate_ :: MonadResource m
          => IO a -- ^ allocate
          -> IO () -- ^ free resource
          -> m ReleaseKey
allocate_ a = fmap fst . allocate a . const

-- | Perform asynchronous exception masking.
--
-- This is more general then @Control.Exception.mask@, yet more efficient
-- than @Control.Exception.Lifted.mask@.
--
-- Since 0.3.0
resourceMask :: MonadResource m => ((forall a. ResourceT IO a -> ResourceT IO a) -> ResourceT IO b) -> m b
resourceMask r = liftResourceT (resourceMaskRIO r)

allocateRIO :: IO a -> (a -> IO ()) -> ResourceT IO (ReleaseKey, a)
allocateRIO acquire rel = ResourceT $ \istate -> liftIO $ E.mask_ $ do
    a <- acquire
    key <- register' istate $ rel a
    return (key, a)

registerRIO :: IO () -> ResourceT IO ReleaseKey
registerRIO rel = ResourceT $ \istate -> liftIO $ register' istate rel

resourceMaskRIO :: ((forall a. ResourceT IO a -> ResourceT IO a) -> ResourceT IO b) -> ResourceT IO b
resourceMaskRIO f = ResourceT $ \istate -> liftIO $ E.mask $ \restore ->
    let ResourceT f' = f (go restore)
     in f' istate
  where
    go :: (forall a. IO a -> IO a) -> (forall a. ResourceT IO a -> ResourceT IO a)
    go r (ResourceT g) = ResourceT (\i -> r (g i))



release' :: I.IORef ReleaseMap
         -> Int
         -> (Maybe (IO ()) -> IO a)
         -> IO a
release' istate key act = E.mask_ $ do
    maction <- I.atomicModifyIORef istate lookupAction
    act maction
  where
    lookupAction rm@(ReleaseMap next rf m) =
        case IntMap.lookup key m of
            Nothing -> (rm, Nothing)
            Just action ->
                ( ReleaseMap next rf $ IntMap.delete key m
                , Just (action ReleaseEarly)
                )
    -- We tried to call release, but since the state is already closed, we
    -- can assume that the release action was already called. Previously,
    -- this threw an exception, though given that @release@ can be called
    -- from outside the context of a @ResourceT@ starting with version
    -- 0.4.4, it's no longer a library misuse or a library bug.
    lookupAction ReleaseMapClosed = (ReleaseMapClosed, Nothing)



-- | Unwrap a 'ResourceT' transformer, and call all registered release actions.
--
-- Note that there is some reference counting involved due to 'resourceForkIO'.
-- If multiple threads are sharing the same collection of resources, only the
-- last call to @runResourceT@ will deallocate the resources.
--
-- /NOTE/ Since version 1.2.0, this function will throw a
-- 'ResourceCleanupException' if any of the cleanup functions throw an
-- exception.
--
-- @since 0.3.0
runResourceT :: MonadUnliftIO m => ResourceT m a -> m a
runResourceT (ResourceT r) = withRunInIO $ \run -> do
    istate <- createInternalState
    E.mask $ \restore -> do
        res <- restore (run (r istate)) `E.catch` \e -> do
            stateCleanupChecked (Just e) istate
            E.throwIO e
        stateCleanupChecked Nothing istate
        return res

-- | Backwards compatible alias for 'runResourceT'.
--
-- @since 1.1.11
runResourceTChecked :: MonadUnliftIO m => ResourceT m a -> m a
runResourceTChecked = runResourceT
{-# INLINE runResourceTChecked #-}

bracket_ :: MonadUnliftIO m
         => IO () -- ^ allocate
         -> IO () -- ^ normal cleanup
         -> (E.SomeException -> IO ()) -- ^ exceptional cleanup
         -> m a
         -> m a
bracket_ alloc cleanupNormal cleanupExc inside =
    withRunInIO $ \run -> E.mask $ \restore -> do
        alloc
        res <- restore (run inside) `E.catch` (\e -> cleanupExc e >> E.throwIO e)
        cleanupNormal
        return res

-- | This function mirrors @join@ at the transformer level: it will collapse
-- two levels of @ResourceT@ into a single @ResourceT@.
--
-- Since 0.4.6
joinResourceT :: ResourceT (ResourceT m) a
              -> ResourceT m a
joinResourceT (ResourceT f) = ResourceT $ \r -> unResourceT (f r) r

-- | Introduce a reference-counting scheme to allow a resource context to be
-- shared by multiple threads. Once the last thread exits, all remaining
-- resources will be released.
--
-- The first parameter is a function which will be used to create the
-- thread, such as @forkIO@ or @async@.
--
-- Note that abuse of this function will greatly delay the deallocation of
-- registered resources. This function should be used with care. A general
-- guideline:
--
-- If you are allocating a resource that should be shared by multiple threads,
-- and will be held for a long time, you should allocate it at the beginning of
-- a new @ResourceT@ block and then call @resourceForkWith@ from there.
--
-- @since 1.1.9
resourceForkWith
  :: MonadUnliftIO m
  => (IO () -> IO a)
  -> ResourceT m ()
  -> ResourceT m a
resourceForkWith g (ResourceT f) =
  ResourceT $ \r -> withRunInIO $ \run -> E.mask $ \restore ->
    -- We need to make sure the counter is incremented before this call
    -- returns. Otherwise, the parent thread may call runResourceT before
    -- the child thread increments, and all resources will be freed
    -- before the child gets called.
    bracket_
        (stateAlloc r)
        (return ())
        (const $ return ())
        (g $ bracket_
            (return ())
            (stateCleanup ReleaseNormal r)
            (\e -> stateCleanup (ReleaseException' e) r)
            (restore $ run $ f r))

-- | Launch a new reference counted resource context using @forkIO@.
--
-- This is defined as @resourceForkWith forkIO@.
--
-- Note: Using regular 'forkIO' inside of a 'ResourceT' is inherently unsafe,
-- since the forked thread may try access the resources of the parent after they are cleaned up.
-- When you use 'resourceForkIO' or 'resourceForkWith', 'ResourceT' is made aware of the new thread, and will only cleanup resources when all threads finish.
-- Other concurrency mechanisms, like 'concurrently' or 'race', are safe to use.
--
-- If you encounter 'InvalidAccess' exceptions ("The mutable state is being accessed after cleanup"),
-- use of 'forkIO' is a possible culprit.
--
-- @since 0.3.0
resourceForkIO :: MonadUnliftIO m => ResourceT m () -> ResourceT m ThreadId
resourceForkIO = resourceForkWith forkIO

-- | Just use 'MonadUnliftIO' directly now, legacy explanation continues:
--
-- A @Monad@ which can be used as a base for a @ResourceT@.
--
-- A @ResourceT@ has some restrictions on its base monad:
--
-- * @runResourceT@ requires an instance of @MonadUnliftIO@.
-- * @MonadResource@ requires an instance of @MonadIO@
--
-- Note that earlier versions of @conduit@ had a typeclass @ResourceIO@. This
-- fulfills much the same role.
--
-- Since 0.3.2
type MonadResourceBase = MonadUnliftIO
{-# DEPRECATED MonadResourceBase "Use MonadUnliftIO directly instead" #-}

-- $internalState
--
-- A @ResourceT@ internally is a modified @ReaderT@ monad transformer holding
-- onto a mutable reference to all of the release actions still remaining to be
-- performed. If you are building up a custom application monad, it may be more
-- efficient to embed this @ReaderT@ functionality directly in your own monad
-- instead of wrapping around @ResourceT@ itself. This section provides you the
-- means of doing so.

-- | Create a new internal state. This state must be closed with
-- @closeInternalState@. It is your responsibility to ensure exception safety.
-- Caveat emptor!
--
-- Since 0.4.9
createInternalState :: MonadIO m => m InternalState
createInternalState = liftIO
                    $ I.newIORef
                    $ ReleaseMap maxBound (minBound + 1) IntMap.empty

-- | Close an internal state created by @createInternalState@.
--
-- Since 0.4.9
closeInternalState :: MonadIO m => InternalState -> m ()
closeInternalState = liftIO . stateCleanup ReleaseNormal

-- | Get the internal state of the current @ResourceT@.
--
-- Since 0.4.6
getInternalState :: Monad m => ResourceT m InternalState
getInternalState = ResourceT return

-- | The internal state held by a @ResourceT@ transformer.
--
-- Since 0.4.6
type InternalState = I.IORef ReleaseMap

-- | Unwrap a @ResourceT@ using the given @InternalState@.
--
-- Since 0.4.6
runInternalState :: ResourceT m a -> InternalState -> m a
runInternalState = unResourceT

-- | Run an action in the underlying monad, providing it the @InternalState@.
--
-- Since 0.4.6
withInternalState :: (InternalState -> m a) -> ResourceT m a
withInternalState = ResourceT
