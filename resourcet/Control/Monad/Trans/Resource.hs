{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif
{-# LANGUAGE Safe #-}
-- | Allocate resources which are guaranteed to be released.
--
-- For more information, see <https://www.fpcomplete.com/user/snoyberg/library-documentation/resourcet>.
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
      -- * Special actions
    , resourceForkIO
      -- * Monad transformation
    , transResourceT
    , joinResourceT
      -- * Registering/releasing
    , allocate
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
    , MonadBaseControl
      -- * Internal state
      -- $internalState
    , InternalState
    , getInternalState
    , runInternalState
    , withInternalState
    , createInternalState
    , closeInternalState
      -- * Backwards compatibility
    , ExceptionT (..)
    , runExceptionT
    , runExceptionT_
    , runException
    , runException_
    , MonadThrow (..)
    , monadThrow
    ) where

import qualified Data.IntMap as IntMap
import Control.Exception (SomeException, throw)
import Control.Monad.Trans.Control
    ( MonadBaseControl (..), liftBaseDiscard, control )
import qualified Data.IORef as I
import Control.Monad.Base (MonadBase, liftBase)
import Control.Applicative (Applicative (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (liftM)
import qualified Control.Exception as E
import Data.Monoid (Monoid)
import qualified Control.Exception.Lifted as L

import Control.Monad.Trans.Resource.Internal

import Control.Concurrent (ThreadId, forkIO)

import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Catch.Pure (CatchT, runCatchT)
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

-- | Unprotect resource from cleanup actions, this allowes you to send
-- resource into another resourcet process and reregister it there.
-- It returns an release action that should be run in order to clean
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

-- | Perform asynchronous exception masking.
--
-- This is more general then @Control.Exception.mask@, yet more efficient
-- than @Control.Exception.Lifted.mask@.
--
-- Since 0.3.0
resourceMask :: MonadResource m => ((forall a. ResourceT IO a -> ResourceT IO a) -> ResourceT IO b) -> m b
resourceMask r = liftResourceT (resourceMaskRIO r)

allocateRIO :: IO a -> (a -> IO ()) -> ResourceT IO (ReleaseKey, a)
allocateRIO acquire rel = ResourceT $ \istate -> liftIO $ E.mask $ \restore -> do
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
-- Since 0.3.0
runResourceT :: MonadBaseControl IO m => ResourceT m a -> m a
runResourceT (ResourceT r) = control $ \run -> do
    istate <- createInternalState
    E.mask $ \restore -> do
        res <- restore (run (r istate)) `E.onException`
            stateCleanup ReleaseException istate
        stateCleanup ReleaseNormal istate
        return res

bracket_ :: MonadBaseControl IO m
         => IO () -- ^ allocate
         -> IO () -- ^ normal cleanup
         -> IO () -- ^ exceptional cleanup
         -> m a
         -> m a
bracket_ alloc cleanupNormal cleanupExc inside =
    control $ \run -> E.mask $ \restore -> do
        alloc
        res <- restore (run inside) `E.onException` cleanupExc
        cleanupNormal
        return res

finally :: MonadBaseControl IO m => m a -> IO () -> m a
finally action cleanup =
    control $ \run -> E.finally (run action) cleanup

-- | This function mirrors @join@ at the transformer level: it will collapse
-- two levels of @ResourceT@ into a single @ResourceT@.
--
-- Since 0.4.6
joinResourceT :: ResourceT (ResourceT m) a
              -> ResourceT m a
joinResourceT (ResourceT f) = ResourceT $ \r -> unResourceT (f r) r

-- | For backwards compatibility.
type ExceptionT = CatchT

-- | For backwards compatibility.
runExceptionT :: ExceptionT m a -> m (Either SomeException a)
runExceptionT = runCatchT

-- | Same as 'runExceptionT', but immediately 'E.throw' any exception returned.
--
-- Since 0.3.0
runExceptionT_ :: Monad m => ExceptionT m a -> m a
runExceptionT_ = liftM (either E.throw id) . runExceptionT

-- | Run an @ExceptionT Identity@ stack.
--
-- Since 0.4.2
runException :: ExceptionT Identity a -> Either SomeException a
runException = runIdentity . runExceptionT

-- | Run an @ExceptionT Identity@ stack, but immediately 'E.throw' any exception returned.
--
-- Since 0.4.2
runException_ :: ExceptionT Identity a -> a
runException_ = runIdentity . runExceptionT_

-- | Introduce a reference-counting scheme to allow a resource context to be
-- shared by multiple threads. Once the last thread exits, all remaining
-- resources will be released.
--
-- Note that abuse of this function will greatly delay the deallocation of
-- registered resources. This function should be used with care. A general
-- guideline:
--
-- If you are allocating a resource that should be shared by multiple threads,
-- and will be held for a long time, you should allocate it at the beginning of
-- a new @ResourceT@ block and then call @resourceForkIO@ from there.
--
-- Since 0.3.0
resourceForkIO :: MonadBaseControl IO m => ResourceT m () -> ResourceT m ThreadId
resourceForkIO (ResourceT f) = ResourceT $ \r -> L.mask $ \restore ->
    -- We need to make sure the counter is incremented before this call
    -- returns. Otherwise, the parent thread may call runResourceT before
    -- the child thread increments, and all resources will be freed
    -- before the child gets called.
    bracket_
        (stateAlloc r)
        (return ())
        (return ())
        (liftBaseDiscard forkIO $ bracket_
            (return ())
            (stateCleanup ReleaseNormal r)
            (stateCleanup ReleaseException r)
            (restore $ f r))



-- | A @Monad@ which can be used as a base for a @ResourceT@.
--
-- A @ResourceT@ has some restrictions on its base monad:
--
-- * @runResourceT@ requires an instance of @MonadBaseControl IO@.
-- * @MonadResource@ requires an instance of @MonadThrow@, @MonadIO@, and @Applicative@.
--
-- While any instance of @MonadBaseControl IO@ should be an instance of the
-- other classes, this is not guaranteed by the type system (e.g., you may have
-- a transformer in your stack with does not implement @MonadThrow@). Ideally,
-- we would like to simply create an alias for the five type classes listed,
-- but this is not possible with GHC currently.
--
-- Instead, this typeclass acts as a proxy for the other five. Its only purpose
-- is to make your type signatures shorter.
--
-- Note that earlier versions of @conduit@ had a typeclass @ResourceIO@. This
-- fulfills much the same role.
--
-- Since 0.3.2
#if __GLASGOW_HASKELL__ >= 704
type MonadResourceBase m = (MonadBaseControl IO m, MonadThrow m, MonadBase IO m, MonadIO m, Applicative m)
#else
class (MonadBaseControl IO m, MonadThrow m, MonadIO m, Applicative m) => MonadResourceBase m
instance (MonadBaseControl IO m, MonadThrow m, MonadIO m, Applicative m) => MonadResourceBase m
#endif

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
createInternalState :: MonadBase IO m => m InternalState
createInternalState = liftBase
                    $ I.newIORef
                    $ ReleaseMap maxBound (minBound + 1) IntMap.empty

-- | Close an internal state created by @createInternalState@.
--
-- Since 0.4.9
closeInternalState :: MonadBase IO m => InternalState -> m ()
closeInternalState = liftBase . stateCleanup ReleaseNormal

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

-- | Backwards compatibility
monadThrow :: (E.Exception e, MonadThrow m) => e -> m a
monadThrow = throwM
