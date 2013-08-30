{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImpredicativeTypes #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif
-- | Allocate resources which are guaranteed to be released.
--
-- For more information, see <http://www.yesodweb.com/book/conduits>.
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
      -- * A specific Exception transformer
    , ExceptionT (..)
    , runExceptionT_
    , runException
    , runException_
      -- * Registering/releasing
    , allocate
    , register
    , release
    , unprotect
    , resourceMask
      -- * Type class/associated types
    , MonadResource (..)
    , MonadUnsafeIO (..)
    , MonadThrow (..)
    , MonadActive (..)
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

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.Resource.Internal
import Control.Monad.Trans.RWS      ( RWST     )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import Control.Concurrent (ThreadId, forkIO)

import Control.Monad.ST (ST)

import qualified Control.Monad.ST.Lazy as Lazy

import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Morph




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
resourceMask = liftResourceT . resourceMaskRIO

allocateRIO :: IO a -> (a -> IO ()) -> ResourceT IO (ReleaseKey, a)
allocateRIO acquire rel = ResourceT $ \istate -> liftIO $ E.mask $ \restore -> do
    a <- restore acquire
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

register' :: I.IORef ReleaseMap
          -> IO ()
          -> IO ReleaseKey
register' istate rel = I.atomicModifyIORef istate $ \rm ->
    case rm of
        ReleaseMap key rf m ->
            ( ReleaseMap (key - 1) rf (IntMap.insert key rel m)
            , ReleaseKey istate key
            )
        ReleaseMapClosed -> throw $ InvalidAccess "register'"



release' :: I.IORef ReleaseMap
         -> Int
         -> (Maybe (IO ()) -> IO a)
         -> IO a
release' istate key act = E.mask $ \restore -> do
    maction <- I.atomicModifyIORef istate lookupAction
    restore (act maction)
  where
    lookupAction rm@(ReleaseMap next rf m) =
        case IntMap.lookup key m of
            Nothing -> (rm, Nothing)
            Just action ->
                ( ReleaseMap next rf $ IntMap.delete key m
                , Just action
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
runResourceT (ResourceT r) = do
    istate <- liftBase $ I.newIORef
        $ ReleaseMap maxBound minBound IntMap.empty
    bracket_
        (stateAlloc istate)
        (stateCleanup istate)
        (r istate)

bracket_ :: MonadBaseControl IO m => IO () -> IO () -> m a -> m a
bracket_ alloc cleanup inside =
    control $ \run -> E.bracket_ alloc cleanup (run inside)



-- | This function mirrors @join@ at the transformer level: it will collapse
-- two levels of @ResourceT@ into a single @ResourceT@.
--
-- Since 0.4.6
joinResourceT :: ResourceT (ResourceT m) a
              -> ResourceT m a
joinResourceT (ResourceT f) = ResourceT $ \r -> unResourceT (f r) r



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
        (liftBaseDiscard forkIO $ bracket_
            (return ())
            (stateCleanup r)
            (restore $ f r))



-- | Determine if some monad is still active. This is intended to prevent usage
-- of a monadic state after it has been closed.  This is necessary for such
-- cases as lazy I\/O, where an unevaluated thunk may still refer to a
-- closed @ResourceT@.
--
-- Since 0.3.0
class Monad m => MonadActive m where
    monadActive :: m Bool

instance (MonadIO m, MonadActive m) => MonadActive (ResourceT m) where
    monadActive = ResourceT $ \rmMap -> do
        rm <- liftIO $ I.readIORef rmMap
        case rm of
            ReleaseMapClosed -> return False
            _ -> monadActive -- recurse

instance MonadActive Identity where
    monadActive = return True

instance MonadActive IO where
    monadActive = return True

instance MonadActive (ST s) where
    monadActive = return True

instance MonadActive (Lazy.ST s) where
    monadActive = return True

#define GO(T) instance MonadActive m => MonadActive (T m) where monadActive = lift monadActive
#define GOX(X, T) instance (X, MonadActive m) => MonadActive (T m) where monadActive = lift monadActive
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(StateT s)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
#undef GO
#undef GOX

-- | A @Monad@ which can be used as a base for a @ResourceT@.
--
-- A @ResourceT@ has some restrictions on its base monad:
--
-- * @runResourceT@ requires an instance of @MonadBaseControl IO@.
-- * @MonadResource@ requires an instance of @MonadThrow@, @MonadUnsafeIO@, @MonadIO@, and @Applicative@.
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
type MonadResourceBase m = (MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, MonadIO m, Applicative m)
#else
class (MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, MonadIO m, Applicative m) => MonadResourceBase m
instance (MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, MonadIO m, Applicative m) => MonadResourceBase m
#endif

-- $internalState
--
-- A @ResourceT@ internally is a modified @ReaderT@ monad transformer holding
-- onto a mutable reference to all of the release actions still remaining to be
-- performed. If you are building up a custom application monad, it may be more
-- efficient to embed this @ReaderT@ functionality directly in your own monad
-- instead of wrapping around @ResourceT@ itself. This section provides you the
-- means of doing so.

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
