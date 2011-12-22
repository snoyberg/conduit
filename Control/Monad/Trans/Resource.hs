{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Control.Monad.Trans.Resource
    ( -- * Data types
      ResourceT (..)
    , ReleaseKey
      -- * Type class/associated types
    , Resource (..)
    , ResourceUnsafeIO (..)
    , ResourceIO
    , ResourceBaseIO (..)
    , ResourceThrow (..)
      -- * Use references
    , Ref
    , modifyRef
    , readRef
    , writeRef
    , newRef
      -- * Unwrap
    , runResourceT
      -- * Resource allocation
    , with
    , withIO
    , register
    , release
      -- * Special actions
    , resourceForkIO
      -- * Monad transformation
    , transResourceT
      -- * A specific Exception transformer
    , ExceptionT (..)
    , runExceptionT_
    ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Exception (SomeException)
import Control.Monad.Trans.Control
    ( MonadTransControl (..), MonadBaseControl (..)
    , ComposeSt, defaultLiftBaseWith, defaultRestoreM
    , liftBaseDiscard
    )
import qualified Data.IORef as I
import Control.Monad.Base (MonadBase, liftBase)
import Control.Applicative (Applicative (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (liftM)
import qualified Control.Exception as E
import Control.Monad.ST (ST, unsafeIOToST)
import qualified Control.Monad.ST.Lazy as Lazy
import qualified Data.STRef as S
import qualified Data.STRef.Lazy as SL
import Data.Monoid (Monoid)
import qualified Control.Exception.Lifted as L

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )

import Data.Word (Word)

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import Control.Concurrent (ThreadId, forkIO)

newRef :: Resource m => a -> ResourceT m (Ref (Base m) a)
newRef = lift . resourceLiftBase . newRef'

readRef :: Resource m => Ref (Base m) a -> ResourceT m a
readRef = lift . resourceLiftBase . readRef'

writeRef :: Resource m => Ref (Base m) a -> a -> ResourceT m ()
writeRef r = lift . resourceLiftBase . writeRef' r

modifyRef :: Resource m => Ref (Base m) a -> (a -> (a, b)) -> ResourceT m b
modifyRef r = lift . resourceLiftBase . modifyRef' r

-- | A base monad which provides mutable references and some exception-safe way
-- of interacting with them. For monads which cannot handle exceptions (e.g.,
-- 'ST'), exceptions may be ignored. However, in such cases, scarce resources
-- should /not/ be allocated in those monads, as exceptions may cause the
-- cleanup functions to not run.
--
-- The instance for 'IO', however, is fully exception-safe.
--
-- Minimal complete definition: @Ref@, @newRef'@, @readRef'@ and @writeRef'@.
class Monad m => HasRef m where
    type Ref m :: * -> *
    newRef' :: a -> m (Ref m a)
    readRef' :: Ref m a -> m a
    writeRef' :: Ref m a -> a -> m ()

    modifyRef' :: Ref m a -> (a -> (a, b)) -> m b
    modifyRef' sa f = do
        a0 <- readRef' sa
        let (a, b) = f a0
        writeRef' sa a
        return b

    mask :: ((forall a. m a -> m a) -> m b) -> m b
    mask f = f id

    mask_ :: m a -> m a
    mask_ = mask . const

    try :: m a -> m (Either SomeException a)
    try = liftM Right

instance HasRef IO where
    type Ref IO = I.IORef
    newRef' = I.newIORef
    modifyRef' = I.atomicModifyIORef
    readRef' = I.readIORef
    writeRef' = I.writeIORef
    mask = E.mask
    mask_ = E.mask_
    try = E.try

instance HasRef (ST s) where
    type Ref (ST s) = S.STRef s
    newRef' = S.newSTRef
    readRef' = S.readSTRef
    writeRef' = S.writeSTRef

instance HasRef (Lazy.ST s) where
    type Ref (Lazy.ST s) = SL.STRef s
    newRef' = SL.newSTRef
    readRef' = SL.readSTRef
    writeRef' = SL.writeSTRef

-- | A 'Monad' with a base that has mutable references, and allows some way to
-- run base actions and clean up properly.
class (HasRef (Base m), Monad m) => Resource m where
    type Base m :: * -> *

    resourceLiftBase :: Base m a -> m a
    resourceBracket_ :: Base m () -- ^ init
                     -> Base m () -- ^ cleanup
                     -> m c       -- ^ body
                     -> m c

instance Resource IO where
    type Base IO = IO
    resourceLiftBase = id
    resourceBracket_ = E.bracket_

instance Resource (ST s) where
    type Base (ST s) = ST s
    resourceLiftBase = id
    resourceBracket_ ma mb mc = do
        ma
        c <- mc
        mb
        return c

instance Resource (Lazy.ST s) where
    type Base (Lazy.ST s) = Lazy.ST s
    resourceLiftBase = id
    resourceBracket_ ma mb mc = do
        ma
        c <- mc
        mb
        return c

instance (MonadTransControl t, Resource m, Monad (t m))
        => Resource (t m) where
    type Base (t m) = Base m

    resourceLiftBase = lift . resourceLiftBase
    resourceBracket_ a b c =
        control' $ \run -> resourceBracket_ a b (run c)
      where
        control' f = liftWith f >>= restoreT . return

-- | A 'Resource' based on some monad which allows running of some 'IO'
-- actions, via unsafe calls. This applies to 'IO' and 'ST', for instance.
class Resource m => ResourceUnsafeIO m where
    unsafeFromIO :: IO a -> m a

instance ResourceUnsafeIO IO where
    unsafeFromIO = id

instance ResourceUnsafeIO (ST s) where
    unsafeFromIO = unsafeIOToST

instance ResourceUnsafeIO (Lazy.ST s) where
    unsafeFromIO = Lazy.unsafeIOToST

instance (MonadTransControl t, ResourceUnsafeIO m, Monad (t m)) => ResourceUnsafeIO (t m) where
    unsafeFromIO = lift . unsafeFromIO

class ResourceBaseIO m where
    safeFromIOBase :: IO a -> m a

instance ResourceBaseIO IO where
    safeFromIOBase = id

-- | A 'Resource' which can safely run 'IO' calls.
class (ResourceBaseIO (Base m), ResourceUnsafeIO m, ResourceThrow m,
       MonadIO m, MonadBaseControl IO m)
        => ResourceIO m

instance ResourceIO IO

instance (MonadTransControl t, ResourceIO m, Monad (t m), ResourceThrow (t m),
          MonadBaseControl IO (t m), MonadIO (t m))
        => ResourceIO (t m)

newtype ReleaseKey = ReleaseKey Int

type RefCount = Word
type NextKey = Int

data ReleaseMap base =
    ReleaseMap !NextKey !RefCount !(IntMap (base ()))

newtype ResourceT m a =
    ResourceT (Ref (Base m) (ReleaseMap (Base m)) -> m a)

with :: Resource m
     => Base m a -- ^ allocate
     -> (a -> Base m ()) -- ^ free resource
     -> ResourceT m (ReleaseKey, a)
with acquire rel = ResourceT $ \istate -> resourceLiftBase $ mask $ \restore -> do
    a <- restore acquire
    key <- register' istate $ rel a
    return (key, a)

withIO :: ResourceIO m
       => IO a -- ^ allocate
       -> (a -> IO ()) -- ^ free resource
       -> ResourceT m (ReleaseKey, a)
withIO acquire rel = ResourceT $ \istate -> resourceLiftBase $ mask $ \restore -> do
    a <- restore $ safeFromIOBase acquire
    key <- register' istate $ safeFromIOBase $ safeFromIOBase $ rel a
    return (key, a)

register :: Resource m
         => Base m ()
         -> ResourceT m ReleaseKey
register rel = ResourceT $ \istate -> resourceLiftBase $ register' istate rel

register' :: HasRef base
          => Ref base (ReleaseMap base)
          -> base ()
          -> base ReleaseKey
register' istate rel = modifyRef' istate $ \(ReleaseMap key rf m) ->
    ( ReleaseMap (key + 1) rf (IntMap.insert key rel m)
    , ReleaseKey key
    )

release :: Resource m
        => ReleaseKey
        -> ResourceT m ()
release rk = ResourceT $ \istate -> resourceLiftBase $ release' istate rk

release' :: HasRef base
         => Ref base (ReleaseMap base)
         -> ReleaseKey
         -> base ()
release' istate (ReleaseKey key) = mask $ \restore -> do
    maction <- modifyRef' istate lookupAction
    maybe (return ()) restore maction
  where
    lookupAction rm@(ReleaseMap next rf m) =
        case IntMap.lookup key m of
            Nothing -> (rm, Nothing)
            Just action ->
                ( ReleaseMap next rf $ IntMap.delete key m
                , Just action
                )

stateAlloc :: HasRef m => Ref m (ReleaseMap m) -> m ()
stateAlloc istate = do
    modifyRef' istate $ \(ReleaseMap nk rf m) ->
        (ReleaseMap nk (rf + 1) m, ())

stateCleanup :: HasRef m => Ref m (ReleaseMap m) -> m ()
stateCleanup istate = mask_ $ do
    (rf, m) <- modifyRef' istate $ \(ReleaseMap nk rf m) ->
        (ReleaseMap nk (rf - 1) m, (rf - 1, m))
    if rf == minBound
        then do
            mapM_ (\x -> try x >> return ()) $ IntMap.elems m
            -- To make sure we have no race conditions, let's put an
            -- undefined value in the state. If somehow another thread is
            -- still able to access it, at least we get clearer error
            -- messages.
            writeRef' istate $ error "Control.Monad.Trans.Resource.stateCleanup: There is a bug in the implementation. The mutable state is being accessed after cleanup. Please contact the maintainers."
        else return ()

runResourceT :: Resource m => ResourceT m a -> m a
runResourceT (ResourceT r) = do
    istate <- resourceLiftBase $ newRef'
        $ ReleaseMap minBound minBound IntMap.empty
    resourceBracket_
        (stateAlloc istate)
        (stateCleanup istate)
        (r istate)

transResourceT :: (Base m ~ Base n)
               => (m a -> n a)
               -> ResourceT m a
               -> ResourceT n a
transResourceT f (ResourceT mx) = ResourceT (\r -> f (mx r))

-------- All of our monad et al instances
instance Monad m => Functor (ResourceT m) where
    fmap f (ResourceT m) = ResourceT $ \r -> liftM f (m r)

instance Monad m => Applicative (ResourceT m) where
    pure = ResourceT . const . return
    ResourceT mf <*> ResourceT ma = ResourceT $ \r -> do
        f <- mf r
        a <- ma r
        return $ f a

instance Monad m => Monad (ResourceT m) where
    return = pure
    ResourceT ma >>= f =
        ResourceT $ \r -> ma r >>= flip un r . f
      where
        un (ResourceT x) = x

instance MonadTrans ResourceT where
    lift = ResourceT . const

instance MonadIO m => MonadIO (ResourceT m) where
    liftIO = lift . liftIO

instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase = lift . liftBase

{-
instance MonadTransControl ResourceT where
    newtype StT ResourceT a = StReader {unStReader :: a}
    liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> liftM StReader $ t r
    restoreT = ResourceT . const . liftM unStReader
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}
-}

instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
     newtype StM (ResourceT m) a = StMT (StM m a)
     liftBaseWith f = ResourceT $ \reader ->
         liftBaseWith $ \runInBase ->
             f $ liftM StMT . runInBase . (\(ResourceT r) -> r reader)
     restoreM (StMT base) = ResourceT $ const $ restoreM base

-- | The express purpose of this transformer is to allow the 'ST' monad to
-- catch exceptions via the 'ResourceThrow' typeclass.
newtype ExceptionT m a = ExceptionT { runExceptionT :: m (Either SomeException a) }

runExceptionT_ :: Monad m => ExceptionT m a -> m a
runExceptionT_ = liftM (either E.throw id) . runExceptionT

instance Monad m => Functor (ExceptionT m) where
    fmap f = ExceptionT . (liftM . fmap) f . runExceptionT
instance Monad m => Applicative (ExceptionT m) where
    pure = ExceptionT . return . Right
    ExceptionT mf <*> ExceptionT ma = ExceptionT $ do
        ef <- mf
        case ef of
            Left e -> return (Left e)
            Right f -> do
                ea <- ma
                case ea of
                    Left e -> return (Left e)
                    Right x -> return (Right (f x))
instance Monad m => Monad (ExceptionT m) where
    return = pure
    ExceptionT ma >>= f = ExceptionT $ do
        ea <- ma
        case ea of
            Left e -> return (Left e)
            Right a -> runExceptionT (f a)
instance MonadBase b m => MonadBase b (ExceptionT m) where
    liftBase = lift . liftBase
instance MonadTrans ExceptionT where
    lift = ExceptionT . liftM Right
instance MonadTransControl ExceptionT where
    newtype StT ExceptionT a = StExc { unStExc :: Either SomeException a }
    liftWith f = ExceptionT $ liftM return $ f $ liftM StExc . runExceptionT
    restoreT = ExceptionT . liftM unStExc
instance MonadBaseControl b m => MonadBaseControl b (ExceptionT m) where
    newtype StM (ExceptionT m) a = StE { unStE :: ComposeSt ExceptionT m a }
    liftBaseWith = defaultLiftBaseWith StE
    restoreM = defaultRestoreM unStE
instance (Resource m, MonadBaseControl (Base m) m)
        => ResourceThrow (ExceptionT m) where
    resourceThrow = ExceptionT . return . Left . E.toException

-- | A 'Resource' which can throw some types of exceptions.
class Resource m => ResourceThrow m where
    resourceThrow :: E.Exception e => e -> m a

instance ResourceThrow IO where
    resourceThrow = E.throwIO

#define GO(T) instance (ResourceThrow m) => ResourceThrow (T m) where resourceThrow = lift . resourceThrow
#define GOX(X, T) instance (X, ResourceThrow m) => ResourceThrow (T m) where resourceThrow = lift . resourceThrow
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

-- | Introduce a reference-counting scheme to allow a resource context to be
-- shared by multiple threads. Once the last thread exits, all remaining
-- resources will be released.
resourceForkIO :: ResourceIO m => ResourceT m () -> ResourceT m ThreadId
resourceForkIO (ResourceT f) = ResourceT $ \r -> L.mask $ \restore ->
    -- We need to make sure the counter is incremented before this call
    -- returns. Otherwise, the parent thread may call runResourceT before
    -- the child thread increments, and all resources will be freed
    -- before the child gets called.
    resourceBracket_
        (stateAlloc r)
        (return ())
        (liftBaseDiscard forkIO $ resourceBracket_
            (return ())
            (stateCleanup r)
            (restore $ f r))
