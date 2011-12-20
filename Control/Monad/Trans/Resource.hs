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
    , register
    , release
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

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

newRef :: Resource m => a -> ResourceT m (Ref (Base m) a)
newRef = lift . resourceLiftBase . newRef'

readRef :: Resource m => Ref (Base m) a -> ResourceT m a
readRef = lift . resourceLiftBase . readRef'

writeRef :: Resource m => Ref (Base m) a -> a -> ResourceT m ()
writeRef r = lift . resourceLiftBase . writeRef' r

modifyRef :: Resource m => Ref (Base m) a -> (a -> (a, b)) -> ResourceT m b
modifyRef r = lift . resourceLiftBase . modifyRef' r

class Monad m => HasRef m where
    type Ref m :: * -> *
    newRef' :: a -> m (Ref m a)
    modifyRef' :: Ref m a -> (a -> (a, b)) -> m b
    readRef' :: Ref m a -> m a
    writeRef' :: Ref m a -> a -> m ()
    mask :: ((forall a. m a -> m a) -> m b) -> m b
    mask_ :: m a -> m a
    finally' :: m a -> m b -> m a
    try :: m a -> m (Either SomeException a)

instance HasRef IO where
    type Ref IO = I.IORef
    newRef' = I.newIORef
    modifyRef' = I.atomicModifyIORef
    readRef' = I.readIORef
    writeRef' = I.writeIORef
    mask = E.mask
    mask_ = E.mask_
    finally' = E.finally
    try = E.try

instance HasRef (ST s) where
    type Ref (ST s) = S.STRef s
    newRef' = S.newSTRef
    modifyRef' sa f = do
        a0 <- S.readSTRef sa
        let (a, b) = f a0
        S.writeSTRef sa a
        return b
    readRef' = S.readSTRef
    writeRef' = S.writeSTRef
    mask f = f id
    mask_ = id
    finally' ma mb = ma >>= \a -> mb >> return a
    try = fmap Right

instance HasRef (Lazy.ST s) where
    type Ref (Lazy.ST s) = SL.STRef s
    newRef' = SL.newSTRef
    modifyRef' sa f = do
        a0 <- SL.readSTRef sa
        let (a, b) = f a0
        SL.writeSTRef sa a
        return b
    readRef' = SL.readSTRef
    writeRef' = SL.writeSTRef
    mask f = f id
    mask_ = id
    finally' ma mb = ma >>= \a -> mb >> return a
    try = fmap Right

class (HasRef (Base m), Monad m) => Resource m where
    type Base m :: * -> *

    resourceLiftBase :: Base m a -> m a
    resourceFinally :: m a -> Base m b -> m a

-- | A 'Resource' based on some monad which allows running of some 'IO'
-- actions, via unsafe calls. This applies to 'IO' and 'ST', for instance.
class Resource m => ResourceUnsafeIO m where
    unsafeFromIO :: IO a -> m a

-- | A 'Resource' which can throw some types of exceptions.
class Resource m => ResourceThrow m where
    resourceThrow :: E.Exception e => e -> m a

instance Resource IO where
    type Base IO = IO
    resourceLiftBase = id
    resourceFinally = E.finally

instance ResourceUnsafeIO IO where
    unsafeFromIO = id

instance Resource (ST s) where
    type Base (ST s) = ST s

    resourceLiftBase = id
    resourceFinally = finally'

instance ResourceUnsafeIO (ST s) where
    unsafeFromIO = unsafeIOToST

instance Resource (Lazy.ST s) where
    type Base (Lazy.ST s) = Lazy.ST s

    resourceLiftBase = id
    resourceFinally = finally'

instance ResourceUnsafeIO (Lazy.ST s) where
    unsafeFromIO = Lazy.unsafeIOToST

instance (MonadTransControl t, Resource m, Monad (t m)) => Resource (t m) where
    type Base (t m) = Base m

    resourceLiftBase = lift . resourceLiftBase
    resourceFinally a b =
        control' $ \run -> resourceFinally (run a) b
      where
        control' f = liftWith f >>= restoreT . return

instance (MonadTransControl t, ResourceUnsafeIO m, Monad (t m)) => ResourceUnsafeIO (t m) where
    unsafeFromIO = lift . unsafeFromIO

data ReleaseMap base = ReleaseMap !Int !(IntMap (base ()))
newtype ReleaseKey = ReleaseKey Int

newtype ResourceT m a = ResourceT (Ref (Base m) (ReleaseMap (Base m)) -> m a)

with :: Resource m
     => Base m a -- ^ allocate
     -> (a -> Base m ()) -- ^ free resource
     -> ResourceT m (ReleaseKey, a)
with acquire rel = ResourceT $ \istate -> resourceLiftBase $ mask $ \restore -> do
    a <- restore acquire
    key <- register' istate $ rel a
    return (key, a)

register :: Resource m
         => Base m ()
         -> ResourceT m ReleaseKey
register rel = ResourceT $ \istate -> resourceLiftBase $ register' istate rel

register' :: HasRef base
          => Ref base (ReleaseMap base)
          -> base ()
          -> base ReleaseKey
register' istate rel = modifyRef' istate $ \(ReleaseMap key m) ->
    ( ReleaseMap (key + 1) (IntMap.insert key rel m)
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
    lookupAction rm@(ReleaseMap next m) =
        case IntMap.lookup key m of
            Nothing -> (rm, Nothing)
            Just action ->
                ( ReleaseMap next $ IntMap.delete key m
                , Just action
                )

runResourceT :: Resource m => ResourceT m a -> m a
runResourceT (ResourceT r) = do
    istate <- resourceLiftBase $ newRef' $ ReleaseMap minBound IntMap.empty
    resourceFinally (r istate) (cleanup istate)
  where
    cleanup istate = mask_ $ do
        ReleaseMap _ m <- readRef' istate
        mapM_ (\x -> try x >> return ()) $ IntMap.elems m

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

{- Maybe it's a good thing this can't be implemented...
instance MonadTransControl ResourceT where
    newtype StT ResourceT a = StReader {unStReader :: a}
    liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> liftM StReader $ t r
    restoreT = ResourceT . const . liftM unStReader
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
     newtype StM (ResourceT m) a = StMT {unStMT :: ComposeSt ResourceT m a}
     liftBaseWith = defaultLiftBaseWith StMT
     restoreM     = defaultRestoreM   unStMT
-}

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

instance ResourceThrow IO where
    resourceThrow = E.throwIO

#define GO(T) instance (MonadBaseControl (Base m) m, ResourceThrow m) => ResourceThrow (T m) where resourceThrow = lift . resourceThrow
#define GOX(X, T) instance (MonadBaseControl (Base m) m, X, ResourceThrow m) => ResourceThrow (T m) where resourceThrow = lift . resourceThrow
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
