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
    , Ref
      -- * Unwrap
    , runResourceT
      -- * Resource allocation
    , with
    , register
    , release
      -- * Monad transformation
    , transResourceT
      -- * Other
    , throwBase
    ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Exception (SomeException)
import Control.Monad.Trans.Control
    ( MonadTransControl (..), MonadBaseControl (..)
    , control
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

class Monad m => HasRef m where
    type Ref' m :: * -> *
    newRef' :: a -> m (Ref' m a)
    modifyRef' :: Ref' m a -> (a -> (a, b)) -> m b
    readRef' :: Ref' m a -> m a
    writeRef' :: Ref' m a -> a -> m ()
    mask :: ((forall a. m a -> m a) -> m b) -> m b
    mask_ :: m a -> m a
    finally' :: m a -> m b -> m a
    try :: m a -> m (Either SomeException a)
    throwBase :: E.Exception e => e -> m a

instance HasRef IO where
    type Ref' IO = I.IORef
    newRef' = I.newIORef
    modifyRef' = I.atomicModifyIORef
    readRef' = I.readIORef
    writeRef' = I.writeIORef
    mask = E.mask
    mask_ = E.mask_
    finally' = E.finally
    try = E.try
    throwBase = E.throwIO

instance HasRef (ST s) where
    type Ref' (ST s) = S.STRef s
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
    throwBase = return . E.throw

instance HasRef (Lazy.ST s) where
    type Ref' (Lazy.ST s) = SL.STRef s
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
    throwBase = return . E.throw

type family Ref (m :: * -> *) :: * -> *
type instance Ref m = Ref' (Base m)

class (Monad m, MonadBase (Base m) m, HasRef (Base m))
        => Resource m where
    type Base m :: * -> *

    newRef :: a -> ResourceT m (Ref m a)
    readRef :: Ref m a -> ResourceT m a
    writeRef :: Ref m a -> a -> ResourceT m ()
    modifyRef :: Ref m a -> (a -> (a, b)) -> ResourceT m b
    resourceThrow :: E.Exception e => e -> ResourceT m a
    resourceFinally :: m a -> Base m b -> m a

-- | A 'Resource' based on some monad which allows running of some 'IO'
-- actions, via unsafe calls. This applies to 'IO' and 'ST', for instance.
class Resource m => ResourceUnsafeIO m where
    unsafeFromIO :: IO a -> m a

instance Resource IO where
    type Base IO = IO

    newRef = lift . newRef'
    readRef = lift . readRef'
    writeRef r = lift . writeRef' r
    modifyRef r = lift . modifyRef' r
    resourceThrow = lift . throwBase
    resourceFinally = finally'

instance ResourceUnsafeIO IO where
    unsafeFromIO = id

instance Resource (ST s) where
    type Base (ST s) = ST s

    newRef = lift . newRef'
    readRef = lift . readRef'
    writeRef r = lift . writeRef' r
    modifyRef r = lift . modifyRef' r
    resourceThrow = lift . throwBase
    resourceFinally = finally'

instance ResourceUnsafeIO (ST s) where
    unsafeFromIO = unsafeIOToST

instance Resource (Lazy.ST s) where
    type Base (Lazy.ST s) = Lazy.ST s

    newRef = lift . newRef'
    readRef = lift . readRef'
    writeRef r = lift . writeRef' r
    modifyRef r = lift . modifyRef' r
    resourceThrow = lift . throwBase
    resourceFinally = finally'

instance ResourceUnsafeIO (Lazy.ST s) where
    unsafeFromIO = Lazy.unsafeIOToST

instance (MonadTransControl t, MonadBaseControl (Base m) (t m), Resource m) => Resource (t m) where
    type Base (t m) = Base m

    newRef = liftBase . newRef'
    readRef = liftBase . readRef'
    writeRef r = liftBase . writeRef' r
    modifyRef r = liftBase . modifyRef' r
    resourceThrow = liftBase . throwBase
    resourceFinally a b = control $ \run -> finally' (run a) (run $ liftBase b)

instance (MonadTransControl t, MonadBaseControl (Base m) (t m), ResourceUnsafeIO m) => ResourceUnsafeIO (t m) where
    unsafeFromIO = lift . unsafeFromIO

data ReleaseMap base = ReleaseMap !Int !(IntMap (base ()))
newtype ReleaseKey = ReleaseKey Int

-- I'd rather just have a type synonym around ReaderT, but that makes so ugly
-- error messages.
newtype ResourceT m a = ResourceT (Ref' (Base m) (ReleaseMap (Base m)) -> m a)

with :: Resource m
     => Base m a -- ^ allocate
     -> (a -> Base m ()) -- ^ free resource
     -> ResourceT m (ReleaseKey, a)
with acquire rel = ResourceT $ \istate -> liftBase $ mask $ \restore -> do
    a <- restore acquire
    key <- register' istate $ rel a
    return (key, a)

register :: Resource m
         => Base m ()
         -> ResourceT m ReleaseKey
register rel = ResourceT $ \istate -> liftBase $ register' istate rel

register' :: HasRef base
          => Ref' base (ReleaseMap base)
          -> base ()
          -> base ReleaseKey
register' istate rel = modifyRef' istate $ \(ReleaseMap key m) ->
    ( ReleaseMap (key + 1) (IntMap.insert key rel m)
    , ReleaseKey key
    )

release :: Resource m
        => ReleaseKey
        -> ResourceT m ()
release rk = ResourceT $ \istate -> liftBase $ release' istate rk

release' :: HasRef base
         => Ref' base (ReleaseMap base)
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
    istate <- liftBase $ newRef' $ ReleaseMap minBound IntMap.empty
    resourceFinally (r istate) (cleanup istate)
  where
    cleanup istate = mask_ $ do
        ReleaseMap _ m <- readRef' istate
        mapM_ (\x -> try x >> return ()) $ IntMap.elems m
    {-
    finally' a sequel = control $ \runInBase ->
                            finally (runInBase a) (runInBase sequel)
                                -}

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
