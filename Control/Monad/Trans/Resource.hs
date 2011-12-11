{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Trans.Resource
    ( -- * Data types
      ResourceT
    , ReleaseKey
      -- * Unwrap
    , runResourceT
      -- * Resource allocation
    , with
    , register
    , release
      -- * Internal
    , ResourceTT (..)
    ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Exception.Lifted (try, finally, SomeException, mask, mask_)
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

data ReleaseMap m = ReleaseMap !Int !(IntMap (ResourceT m ()))
newtype ReleaseKey = ReleaseKey Int

-- I'd rather just have a type synonym around ReaderT, but that makes so ugly
-- error messages.
newtype ResourceTT m' m a = ResourceTT (I.IORef (ReleaseMap m') -> m a)
type ResourceT m = ResourceTT m m

ask :: Monad m => ResourceT m (I.IORef (ReleaseMap m))
ask = ResourceTT return

with :: MonadBaseControl IO m
     => ResourceT m a -- ^ allocate
     -> (a -> ResourceT m ()) -- ^ free resource
     -> ResourceT m (ReleaseKey, a)
with acquire rel = mask_ $ do
    a <- acquire
    key <- register $ rel a
    return (key, a)

modify :: MonadBase IO m => (ReleaseMap m -> (ReleaseMap m, a)) -> ResourceT m a
modify f = do
    istate <- ask
    liftBase $ I.atomicModifyIORef istate $ \a -> f a

get :: MonadBase IO m => ResourceT m (ReleaseMap m)
get = ask >>= liftBase . I.readIORef

register :: MonadBaseControl IO m
         => ResourceT m ()
         -> ResourceT m ReleaseKey
register rel = mask_ $
    modify $ \(ReleaseMap key m) ->
        ( ReleaseMap (key + 1) (IntMap.insert key rel m)
        , ReleaseKey key
        )

release :: MonadBaseControl IO m
        => ReleaseKey
        -> ResourceT m ()
release (ReleaseKey key) = mask $ \restore -> do
    ReleaseMap _ m <- get
    case IntMap.lookup key m of
        Nothing -> return () -- maybe we should throw an exception?
        Just r -> modify (delete key) >> restore (try' r >> return ())
  where
    delete k (ReleaseMap next m) = (ReleaseMap next $ IntMap.delete k m, ())

runResourceT :: MonadBaseControl IO m
          => ResourceT m a
          -> m a
runResourceT r = do
    istate <- liftBase $ I.newIORef $ ReleaseMap minBound IntMap.empty
    let ResourceTT f = r `finally` cleanup
    f istate
  where
    cleanup = do
        ReleaseMap _ m <- get
        if IntMap.null m
            then return ()
            else do
                let (key, _) = IntMap.findMin m
                release $ ReleaseKey key
                cleanup

try' :: MonadBaseControl IO m
     => m a
     -> m (Either SomeException a)
try' = try

-------- All of our monad et al instances
instance Monad m => Functor (ResourceTT m' m) where
    fmap f (ResourceTT m) = ResourceTT $ \r -> liftM f (m r)

instance Monad m => Applicative (ResourceTT m' m) where
    pure = ResourceTT . const . return
    ResourceTT mf <*> ResourceTT ma = ResourceTT $ \r -> do
        f <- mf r
        a <- ma r
        return $ f a

instance Monad m => Monad (ResourceTT m' m) where
    return = pure
    ResourceTT ma >>= f =
        ResourceTT $ \r -> ma r >>= flip un r . f
      where
        un (ResourceTT x) = x

instance MonadTrans (ResourceTT m') where
    lift = ResourceTT . const

instance MonadIO m => MonadIO (ResourceTT m' m) where
    liftIO = lift . liftIO

instance MonadBase b m => MonadBase b (ResourceTT m' m) where
    liftBase = lift . liftBase

instance MonadTransControl (ResourceTT m') where
    newtype StT (ResourceTT m') a = StReader {unStReader :: a}
    liftWith f = ResourceTT $ \r -> f $ \(ResourceTT t) -> liftM StReader $ t r
    restoreT = ResourceTT . const . liftM unStReader
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ResourceTT m' m) where
     newtype StM (ResourceTT m' m) a = StMT {unStMT :: ComposeSt (ResourceTT m') m a}
     liftBaseWith = defaultLiftBaseWith StMT
     restoreM     = defaultRestoreM   unStMT
