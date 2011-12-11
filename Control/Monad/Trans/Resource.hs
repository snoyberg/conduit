{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Trans.Resource
    ( -- * Data types
      ResourceT (..)
    , ReleaseKey
      -- * Unwrap
    , runResourceT
      -- * Resource allocation
    , with
    , register
    , release
      -- * Monad transformation
    , transResourceT
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

data ReleaseMap = ReleaseMap !Int !(IntMap (IO ()))
newtype ReleaseKey = ReleaseKey Int

-- I'd rather just have a type synonym around ReaderT, but that makes so ugly
-- error messages.
newtype ResourceT m a = ResourceT (I.IORef ReleaseMap -> m a)

ask :: Monad m => ResourceT m (I.IORef ReleaseMap)
ask = ResourceT return

with :: MonadBaseControl IO m
     => IO a -- ^ allocate
     -> (a -> IO ()) -- ^ free resource
     -> ResourceT m (ReleaseKey, a)
with acquire rel = mask_ $ do
    a <- liftBase acquire
    key <- register $ rel a
    return (key, a)

modify :: MonadBase IO m => (ReleaseMap -> (ReleaseMap, a)) -> ResourceT m a
modify f = do
    istate <- ask
    liftBase $ I.atomicModifyIORef istate $ \a -> f a

get :: MonadBase IO m => ResourceT m ReleaseMap
get = ask >>= liftBase . I.readIORef

register :: MonadBaseControl IO m
         => IO ()
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
        Just r -> do
            modify (delete key)
            restore (liftBase (try' r) >> return ())
  where
    delete k (ReleaseMap next m) = (ReleaseMap next $ IntMap.delete k m, ())

runResourceT :: MonadBaseControl IO m
          => ResourceT m a
          -> m a
runResourceT r = do
    istate <- liftBase $ I.newIORef $ ReleaseMap minBound IntMap.empty
    let ResourceT f = r `finally` cleanup
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

transResourceT :: (m a -> n a)
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
