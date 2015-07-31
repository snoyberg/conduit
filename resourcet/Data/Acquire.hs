{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
-- | This was previously known as the Resource monad. However, that term is
-- confusing next to the ResourceT transformer, so it has been renamed.
module Data.Acquire
    ( Acquire
    , with
    , withEx
    , mkAcquire
    , mkAcquireType
    , allocateAcquire
    , ReleaseType (..)
    ) where

import Control.Monad.Trans.Resource.Internal
import Control.Monad.Trans.Resource
import Data.Acquire.Internal
import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception.Lifted as E
import Data.Typeable (Typeable)
import Control.Monad (liftM, ap)

-- | Allocate a resource and register an action with the @MonadResource@ to
-- free the resource.
--
-- Since 1.1.0
allocateAcquire :: MonadResource m => Acquire a -> m (ReleaseKey, a)
allocateAcquire = liftResourceT . allocateAcquireRIO

allocateAcquireRIO :: Acquire a -> ResourceT IO (ReleaseKey, a)
allocateAcquireRIO (Acquire f) = ResourceT $ \istate -> liftIO $ E.mask $ \restore -> do
    Allocated a free <- f restore
    key <- registerType istate free
    return (key, a)
