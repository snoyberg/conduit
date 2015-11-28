{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
module Data.Acquire.Internal
    ( Acquire (..)
    , Allocated (..)
    , with
    , withEx
    , mkAcquire
    , ReleaseType (..)
    , mkAcquireType
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception.Lifted as E
import Data.Typeable (Typeable)
import Control.Monad (liftM, ap)
import qualified Control.Monad.Catch as C
import GHC.IO (unsafeUnmask)

-- | The way in which a release is called.
--
-- Since 1.1.2
data ReleaseType = ReleaseEarly
                 | ReleaseNormal
                 | ReleaseException
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

data Allocated a = Allocated !a !(ReleaseType -> IO ())

-- | A method for acquiring a scarce resource, providing the means of freeing
-- it when no longer needed. This data type provides
-- @Functor@/@Applicative@/@Monad@ instances for composing different resources
-- together. You can allocate these resources using either the @bracket@
-- pattern (via @with@) or using @ResourceT@ (via @allocateAcquire@).
--
-- This concept was originally introduced by Gabriel Gonzalez and described at:
-- <http://www.haskellforall.com/2013/06/the-resource-applicative.html>. The
-- implementation in this package is slightly different, due to taking a
-- different approach to async exception safety.
--
-- Since 1.1.0
newtype Acquire a = Acquire ((forall b. IO b -> IO b) -> IO (Allocated a))
    deriving Typeable

instance Functor Acquire where
    fmap = liftM
instance Applicative Acquire where
    pure a = Acquire (\_ -> return (Allocated a (const $ return ())))
    (<*>) = ap

instance Monad Acquire where
    return = pure
    Acquire f >>= g' = Acquire $ \restore -> do
        Allocated x free1 <- f restore
        let Acquire g = g' x
        Allocated y free2 <- g restore `E.onException` free1 ReleaseException
        return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)

instance MonadIO Acquire where
    liftIO f = Acquire $ \restore -> do
        x <- restore f
        return $! Allocated x (const $ return ())

instance MonadBase IO Acquire where
    liftBase = liftIO

-- | Create an @Acquire@ value using the given allocate and free functions.
--
-- Since 1.1.0
mkAcquire :: IO a -- ^ acquire the resource
          -> (a -> IO ()) -- ^ free the resource
          -> Acquire a
mkAcquire create free = Acquire $ \restore -> do
    x <- restore create
    return $! Allocated x (const $ free x)

-- | Same as 'mkAcquire', but the cleanup function will be informed of /how/
-- cleanup was initiated. This allows you to distinguish, for example, between
-- normal and exceptional exits.
--
-- Since 1.1.2
mkAcquireType
    :: IO a -- ^ acquire the resource
    -> (a -> ReleaseType -> IO ()) -- ^ free the resource
    -> Acquire a
mkAcquireType create free = Acquire $ \restore -> do
    x <- restore create
    return $! Allocated x (free x)

-- | Allocate the given resource and provide it to the provided function. The
-- resource will be freed as soon as the inner block is exited, whether
-- normally or via an exception. This function is similar in function to
-- @bracket@.
--
-- Since 1.1.0
with :: MonadBaseControl IO m
     => Acquire a
     -> (a -> m b)
     -> m b
with (Acquire f) g = control $ \run -> E.mask $ \restore -> do
    Allocated x free <- f restore
    res <- restore (run (g x)) `E.onException` free ReleaseException
    free ReleaseNormal
    return res

-- | Same as @with@, but uses the @MonadMask@ typeclass from exceptions instead
-- of @MonadBaseControl@ from exceptions.
--
-- Since 1.1.3
#if MIN_VERSION_exceptions(0,6,0)
withEx :: (C.MonadMask m, MonadIO m)
#else
withEx :: (C.MonadCatch m, MonadIO m)
#endif
       => Acquire a
       -> (a -> m b)
       -> m b
withEx (Acquire f) g = do
    -- We need to do some funny business, since the restore we get below is
    -- specialized to the m from the result, whereas we need a restore function
    -- in IO. Checking the current masking state is exactly how mask is
    -- implemented in base.
    origMS <- liftIO E.getMaskingState

    C.mask $ \restore -> do
        Allocated x free <- liftIO $ f $ case origMS of
            E.Unmasked -> unsafeUnmask
            _ -> id
        res <- restore (g x) `C.onException` liftIO (free ReleaseException)
        liftIO $ free ReleaseNormal
        return res
