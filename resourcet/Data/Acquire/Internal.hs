{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Acquire.Internal
    ( Acquire (..)
    , Allocated (..)
    , with
    , mkAcquire
    , ReleaseType (..)
    , mkAcquireType
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO, withRunInIO)
import qualified Control.Exception as E
import Data.Typeable (Typeable)
import Control.Monad (liftM, ap)
import qualified Control.Monad.Catch as C ()

-- | The way in which a release is called.
--
-- @since 1.1.2
data ReleaseType = ReleaseEarly
                 | ReleaseNormal
                 | ReleaseException
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

data Allocated a = Allocated !a !(ReleaseType -> IO ())

-- | A method for acquiring a scarce resource, providing the means of freeing
-- it when no longer needed. This data type provides
-- @Functor@\/@Applicative@\/@Monad@ instances for composing different resources
-- together. You can allocate these resources using either the @bracket@
-- pattern (via @with@) or using @ResourceT@ (via @allocateAcquire@).
--
-- This concept was originally introduced by Gabriel Gonzalez and described at:
-- <http://www.haskellforall.com/2013/06/the-resource-applicative.html>. The
-- implementation in this package is slightly different, due to taking a
-- different approach to async exception safety.
--
-- @since 1.1.0
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

-- | Create an @Acquire@ value using the given allocate and free functions.
--
-- @since 1.1.0
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
-- @since 1.1.2
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
-- @since 1.1.0
with :: MonadUnliftIO m
     => Acquire a
     -> (a -> m b)
     -> m b
with (Acquire f) g = withRunInIO $ \run -> E.mask $ \restore -> do
    Allocated x free <- f restore
    res <- restore (run (g x)) `E.onException` free ReleaseException
    free ReleaseNormal
    return res
