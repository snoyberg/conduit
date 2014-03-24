{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Acquire.Internal
    ( Acquire (..)
    , Allocated (..)
    , with
    , mkAcquire
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception.Lifted as E
import Data.Typeable (Typeable)
import Control.Monad (liftM, ap)

data Allocated a = Allocated !a !(IO ())

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
    pure = return
    (<*>) = ap

instance Monad Acquire where
    return a = Acquire (\_ -> return (Allocated a (return ())))
    Acquire f >>= g' = Acquire $ \restore -> do
        Allocated x free1 <- f restore
        let Acquire g = g' x
        Allocated y free2 <- g restore `E.onException` free1
        return $! Allocated y (free2 `E.finally` free1)

instance MonadIO Acquire where
    liftIO f = Acquire $ \restore -> do
        x <- restore f
        return $! Allocated x (return ())

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
    run (g x) `E.finally` free
