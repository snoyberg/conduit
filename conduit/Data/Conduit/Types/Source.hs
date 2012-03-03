{-# LANGUAGE FlexibleContexts #-}
-- | Defines the types for a source, which is a producer of data.
module Data.Conduit.Types.Source
    ( Source (..)
    ) where

import Data.Monoid (Monoid (..))
import Control.Monad (liftM)

-- | A @Source@ has two operations on it: pull some data, and close the
-- @Source@. Since @Source@ is built on top of 'ResourceT', all acquired
-- resources should be automatically released anyway. Closing a @Source@ early
-- is merely an optimization to free scarce resources as soon as possible.
--
-- A @Source@ is should free any resources it allocated when either
-- @sourceClose@ is called or a @Closed@ is returned. However, based on the
-- usage of @ResourceT@, this is simply an optimization.
--
-- Since 0.2.0
data Source m a =
    Open (Source m a) (m ()) a
  | Closed
  | SourceM (m (Source m a)) (m ())

instance Monad m => Functor (Source m) where
    fmap f (Open next close a) = Open (fmap f next) close (f a)
    fmap _ Closed = Closed
    fmap f (SourceM msrc close) = SourceM (liftM (fmap f) msrc) close

instance Monad m => Monoid (Source m a) where
    mempty = Closed
    mappend x Closed = x
    mappend Closed y = y
    mappend (Open next close a) y = Open (mappend next y) close a
    mappend (SourceM msrc close) y = SourceM (do
        src <- msrc
        return (mappend src y)) close
