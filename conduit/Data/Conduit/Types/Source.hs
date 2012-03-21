{-# LANGUAGE FlexibleContexts #-}
-- | Defines the types for a source, which is a producer of data.
module Data.Conduit.Types.Source
    ( Source (..)
    ) where

import Data.Monoid (Monoid (..))
import Control.Monad (liftM)

-- | A @Source@ has two operations on it: pull some data, and close the
-- @Source@. A @Source@ should free any resources it allocated when either it
-- returns @Closed@ or when it is explicitly closed (the second record on
-- either the @Open@ or @SourceM@ constructors).
--
-- Since 0.3.0
data Source m a =
    Open (Source m a) (m ()) a -- ^ A @Source@ providing more data. Provides records for the next @Source@ in the stream, a close action, and the data provided.
  | Closed -- ^ A @Source@ which has no more data available.
  | SourceM (m (Source m a)) (m ()) -- ^ Requires a monadic action to retrieve the next @Source@ in the stream. Second record allows you to close the @Source@.

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
