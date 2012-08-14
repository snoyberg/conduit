{-# LANGUAGE RankNTypes #-}
module Data.Conduit.NonEmpty where

import           Control.Monad.Trans.Class
import           Data.Conduit
import           Data.Conduit.Internal
import qualified Data.Conduit.List as CL
import           Data.Void

{-
newtype NonEmptySource m o = NonEmptySource (m (o, Source m o))

meld :: Monad m => NonEmptySource m o -> Source m o
meld (NonEmptySource action) = do (o, src) <- lift action
                                  yield o
                                  src

fold1 :: Monad m => (b -> a -> b) -> (a -> b) -> NonEmptySource m a -> m b
fold1 f i (NonEmptySource action) = do (o, src) <- action
                                       src $$ CL.fold f (i o)
-}

-- | Pipes with non-empty output, by construction.
--
-- Cannot take input before returning first output element, because that would be unsafe.
-- Throwing an exception before the first element is allowed.
newtype NonEmptyPipe l i o u m r = NonEmptyPipe (m (o, Pipe l i o u m r))

-- | Specialized type for non-empty sources.
type NonEmptySource m o = NonEmptyPipe Void () o () m ()

-- | Specialized type for non-empty sources (generic variant).
type GNonEmptySource m o = forall l i u. NonEmptyPipe l i o u m ()

-- | Create a pure non-empty pipe from a non-empty list.
nonEmptySourceList :: Monad m => [a] -> NonEmptyPipe l i a u m ()
nonEmptySourceList [] = error "nonEmptySourceList only accepts non-empty lists"
nonEmptySourceList (x:xs) = NonEmptyPipe $ return (x, sourceList xs)

-- | Convert a non-empty pipe to a normal pipe.
toPipe :: Monad m => NonEmptyPipe l i o u m r -> Pipe l i o u m r
toPipe (NonEmptyPipe action) = do (o, src) <- lift action
                                  yield o
                                  src

-- | Fold the output of a non-empty pipe (strict left fold).
--
-- @fold1 f i x@ converts the first element of the non-empty pipe to the output type with @i@,
-- and then folds up the rest of the pipe with @f@.
fold1 :: Monad m => (b -> a -> b) -> (a -> b) -> NonEmptyPipe l i a u m r -> Pipe l i c u m b
fold1 f i (NonEmptyPipe action) = do (o, src) <- lift action
                                     src `pipe` CL.fold f (i o)

-- | Fold the output of a non-empty source (strict left fold).
--
-- @fold1 f i x@ converts the first element of the non-empty source to the output type with @i@,
-- and then folds up the rest of the source with @f@.
fold1' :: Monad m => (b -> a -> b) -> (a -> b) -> NonEmptySource m a -> m b
fold1' f i x = runPipe (fold1 f i x)
