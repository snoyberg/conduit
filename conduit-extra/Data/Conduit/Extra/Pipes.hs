{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Provides a convenience layer on top of conduit with functions and
--   operators similar to the pipes library.
module Data.Conduit.Extra.Pipes
    ( (>->), (<-<)
    , runPipe, runPipeR, runEffect
    , forP, each
    , take, peel
    , module X
    , module CL
    ) where

import Data.Conduit as X
import Data.Conduit.List as CL hiding (take)
import Data.Foldable
import Data.Void
import Prelude hiding (take)

-- | The conduit composition operator, ala pipes.  When combined with
--   'runPipe' (or 'runEffect', if you prefer), this is the only operator
--   needed.
(>->) :: forall a b i o m. Monad m
      => ConduitM i a m () -> ConduitM a o m b -> ConduitM i o m b
(>->) = (=$=)

(<-<) :: forall a b i o m. Monad m
      => ConduitM a o m b -> ConduitM i a m () -> ConduitM i o m b
(<-<) = flip (>->)

-- | Run a conduit.  This name may be preferable to the overly generic
--   'runEffect', which pipes uses.
runPipe :: forall m b. Monad m => ConduitM () Void m b -> m b
runPipe c = yield () $$ c

runEffect :: forall m b. Monad m => ConduitM () Void m b -> m b
runEffect = runPipe

-- | Like 'runPipe', except implies a call to 'runResourceT', for running
--   resource-sensitive pipelines.
runPipeR :: forall m b. (MonadBaseControl IO m, Monad m)
         => ConduitM () Void (ResourceT m) b -> m b
runPipeR = runResourceT . runPipe

-- | Iterate over all the elements from source, similar to 'forM' for a monad.
forP :: Monad m => Source m a -> (a -> m ()) -> m ()
forP p a = p $$ CL.mapM_ a

-- | Take N items from a conduit.  Synonym for Conduit's 'isolate'.
take :: Monad m => Int -> Conduit a m a
take = CL.isolate

-- | Peel off N items from a conduit and return them.  Synonym for Conduit's
--   'take'.
peel :: Monad m => Int -> m [()]
peel n = take n $$ CL.consume

-- | Call 'yield' for each element of the 'Foldable' data structure, resulting
--   in a 'Producer' over these elements.
--
-- >>> runPipe $ forP (each [1..3]) $ liftIO . print
-- 1
-- 2
-- 3
each :: (Monad m, Foldable f) => f a -> Producer m a
each = Data.Foldable.mapM_ yield
