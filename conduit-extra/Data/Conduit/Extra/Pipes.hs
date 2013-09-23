{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Provides a convenience layer on top of conduit with functions and
--   operators similar to the pipes library.
module Data.Conduit.Extra.Pipes
    ( (>->), (<-<)
    , runPipe, runPipeR, runEffect
    , forP, each
    , take, peel
    , replicateM
    , tee
    , module X
    , module CL
    ) where

import Control.Monad.Trans.Class
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

-- | Replicate a monadic action a given number of times via a producer.
replicateM :: Monad m => Int -> m a -> Producer m a
replicateM 0 _ = return ()
replicateM n m = lift m >>= yield >> replicateM (n-1) m

-- | Injects a sink within a pipeline which receives a copy of every input
--   argument, similar to the Unix command of the same name.
--
-- >>> runPipe $ each [1..10] >-> tee (P.mapM_ f) >-> P.mapM_ f
tee :: Monad m => Sink a (ConduitM a a m) b -> ConduitM a a m b
tee c = go $$ c
  where
    go = do
        x <- lift await
        case x of
            Nothing -> return ()
            Just x' -> yield x' >> lift (yield x') >> go
