-- | Various utility functions versions of @conduit@.
module Data.Conduit.Util
    ( -- * Misc
      zip
    , zipSources
    , zipSinks
    , passthroughSink
    ) where

import Prelude hiding (zip)
import Control.Monad (liftM, liftM2)
import Data.Conduit.Internal (Pipe (..), Source, Sink, ConduitM (..), Conduit, awaitForever, yield, await, zipSinks)
import Data.Void (absurd)
import Control.Monad.Trans.Class (lift)

-- | Deprecated synonym for 'zipSources'.
--
-- Since 0.3.0
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip = zipSources
{-# DEPRECATED zip "Use zipSources instead" #-}

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 1.0.13
zipSources :: Monad m => Source m a -> Source m b -> Source m (a, b)
zipSources (ConduitM left0) (ConduitM right0) =
    ConduitM $ go left0 right0
  where
    go (Leftover left ()) right = go left right
    go left (Leftover right ())  = go left right
    go (Done ()) (Done ()) = Done ()
    go (Done ()) (HaveOutput _ close _) = PipeM (close >> return (Done ()))
    go (HaveOutput _ close _) (Done ()) = PipeM (close >> return (Done ()))
    go (Done ()) (PipeM _) = Done ()
    go (PipeM _) (Done ()) = Done ()
    go (PipeM mx) (PipeM my) = PipeM (liftM2 go mx my)
    go (PipeM mx) y@HaveOutput{} = PipeM (liftM (\x -> go x y) mx)
    go x@HaveOutput{} (PipeM my) = PipeM (liftM (go x) my)
    go (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (go srcx srcy) (closex >> closey) (x, y)
    go (NeedInput _ c) right = go (c ()) right
    go left (NeedInput _ c) = go left (c ())

-- | Turn a @Sink@ into a @Conduit@ in the following way:
--
-- * All input passed to the @Sink@ is yielded downstream.
--
-- * When the @Sink@ finishes processing, the result is passed to the provided to the finalizer function.
--
-- Note that the @Sink@ will stop receiving input as soon as the downstream it
-- is connected to shuts down.
--
-- An example usage would be to write the result of a @Sink@ to some mutable
-- variable while allowing other processing to continue.
--
-- Since 1.0.10
passthroughSink :: Monad m
                => Sink i m r
                -> (r -> m ()) -- ^ finalizer
                -> Conduit i m i
passthroughSink (ConduitM sink0) final =
    ConduitM $ go [] sink0
  where
    go _ (Done r) = do
        lift $ final r
        awaitForever yield
    go is (Leftover sink i) = go (i:is) sink
    go _ (HaveOutput _ _ o) = absurd o
    go is (PipeM mx) = do
        x <- lift mx
        go is x
    go (i:is) (NeedInput next _) = go is (next i)
    go [] (NeedInput next done) = do
        mx <- await
        case mx of
            Nothing -> go [] (done ())
            Just x -> do
                yield x
                go [] (next x)
