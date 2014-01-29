-- | Various utility functions versions of @conduit@.
module Data.Conduit.Util
    ( -- * Misc
      zip
    , zipSources
    , zipSinks
    , passthroughSink
    ) where

import Prelude hiding (zip)
import Data.Conduit.Internal (Pipe (..), Source, Sink, ConduitM (..), Conduit, awaitForever, yield, await, zipSinks, zipSources)
import Data.Void (absurd)
import Control.Monad.Trans.Class (lift)

-- | Deprecated synonym for 'zipSources'.
--
-- Since 0.3.0
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip = zipSources
{-# DEPRECATED zip "Use zipSources instead" #-}

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
