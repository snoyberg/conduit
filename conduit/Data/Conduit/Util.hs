-- | Utility functions from older versions of @conduit@. These should be
-- considered deprecated, as there are now easier ways to handle their use
-- cases. This module is provided solely for backwards compatibility.
module Data.Conduit.Util
    ( -- * Source
      module Data.Conduit.Util.Source
      -- * Sink
    , module Data.Conduit.Util.Sink
      -- * Conduit
    , module Data.Conduit.Util.Conduit
      -- * Misc
    , zip
    , zipSinks
    ) where

import Prelude hiding (zip)
import Control.Monad (liftM, liftM2)
import Data.Conduit.Internal (Pipe (..), Source, Sink, injectLeftovers)
import Data.Void (Void, absurd)
import Data.Conduit.Util.Source
import Data.Conduit.Util.Sink
import Data.Conduit.Util.Conduit

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 0.3.0
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip (Leftover left ()) right = zip left right
zip left (Leftover right ())  = zip left right
zip (Done ()) (Done ()) = Done ()
zip (Done ()) (HaveOutput _ close _) = PipeM (close >> return (Done ()))
zip (HaveOutput _ close _) (Done ()) = PipeM (close >> return (Done ()))
zip (Done ()) (PipeM _) = Done ()
zip (PipeM _) (Done ()) = Done ()
zip (PipeM mx) (PipeM my) = PipeM (liftM2 zip mx my)
zip (PipeM mx) y@HaveOutput{} = PipeM (liftM (\x -> zip x y) mx)
zip x@HaveOutput{} (PipeM my) = PipeM (liftM (zip x) my)
zip (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (zip srcx srcy) (closex >> closey) (x, y)
zip (NeedInput _ c) right = zip (c ()) right
zip left (NeedInput _ c) = zip left (c ())

-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- Any leftovers are discarded.
--
-- Since 0.4.1
zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
zipSinks x0 y0 =
    injectLeftovers x0 >< injectLeftovers y0
  where
    (><) :: Monad m => Pipe Void i Void () m r1 -> Pipe Void i Void () m r2 -> Sink i m (r1, r2)

    Leftover _  i    >< _                = absurd i
    _                >< Leftover _  i    = absurd i
    HaveOutput _ _ o >< _                = absurd o
    _                >< HaveOutput _ _ o = absurd o

    PipeM mx         >< y                = PipeM (liftM (>< y) mx)
    x                >< PipeM my         = PipeM (liftM (x ><) my)
    Done x           >< Done y           = Done (x, y)
    NeedInput px cx  >< NeedInput py cy  = NeedInput (\i -> px i >< py i) (\() -> cx () >< cy ())
    NeedInput px cx  >< y@Done{}         = NeedInput (\i -> px i >< y)    (\u -> cx u >< y)
    x@Done{}         >< NeedInput py cy  = NeedInput (\i -> x >< py i)    (\u -> x >< cy u)
