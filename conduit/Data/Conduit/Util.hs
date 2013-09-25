-- | Various utility functions versions of @conduit@.
module Data.Conduit.Util
    ( -- * Misc
      zip
    , zipSinks
    ) where

import Prelude hiding (zip)
import Control.Monad (liftM, liftM2)
import Data.Conduit.Internal (Source, Sink, injectLeftovers, ConduitM (..))
import Data.Void (Void, absurd)

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 0.3.0
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip =
    go
  where
    go (Leftover left ()) right = go left right
    go left (Leftover right ())  = go left right
    go (Done ()) (Done ()) = Done ()
    go (Done ()) (HaveOutput _ close _) = ConduitM (close >> return (Done ()))
    go (HaveOutput _ close _) (Done ()) = ConduitM (close >> return (Done ()))
    go (Done ()) (ConduitM _) = Done ()
    go (ConduitM _) (Done ()) = Done ()
    go (ConduitM mx) (ConduitM my) = ConduitM (liftM2 go mx my)
    go (ConduitM mx) y@HaveOutput{} = ConduitM (liftM (\x -> go x y) mx)
    go x@HaveOutput{} (ConduitM my) = ConduitM (liftM (go x) my)
    go (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (go srcx srcy) (closex >> closey) (x, y)
    go (NeedInput _ c) right = go c right
    go left (NeedInput _ c) = go left c

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
    (><) :: Monad m => ConduitM i Void m r1 -> ConduitM i Void m r2 -> ConduitM i o m (r1, r2)

    Leftover _  i    >< _                = error "zipSinks1: FIXME"
    _                >< Leftover _  i    = error "zipSinks2: FIXME"
    HaveOutput _ _ o >< _                = absurd o
    _                >< HaveOutput _ _ o = absurd o

    ConduitM mx         >< y                = ConduitM (liftM (>< y) mx)
    x                >< ConduitM my         = ConduitM (liftM (x ><) my)
    Done x           >< Done y           = Done (x, y)
    NeedInput px cx  >< NeedInput py cy  = NeedInput (\i -> px i >< py i) (cx >< cy)
    NeedInput px cx  >< y@Done{}         = NeedInput (\i -> px i >< y)    (cx >< y)
    x@Done{}         >< NeedInput py cy  = NeedInput (\i -> x >< py i)    (x >< cy)
