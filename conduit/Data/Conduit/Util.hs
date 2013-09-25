-- | Various utility functions versions of @conduit@.
module Data.Conduit.Util
    ( -- * Misc
      zip
    , zipSinks
    ) where

import Prelude hiding (zip)
import Control.Monad (liftM, liftM2)
import Data.Conduit.Internal (Source, Sink, ConduitM (..), getCleanup, dropOutput)
import Data.Void (Void, absurd)

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 0.3.0
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip l0 r0 =
    go (getCleanup l0) (getCleanup r0) l0 r0
  where
    go _ _ (Done _ ()) (Done _ ()) = Done [] ()
    go _ cr (Done _ ()) (HaveOutput _ _) = dropOutput (cr [])
    go cl _ (HaveOutput _ _) (Done _ ()) = dropOutput (cl [])
    go _ cr (Done _ ()) (ConduitM _) = dropOutput (cr [])
    go cl _ (ConduitM _) (Done _ ()) = dropOutput (cl [])
    go cl cr (ConduitM mx) (ConduitM my) = ConduitM (liftM2 (go cl cr) mx my)
    go cl cr (ConduitM mx) y@HaveOutput{} = ConduitM (liftM (\x -> go cl cr x y) mx)
    go cl cr x@HaveOutput{} (ConduitM my) = ConduitM (liftM (go cl cr x) my)
    go cl cr (HaveOutput srcx x) (HaveOutput srcy y) = HaveOutput (go cl cr srcx srcy) (x, y)
    go cl cr (NeedInput _ c) right = go cl cr c right
    go cl cr left (NeedInput _ c) = go cl cr left c

-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- Any leftovers are discarded.
--
-- Since 0.4.1
zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
zipSinks x0 y0 =
    x0 >< y0
  where
    (><) :: Monad m => ConduitM i Void m r1 -> ConduitM i Void m r2 -> ConduitM i o m (r1, r2)

    HaveOutput _   o >< _                = absurd o
    _                >< HaveOutput _   o = absurd o

    ConduitM mx         >< y                = ConduitM (liftM (>< y) mx)
    x                >< ConduitM my         = ConduitM (liftM (x ><) my)
    Done _ x         >< Done _ y         = Done [] (x, y)
    NeedInput px cx  >< NeedInput py cy  = NeedInput (\i -> px i >< py i) (cx >< cy)
    NeedInput px cx  >< y@Done{}         = NeedInput (\i -> px i >< y)    (cx >< y)
    x@Done{}         >< NeedInput py cy  = NeedInput (\i -> x >< py i)    (x >< cy)
