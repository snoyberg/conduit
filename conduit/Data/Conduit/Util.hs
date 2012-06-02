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
    {- FIXME
      -- * Misc
    , zip
    , zipSinks
    -}
    ) where

import Data.Conduit.Util.Source
import Data.Conduit.Util.Sink
import Data.Conduit.Util.Conduit

{- FIXME
-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 0.3.0
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip (Leftover p i) right = zip (pipePushStrip i p) right
zip left (Leftover p i)  = zip left (pipePushStrip i p)
zip (Done ()) (Done ()) = Done ()
zip (Done ()) (HaveOutput _ close _) = PipeM (close >> return (Done ()))
zip (HaveOutput _ close _) (Done ()) = PipeM (close >> return (Done ()))
zip (Done ()) (PipeM _) = Done ()
zip (PipeM _) (Done ()) = Done ()
zip (PipeM mx closex) (PipeM my closey) = PipeM (liftM2 zip mx my) (closex >> closey)
zip (PipeM mx closex) y@(HaveOutput _ closey _) = PipeM (liftM (\x -> zip x y) mx) (closex >> closey)
zip x@(HaveOutput _ closex _) (PipeM my closey) = PipeM (liftM (\y -> zip x y) my) (closex >> closey)
zip (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (zip srcx srcy) (closex >> closey) (x, y)
zip (NeedInput _ c) right = zip c right
zip left (NeedInput _ c) = zip left c


-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- If both sinks finish on the same chunk, and both report leftover input,
-- arbitrarily yield the left sink's leftover input.
--
-- Since 0.4.1
zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
zipSinks = (><)
  where
    (><) :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
    Leftover px i    >< py               = pipePushStrip i px >< py
    px               >< Leftover py i    = px >< pipePushStrip i py
    PipeM mpx mx     >< py               = PipeM (liftM (>< py) mpx) (liftM2 (,) mx (pipeClose py))
    px               >< PipeM mpy my     = PipeM (liftM (px ><) mpy) (liftM2 (,) (pipeClose px) my)

    Done x           >< Done y           = Done (x, y)

    NeedInput fpx px >< NeedInput fpy py = NeedInput (\i -> zipSinks (fpx i) (fpy i)) (px >< py)
    NeedInput fpx px >< py               = NeedInput (\i -> zipSinks (fpx i) py)      (px >< noInput py)
    px               >< NeedInput fpy py = NeedInput (\i -> zipSinks px (fpy i))      (noInput px >< py)

    HaveOutput _ _ o >< _                = absurd o
    _                >< HaveOutput _ _ o = absurd o
-}
