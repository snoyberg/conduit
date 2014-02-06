module Data.Conduit.Extra
    ( module Data.Conduit.Extra.ZipSink
    , module Data.Conduit.Extra.Resumable
    , fuseLeftovers
    , fuseReturnLeftovers
    ) where

import Data.Conduit.Extra.ZipSink
import Data.Conduit.Extra.Resumable
import Data.Conduit
import Data.Conduit.Internal (Pipe (..), ConduitM (..))
import Control.Monad (liftM)
import Data.Void (absurd)

fuseLeftovers :: Monad m
              => ConduitM a b m ()
              -> ConduitM b c m r
              -> ConduitM a c m (r, [b])
fuseLeftovers (ConduitM left0) (ConduitM right0) =
    ConduitM $ goRight (return ()) [] left0 right0
  where
    goRight final bs left right =
        case right of
            HaveOutput p c o -> HaveOutput (recurse p) (c >> final) o
            NeedInput rp rc  ->
                case bs of
                    [] -> goLeft rp rc final left
                    b:bs' -> goRight final bs' left (rp b)
            Done r2          -> PipeM (final >> return (Done (r2, bs)))
            PipeM mp         -> PipeM (liftM recurse mp)
            Leftover p b     -> goRight final (b:bs) left p
      where
        recurse = goRight final bs left

    goLeft rp rc final left =
        case left of
            HaveOutput left' final' o -> goRight final' [] left' (rp o)
            NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
            Done r1                   -> goRight (return ()) [] (Done r1) (rc r1)
            PipeM mp                  -> PipeM (liftM recurse mp)
            Leftover left' i          -> Leftover (recurse left') i
      where
        recurse = goLeft rp rc final

fuseReturnLeftovers
    :: Monad m
    => ([b] -> [a])
    -> ConduitM a b m ()
    -> ConduitM b c m r
    -> ConduitM a c m r
fuseReturnLeftovers f left right = do
    (r, bs) <- fuseLeftovers left right
    mapM_ leftover $ reverse $ f bs
    return r