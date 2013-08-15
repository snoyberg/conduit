module Data.Conduit.Extra.Resumable
    ( ResumableConduit(..)
    , connectResume
    , (=$$+)
    , (=$$++)
    , (=$$+-)
    ) where

import Control.Monad
import Control.Monad.Trans (lift)
import Data.Conduit
import Data.Conduit.Internal (ConduitM(..), Pipe(..))
import Data.Void (absurd)

-- | A generalization of 'ResumableSource'. Allows to resume an arbitrary
-- conduit, keeping its state and using it later (or finalizing it).
data ResumableConduit i m o =
    ResumableConduit (Conduit i m o) (m ())


-- | Connect a 'ResumableConduit' to a sink and return the output of the sink
-- together with a new 'ResumableConduit'.
connectResume :: Monad m
              => ResumableConduit i m o
              -> Sink o m r
              -> Sink i m (ResumableConduit i m o, r)
connectResume (ResumableConduit (ConduitM left0) leftFinal0) (ConduitM right0) =
    ConduitM $ goRight leftFinal0 left0 right0
  where
    goRight leftFinal left right =
        case right of
            HaveOutput _ _ o -> absurd o
            NeedInput rp rc -> goLeft rp rc leftFinal left
            Done r2 -> Done (ResumableConduit (ConduitM left) leftFinal, r2)
            PipeM mp -> PipeM (liftM (goRight leftFinal left) mp)
            Leftover p i -> goRight leftFinal (HaveOutput left leftFinal i) p

    goLeft rp rc leftFinal left =
        case left of
            HaveOutput left' leftFinal' o -> goRight leftFinal' left' (rp o)
            NeedInput left' lc -> NeedInput (recurse . left') (recurse . lc)
            Done () -> goRight (return ()) (Done ()) (rc ())
            PipeM mp -> PipeM (liftM recurse mp)
            Leftover left' i -> Leftover (recurse left') i -- recurse p
      where
        recurse = goLeft rp rc leftFinal

-- | The connect-and-resume operator. This does not close the @Conduit@, but
-- instead returns it to be used again. This allows a @Conduit@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Leftover data returned from the @Sink@ will be discarded.
--
-- Mnemonic: connect + do more.
(=$$+) :: Monad m => Conduit a m b -> Sink b m r -> Sink a m (ResumableConduit a m b, r)
(=$$+) conduit = connectResume (ResumableConduit conduit (return ()))
{-# INLINE (=$$+) #-}

-- | Continue processing after usage of '=$$+'. An alias for 'connectResume'.
(=$$++) :: Monad m => ResumableConduit i m o -> Sink o m r -> Sink i m (ResumableConduit i m o, r)
(=$$++) = connectResume
{-# INLINE (=$$++) #-} 

-- | Complete processing of a 'ResumableConduit'. This will run the finalizer
-- associated with the @ResumableConduit@. In order to guarantee process
-- resource finalization, you /must/ use this operator after using '=$$+' and
-- '=$$++'.
(=$$+-) :: Monad m => ResumableConduit i m o -> Sink o m r -> Sink i m r
rsrc =$$+- sink = do
    (ResumableConduit _ final, res) <- connectResume rsrc sink
    lift final
    return res
{-# INLINE (=$$+-) #-}


infixr 0 =$$+
infixr 0 =$$++
infixr 0 =$$+-
