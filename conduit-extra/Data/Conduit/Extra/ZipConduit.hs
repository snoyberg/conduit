{-# LANGUAGE DeriveFunctor #-}
module Data.Conduit.Extra.ZipConduit
    ( ZipConduit (..)
    , sequenceConduits
    ) where

import Data.Conduit
import Data.Conduit.Internal (Pipe (..), ConduitM (..), injectLeftovers)
import Data.Void (absurd)
import Control.Monad (liftM)
import Control.Applicative (Applicative (..))
import Data.Traversable (Traversable, sequenceA)

zipConduit :: Monad m
           => ConduitM i o m (x -> y)
           -> ConduitM i o m x
           -> ConduitM i o m y
zipConduit (ConduitM left0) (ConduitM right0) =
    ConduitM $ go (return ()) (return ()) (injectLeftovers left0) (injectLeftovers right0)
  where
    go _ _ (Done f) (Done x) = Done (f x)
    go _ finalY (HaveOutput x finalX o) y = HaveOutput
        (go finalX finalY x y)
        (finalX >> finalY)
        o
    go finalX _ x (HaveOutput y finalY o) = HaveOutput
        (go finalX finalY x y)
        (finalX >> finalY)
        o
    go _ _ (Leftover _ i) _ = absurd i
    go _ _ _ (Leftover _ i) = absurd i
    go finalX finalY (PipeM mx) y = PipeM (flip (go finalX finalY) y `liftM` mx)
    go finalX finalY x (PipeM my) = PipeM (go finalX finalY x `liftM` my)
    go finalX finalY (NeedInput px cx) (NeedInput py cy) = NeedInput
        (\i -> go finalX finalY (px i) (py i))
        (\u -> go finalX finalY (cx u) (cy u))
    go finalX finalY (NeedInput px cx) (Done y) = NeedInput
        (\i -> go finalX finalY (px i) (Done y))
        (\u -> go finalX finalY (cx u) (Done y))
    go finalX finalY (Done x) (NeedInput py cy) = NeedInput
        (\i -> go finalX finalY (Done x) (py i))
        (\u -> go finalX finalY (Done x) (cy u))

-- | Provides an alternative @Applicative@ instance for @ConduitM@. In this instance,
-- every incoming value is provided to all @ConduitM@s, and output is coalesced together.
-- Leftovers from individual @ConduitM@s will be used within that component, and then discarded
-- at the end of their computation. Output and finalizers will both be handled in a left-biased manner.
--
-- As an example, take the following program:
--
-- @
-- main :: IO ()
-- main = do
--     let src = mapM_ yield [1..3 :: Int]
--         conduit1 = CL.map (+1)
--         conduit2 = CL.concatMap (replicate 2)
--         conduit = getZipConduit $ ZipConduit conduit1 <* ZipConduit conduit2
--         sink = CL.mapM_ print
--     src $$ conduit =$ sink
-- @
--
-- It will produce the output: 2, 1, 1, 3, 2, 2, 4, 3, 3
--
-- Since 0.1.5
newtype ZipConduit i o m r = ZipConduit { getZipConduit :: ConduitM i o m r }
    deriving Functor
instance Monad m => Applicative (ZipConduit i o m) where
    pure = ZipConduit . pure
    ZipConduit left <*> ZipConduit right = ZipConduit (zipConduit left right)

-- | Provide identical input to all of the @Conduit@s and combine their outputs
-- into a single stream.
--
-- Implemented on top of @ZipConduit@, see that data type for more details.
--
-- Since 0.1.5
sequenceConduits :: (Traversable f, Monad m) => f (ConduitM i o m r) -> ConduitM i o m (f r)
sequenceConduits = getZipConduit . sequenceA . fmap ZipConduit
