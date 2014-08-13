{-# LANGUAGE RankNTypes, BangPatterns #-}
-- Compare low-level, fused, unfused, and partially fused
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Internal (Step (..), Stream (..), unstream, StreamConduit (..))
import Criterion.Main
import Data.Functor.Identity (runIdentity)

-- | unfused
enumFromToC :: (Eq a, Monad m, Enum a) => a -> a -> Producer m a
enumFromToC x0 y =
    loop x0
  where
    loop x
        | x == y = yield x
        | otherwise = yield x >> loop (succ x)
{-# INLINE enumFromToC #-}

-- | unfused
mapC :: Monad m => (a -> b) -> Conduit a m b
mapC f = awaitForever $ yield . f
{-# INLINE mapC #-}

-- | unfused
foldC :: Monad m => (b -> a -> b) -> b -> Consumer a m b
foldC f =
    loop
  where
    loop !b = await >>= maybe (return b) (loop . f b)
{-# INLINE foldC #-}

main :: IO ()
main = defaultMain
    [ bench "low level" $ flip whnf upper0 $ \upper ->
        let loop x t
                | x > upper = t
                | otherwise = loop (x + 1) (t + ((x * 2) + 1))
         in loop 1 0
    , bench "completely fused" $ flip whnf upper0 $ \upper ->
        runIdentity
            $ CL.enumFromTo 1 upper
           $$ CL.map (* 2)
           =$ CL.map (+ 1)
           =$ CL.fold (+) 0
    , bench "runConduit, completely fused" $ flip whnf upper0 $ \upper ->
        runIdentity
            $ runConduit
            $ CL.enumFromTo 1 upper
          =$= CL.map (* 2)
          =$= CL.map (+ 1)
          =$= CL.fold (+) 0
    , bench "completely unfused" $ flip whnf upper0 $ \upper ->
        runIdentity
            $ enumFromToC 1 upper
           $$ mapC (* 2)
           =$ mapC (+ 1)
           =$ foldC (+) 0
    , bench "beginning fusion" $ flip whnf upper0 $ \upper ->
        runIdentity
            $ (CL.enumFromTo 1 upper $= CL.map (* 2))
           $$ mapC (+ 1)
           =$ foldC (+) 0
    , bench "middle fusion" $ flip whnf upper0 $ \upper ->
        runIdentity
            $ enumFromToC 1 upper
           $$ (CL.map (* 2) =$= CL.map (+ 1))
           =$ foldC (+) 0
    , bench "ending fusion" $ flip whnf upper0 $ \upper ->
        runIdentity
            $ enumFromToC 1 upper
           $= mapC (* 2)
           $$ (CL.map (+ 1) =$ CL.fold (+) 0)
    , bench "performance of CL.enumFromTo without fusion" $ flip whnf upper0 $ \upper ->
        runIdentity
            $ CL.enumFromTo 1 upper
           $= mapC (* 2)
           $$ (CL.map (+ 1) =$ CL.fold (+) 0)
    ]
  where
    upper0 = 100000 :: Int
