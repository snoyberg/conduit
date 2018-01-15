{-# LANGUAGE RankNTypes, BangPatterns #-}
-- Compare low-level, fused, unfused, and partially fused
import Data.Conduit
import qualified Data.Conduit.List as CL
import Gauge.Main

-- | unfused
enumFromToC :: (Eq a, Monad m, Enum a) => a -> a -> ConduitT i a m ()
enumFromToC x0 y =
    loop x0
  where
    loop x
        | x == y = yield x
        | otherwise = yield x >> loop (succ x)
{-# INLINE enumFromToC #-}

-- | unfused
mapC :: Monad m => (a -> b) -> ConduitT a b m ()
mapC f = awaitForever $ yield . f
{-# INLINE mapC #-}

-- | unfused
foldC :: Monad m => (b -> a -> b) -> b -> ConduitT a o m b
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
              runConduitPure
            $ CL.enumFromTo 1 upper
           .| CL.map (* 2)
           .| CL.map (+ 1)
           .| CL.fold (+) 0
    , bench "runConduit, completely fused" $ flip whnf upper0 $ \upper ->
             runConduitPure
           $ CL.enumFromTo 1 upper
          .| CL.map (* 2)
          .| CL.map (+ 1)
          .| CL.fold (+) 0
    , bench "completely unfused" $ flip whnf upper0 $ \upper ->
              runConduitPure
            $ enumFromToC 1 upper
           .| mapC (* 2)
           .| mapC (+ 1)
           .| foldC (+) 0
    , bench "beginning fusion" $ flip whnf upper0 $ \upper ->
              runConduitPure
            $ (CL.enumFromTo 1 upper .| CL.map (* 2))
           .| mapC (+ 1)
           .| foldC (+) 0
    , bench "middle fusion" $ flip whnf upper0 $ \upper ->
              runConduitPure
            $ enumFromToC 1 upper
           .| (CL.map (* 2) .| CL.map (+ 1))
           .| foldC (+) 0
    , bench "ending fusion" $ flip whnf upper0 $ \upper ->
              runConduitPure
            $ enumFromToC 1 upper
           .| mapC (* 2)
           .| (CL.map (+ 1) .| CL.fold (+) 0)
    , bench "performance of CL.enumFromTo without fusion" $ flip whnf upper0 $ \upper ->
              runConduitPure
            $ CL.enumFromTo 1 upper
           .| mapC (* 2)
           .| (CL.map (+ 1) .| CL.fold (+) 0)
    ]
  where
    upper0 = 100000 :: Int
