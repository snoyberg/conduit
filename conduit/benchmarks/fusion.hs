{-# LANGUAGE RankNTypes #-}

module Main where

import           Criterion.Main
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Functor.Identity (runIdentity)
import           Data.Monoid

foldMapC :: (Monad m, Monoid b)
         => (a -> b)
         -> Consumer a m b
foldMapC f = let combiner accum = mappend accum . f in CL.fold combiner mempty
{-# INLINE foldMapC #-}

groupByC :: Monad m => (a -> a -> Bool) -> Conduit a m [a]
groupByC f =
    start
  where
    start = await >>= maybe (return ()) (loop id)

    loop rest x =
        await >>= maybe (yield (x : rest [])) go
      where
        go y
            | f x y     = loop (rest . (y:)) x
            | otherwise = yield (x : rest []) >> loop id y
{-# INLINE groupByC #-}

main :: IO ()
main = defaultMain
    [ -- groupByS is the most complicated streaming function - let's
      -- check if the fusion buys us anything.  This is also
      -- benchmarked as it shows that functions that use the
      -- STREAMING macro do indeed fuse.
      bgroup "groupBy"
          [ bench "fused" $ flip whnf upper0 $ \upper ->
              runIdentity
                  $ CL.enumFromTo 1 upper
                 $$ CL.map (`div` 10)
                 =$ CL.groupBy (==)
                 =$ CL.map length
                 =$ CL.fold (+) 0
          , bench "unfused" $ flip whnf upper0 $ \upper ->
              runIdentity
                  $ CL.enumFromTo 1 upper
                 $$ CL.map (`div` 10)
                 =$ groupByC (==)
                 =$ CL.map length
                 =$ CL.fold (+) 0
          -- Does fusion also benefit groupByS when it's run in the IO
          -- monad?
          , bench "fused, running on IO" $ whnfIO $
                    CL.enumFromTo 1 upper0
                 $$ CL.map (`div` 10)
                 =$ CL.groupBy (==)
                 =$ CL.map length
                 =$ CL.fold (+) 0
          , bench "unfused, running on IO" $ whnfIO $
                    CL.enumFromTo 1 upper0
                 $$ CL.map (`div` 10)
                 =$ groupByC (==)
                 =$ CL.map length
                 =$ CL.fold (+) 0
          ]
    -- foldMap uses the INLINE_RULE macro to implement fusion.  Here,
    -- we benchmark to check that this provides performance
    -- improvements.
    , bgroup "foldMap"
          [ bench "fused" $ flip whnf upper0 $ \upper ->
              runIdentity
                  $ CL.enumFromTo 1 upper
                 $$ CL.map (`div` 2)
                 =$ CL.foldMap Sum
          , bench "unfused" $ flip whnf upper0 $ \upper ->
              runIdentity
                  $ CL.enumFromTo 1 upper
                 $$ CL.map (`div` 2)
                 =$ foldMapC Sum
          , bench "fused, running on IO" $ whnfIO $
                    CL.enumFromTo 1 upper0
                 $$ CL.map (`div` 2)
                 =$ CL.foldMap Sum
          , bench "unfused, running on IO" $ whnfIO $
                    CL.enumFromTo 1 upper0
                 $$ CL.map (`div` 2)
                 =$ foldMapC Sum
          ]
    ]
  where
    upper0 = 100000 :: Int
