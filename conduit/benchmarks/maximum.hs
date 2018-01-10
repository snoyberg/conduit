{-# LANGUAGE RankNTypes #-}
import           Control.Monad
import           Control.Monad.Identity
import           Criterion.Main
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.List (consume)
import           Data.Maybe
import           Prelude

{- I get output like this, which justifies the new definition:

benchmarking old maximum
time                 6.000 ns   (5.994 ns .. 6.008 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.997 ns   (5.992 ns .. 6.004 ns)
std dev              19.62 ps   (15.78 ps .. 25.19 ps)

benchmarking fold maximum
time                 4.514 ns   (4.510 ns .. 4.518 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.510 ns   (4.505 ns .. 4.516 ns)
std dev              18.15 ps   (13.74 ps .. 25.97 ps)

-}

-- Old definition
maximum1 :: (Monad m, Ord a) => Consumer a m (Maybe a)
maximum1 =
    await >>= maybe (return Nothing) loop
  where
    loop prev = await >>= maybe (return $ Just prev) (loop . max prev)
{-# INLINE [0] maximum1 #-}

maximum2 :: (Monad m, Ord a) => Consumer a m (Maybe a)
maximum2 = C.foldl1 max
{-# INLINE [0] maximum2 #-}

main = defaultMain
    [ bench "old maximum" $ runSink maximum1
    , bench "fold maximum" $ runSink maximum2
    ]

runSink :: Sink Int Identity b -> Benchmarkable
runSink f = error "runSink inlining didn't happen"
{-# NOINLINE runSink #-}
{-# RULES "define runSink" forall f.
  runSink f =
      flip whnf () $ \() ->
          runIdentity $
              C.enumFromTo 1 (100000 :: Int)
           $$ C.map (+1)
           $= C.map (+2)
           $= f
  #-}
