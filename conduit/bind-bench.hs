import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad (foldM)
import Data.List (foldl')

input :: [Int]
input = [1..100 :: Int]

main :: IO ()
main = defaultMain
    [ bench "bind" $ nfIO $ C.runResourceT $ CL.sourceList input C.$$ bindSink 0
    , bench "no bind" $ nfIO $ C.runResourceT $ CL.sourceList input C.$$ CL.fold (+) 0
    , bench "no conduits" $ nfIO $ foldM plusM 0 input
    , bench "pure" $ nf (foldl' (+) 0) input
    ]

plusM a b = return (a + b)

bindSink accum = do
    mx <- CL.head
    case mx of
        Nothing -> return accum
        Just x -> let y = x + accum in y `seq` bindSink y

{-
Original results, conduit 0.1.2:

benchmarking bind
mean: 4.353290 ms, lb 4.339886 ms, ub 4.367442 ms, ci 0.950
std dev: 70.51453 us, lb 59.84734 us, ub 86.26725 us, ci 0.950
found 4 outliers among 100 samples (4.0%)
  3 (3.0%) high mild
variance introduced by outliers: 9.404%
variance is slightly inflated by outliers

benchmarking no bind
mean: 456.1767 us, lb 454.7851 us, ub 457.9179 us, ci 0.950
std dev: 7.919169 us, lb 6.413626 us, ub 9.522540 us, ci 0.950
found 13 outliers among 100 samples (13.0%)
  2 (2.0%) low mild
  11 (11.0%) high severe
variance introduced by outliers: 10.373%
variance is moderately inflated by outliers

benchmarking no conduits
mean: 12.09150 us, lb 12.05770 us, ub 12.12638 us, ci 0.950
std dev: 176.9865 ns, lb 151.5837 ns, ub 211.4992 ns, ci 0.950
found 2 outliers among 100 samples (2.0%)
  2 (2.0%) high mild
variance introduced by outliers: 7.533%
variance is slightly inflated by outliers

benchmarking pure
mean: 1.371857 us, lb 1.360364 us, ub 1.383382 us, ci 0.950
std dev: 58.94660 ns, lb 52.55791 ns, ub 66.36883 ns, ci 0.950
variance introduced by outliers: 40.507%
variance is moderately inflated by outliers

-}
