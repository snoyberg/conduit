import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Functor.Identity (runIdentity)
import Control.Monad.ST (runST)
import Control.Monad (foldM)
import Data.List (foldl')

main :: IO ()
main = defaultMain
    [ bench "bigsum-resourcet-io" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "bigsum-io" (whnfIO $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "bigsum-st" $ whnf (\i -> (runST $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) i)) 0
    , bench "bigsum-identity" $ whnf (\i -> (runIdentity $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) i)) 0
    , bench "bigsum-identity-pure" $ whnf (\i -> (runIdentity $ CL.sourceListPure [1..1000 :: Int] C.$$ CL.foldPure (+) i)) 0
    , bench "bigsum-foldM" $ whnf (\i -> (runIdentity $ foldM (\a b -> return $! a + b) i [1..1000 :: Int])) 0
    , bench "bigsum-pure" $ whnf (\i -> foldl' (+) i [1..1000 :: Int]) 0
    ]
