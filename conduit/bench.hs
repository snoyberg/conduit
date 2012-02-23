import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Functor.Identity (runIdentity)
import Control.Monad.ST (runST)

main :: IO ()
main = defaultMain
    [ bench "bigsum-resourcet-io" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "bigsum-io" (whnfIO $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "bigsum-st" $ whnf (\i -> (runST $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) i)) 0
    , bench "bigsum-identity" $ whnf (\i -> (runIdentity $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) i)) 0
    {-
    , bench "bigsum-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CL.sourceList [1..1000 :: Int]
        bsrc C.$$ CL.fold (+) 0)
    , bench "fileread" (whnfIO $ C.runResourceT $ CB.sourceFile "bench" C.$$ CL.sinkNull)
    , bench "fileread-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CB.sourceFile "bench"
        bsrc C.$$ CL.sinkNull)
    , bench "map" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    , bench "map-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CL.sourceList [1..1000 :: Int]
        bsrc C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    , bench "map-buffer-alt" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CL.sourceList [1..1000 :: Int] C.$= CL.map (+ 1)
        bsrc C.$$ CL.fold (+) 0)
    -}
    ]

