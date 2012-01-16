import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = defaultMain
    [ bench "bigsum" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "fileread" (whnfIO $ C.runResourceT $ CB.sourceFile "bench" C.$$ CL.sinkNull)
    , bench "fileread-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CB.sourceFile "bench"
        bsrc C.$$ CL.sinkNull)
    , bench "fileread-buffer2" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource2 $ CB.sourceFile "bench"
        bsrc `C.connect2` CL.sinkNull)
    , bench "map" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    , bench "bigsum-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CL.sourceList [1..1000 :: Int]
        bsrc C.$$ CL.fold (+) 0)
    , bench "map-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource $ CL.sourceList [1..1000 :: Int]
        bsrc C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    , bench "bigsum-buffer2" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource2 $ CL.sourceList [1..1000 :: Int]
        bsrc `C.connect2` CL.fold (+) 0)
    {-
    , bench "map-buffer" (whnfIO $ C.runResourceT $ do
        bsrc <- C.bufferSource2 $ CL.sourceList [1..1000 :: Int]
        bsrc C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    -}
    ]

