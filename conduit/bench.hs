import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = defaultMain
    [ bench "bigsum" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "fileread" (whnfIO $ C.runResourceT $ CB.sourceFile "bench" C.$$ CL.sinkNull)
    , bench "map" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$= CL.map (+ 1) C.$$ CL.fold (+) 0)
    ]

