import Data.Conduit
import Data.Conduit.Streams
import Data.Conduit.Binary
import qualified System.IO.Streams as Streams
import Criterion.Main
import System.IO
import qualified Data.ByteString.Lazy as L

inputFP = "input.dat"
outputFP = "output.dat"

main = do
    runResourceT $ sourceFile "/dev/urandom"
                $$ isolate (1024 * 1024)
                =$ sinkFile inputFP
    defaultMain
        [ bench "conduit+resourcet" $ whnfIO $
            runResourceT $ sourceFile inputFP $$ sinkFile outputFP
        , bench "conduit+NoHandle" $ whnfIO $
            runResourceT $ sourceFileNoHandle inputFP $$ sinkFileNoHandle outputFP
        , bench "conduit+bracket" $ whnfIO $
            withFile inputFP ReadMode $ \i ->
            withFile outputFP WriteMode $ \o ->
            sourceHandle i $$ sinkHandle o
        , bench "conduit+iostreams" $ whnfIO $
            Streams.withFileAsInput inputFP $ \i ->
            Streams.withFileAsOutput outputFP $ \o ->
            sourceStream i $$ sinkStream o
        , bench "iostreams" $ whnfIO $
            Streams.withFileAsInput inputFP $ \i ->
            Streams.withFileAsOutput outputFP $ \o ->
            Streams.connect i o
        , bench "lazy" $ whnfIO $
            withFile inputFP ReadMode $ \i ->
            withFile outputFP WriteMode $ \o ->
            L.hGetContents i >>= L.hPut o
        ]
