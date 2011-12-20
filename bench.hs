{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.GZip as GZip
import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Codec.Zlib.Enum as EZ
import qualified System.IO as SIO

filePath :: FilePath
filePath = "sample.gz"

lazyIO :: IO ()
lazyIO = do
    compressed <- L.readFile filePath
    L.writeFile "tmp" $ GZip.decompress compressed

conduits1 :: IO ()
conduits1 = C.runResourceT
    $ CB.sourceFile "sample.gz"
    C.$$ CZ.ungzip
    C.=$ CB.sinkFile "tmp"

conduits2 :: IO ()
conduits2 = C.runResourceT
    $ CB.sourceFile "sample.gz"
    C.$= CZ.ungzip
    C.$$ CB.sinkFile "tmp"

enumerator1 :: IO ()
enumerator1 = SIO.withBinaryFile "tmp" SIO.WriteMode $ \h -> E.run_
    $ EB.enumFile filePath
    E.$$ EZ.ungzip
    E.=$ EB.iterHandle h

enumerator2 :: IO ()
enumerator2 = SIO.withBinaryFile "tmp" SIO.WriteMode $ \h -> E.run_
    $ EB.enumFile filePath
    E.$= EZ.ungzip
    E.$$ EB.iterHandle h

main :: IO ()
main = defaultMain
    [ bench "conduits1" conduits1
    , bench "conduits2" conduits2
    , bench "enumerator1" enumerator1
    , bench "enumerator2" enumerator2
    , bench "lazyIO" lazyIO
    ]
