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
import qualified Control.Monad.Trans.Resource as R

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

register1 :: IO ()
register1 = C.runResourceT $ R.register (return ()) >> return ()

register2 :: IO ()
register2 = C.runResourceT $ R.register (putStrLn' "") >> return ()

register3 :: IO ()
register3 = C.runResourceT $ do
    k <- R.register $ return ()
    R.release k

register4 :: IO ()
register4 = C.runResourceT $ do
    k <- R.register $ putStrLn' ""
    R.release k

register5 :: IO ()
register5 = C.runResourceT $
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    R.register (return ()) >> R.register (return ()) >> R.register (return ()) >>
    return ()

register6 :: IO ()
register6 = C.runResourceT $
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    R.register (putStrLn' "") >> R.register (putStrLn' "") >> R.register (putStrLn' "") >>
    return ()

register7 :: IO ()
register7 = C.runResourceT $ do
    k1 <- R.register $ return ()
    R.release k1
    R.register $ return ()
    k2 <- R.register $ return ()
    R.release k2
    R.register $ return ()
    k3 <- R.register $ return ()
    R.release k3
    R.register $ return ()
    k4 <- R.register $ return ()
    R.release k4
    R.register $ return ()
    k5 <- R.register $ return ()
    R.release k5
    return ()

register8 :: IO ()
register8 = C.runResourceT $ do
    k1 <- R.register $ putStrLn' ""
    R.release k1
    R.register $ putStrLn' ""
    k2 <- R.register $ putStrLn' ""
    R.release k2
    R.register $ putStrLn' ""
    k3 <- R.register $ putStrLn' ""
    R.release k3
    R.register $ putStrLn' ""
    k4 <- R.register $ putStrLn' ""
    R.release k4
    R.register $ putStrLn' ""
    k5 <- R.register $ putStrLn' ""
    R.release k5
    return ()

putStrLn' = SIO.hPutStrLn SIO.stderr

main :: IO ()
main = defaultMain
    [ bench "conduits1" conduits1
    , bench "conduits2" conduits2
    , bench "enumerator1" enumerator1
    , bench "enumerator2" enumerator2
    , bench "lazyIO" lazyIO
    , bench "register1" register1
    , bench "register2" register2
    , bench "register3" register3
    , bench "register4" register4
    , bench "register5" register5
    , bench "register6" register6
    , bench "register7" register7
    , bench "register8" register8
    ]
