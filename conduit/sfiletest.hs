import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Internal
import System.IO
import Control.Monad
import Control.Monad.IO.Class

sourceFile :: MonadResource m => FilePath -> Source m Char
sourceFile fp =
    bracketSPipe
        (putStrLn "opening source" >> openFile fp ReadMode)
        (\h -> putStrLn "closing source" >> hClose h)
        loop
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        unless eof $ do
            c <- liftIO $ hGetChar handle
            liftIO $ putStrLn $ "Read from source: " ++ show c
            yield c
            loop handle

sinkFile :: MonadResource m => FilePath -> Sink Char m ()
sinkFile fp =
    bracketSPipe
        (putStrLn "opening sink" >> openFile fp WriteMode)
        (\h -> putStrLn "closing sink" >> hClose h)
        (forever . go)
  where
    go handle = do
        c <- await
        liftIO $ putStrLn $ "Writing to sink: " ++ show c
        liftIO $ hPutChar handle c

conduitFile :: MonadResource m => FilePath -> Conduit Char m Char
conduitFile fp =
    bracketSPipe
        (putStrLn "opening conduit" >> openFile fp WriteMode)
        (\h -> putStrLn "closing conduit" >> hClose h)
        (forever . go)
  where
    go handle = do
        c <- await
        liftIO $ putStrLn $ "Writing to conduit: " ++ show c
        liftIO $ hPutChar handle c
        yield c

src1 = sourceFile "sfiletest.hs" $= CL.isolate 3
src2 = CL.sourceList "world"

main = runResourceT $
    ((src1 $= conduitFile "conduit") >> src2)
    $$ sinkFile "sink"
