import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Binary (sinkHandle)
import System.IO (stdout)
import Data.ByteString.Char8 (ByteString, pack)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.IO.Class (MonadIO, liftIO)

fibs :: MonadIO m => Source m Int
fibs =
    go (1, 1)
  where
    go (x, y) = do
        liftIO $ threadDelay 1000000
        yield x >> go (y, z)
      where
        z = x + y

fibsBS :: MonadIO m => Source m ByteString
fibsBS = mapOutput (\i -> pack $ show i ++ "\n") fibs

main :: IO ()
main = do
    runTCPClient (clientSettings 5000 (pack "localhost")) $ \app -> do
        forkIO $ fibsBS $$ (appSink app)
        (appSource app) $$ sinkHandle stdout
