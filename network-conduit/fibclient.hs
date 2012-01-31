import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Binary (sinkHandle)
import System.IO (stdout)
import Data.ByteString.Char8 (ByteString, pack)
import Control.Monad.Trans.Resource (resourceForkIO)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

fibs :: ResourceIO m => Source m Int
fibs =
    sourceState (1, 1) pull
  where
    pull (x, y) = do
        liftIO $ threadDelay 1000000
        return $ StateOpen (y, z) x
      where
        z = x + y

fibsBS :: ResourceIO m => Source m ByteString
fibsBS = fmap (\i -> pack $ show i ++ "\n") fibs

main :: IO ()
main = do
    runTCPClient (ClientSettings 5000 "localhost") $ \src sink -> do
        resourceForkIO $ fibsBS $$ sink
        src $$ sinkHandle stdout
