import Data.Conduit
import Data.Conduit.Network
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM_)

main :: IO ()
main = do
    _ <- forkIO $ runTCPServer (ServerSettings 4000 Nothing) echo
    threadDelay 1000
    replicateM_ 10000
        $ runTCPClient (ClientSettings 4000 "localhost") doNothing

echo :: Application IO
echo src sink = src $$ sink

doNothing :: Application IO
doNothing _ _ = return ()
