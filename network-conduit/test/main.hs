{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.Network
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM_)

main :: IO ()
main = do
    _ <- forkIO $ runTCPServer (serverSettings 4000 "*4") echo
    threadDelay 1000000
    replicateM_ 10000
        $ runTCPClient (clientSettings 4000 "localhost") doNothing

echo :: Application IO
echo ad = adSource ad $$ adSink ad

doNothing :: Application IO
doNothing _ = return ()
