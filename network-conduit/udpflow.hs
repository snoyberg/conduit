{-# LANGUAGE OverloadedStrings #-}

-- Build using threaded RTS

import Data.Conduit
import Data.Conduit.Network.UDP
import qualified Data.Conduit.List as CL

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.ByteString.Char8 ()
import Network.Socket (addrAddress)

localhost :: String
localhost = "127.0.0.1"
port :: Int
port = 4000

receiver :: IO ()
receiver = do
    sock <- bindPort port (Host localhost)
    sourceSocket sock 4096 $$ CL.mapM_ (\_ -> return ())

sender :: IO ()
sender = do
    (sock, addr) <- getSocket localhost port
    CL.sourceList (repeat ("abc", addrAddress addr)) $$ sinkSocket sock

main :: IO ()
main = do
    forkIO receiver
    forkIO sender

    forever $ threadDelay $ 1000 * 1000 * 10
