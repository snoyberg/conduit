{-# LANGUAGE OverloadedStrings #-}

-- Build using threaded RTS

import Data.Conduit
import Data.Conduit.Network.UDP
import qualified Data.Conduit.List as CL

import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString.Char8 ()
import Network.Socket (addrAddress, sClose)

localhost :: String
localhost = "127.0.0.1"
port :: Int
port = 4000

receiver :: IO ()
receiver = runResourceT $ src $$ CL.mapM_ (\_ -> return ())
  where
    src = bracketP
            (bindPort port (Host localhost))
            sClose
            (\sock -> sourceSocket sock 4096)

sender :: IO ()
sender = do
    (sock, addr) <- getSocket localhost port

    let sink = addCleanup
                (const $ sClose sock)
                (sinkSocket sock)

    CL.sourceList (repeat ("abc", addrAddress addr)) $$ sink

main :: IO ()
main = do
    forkIO receiver
    forkIO sender

    threadDelay $ 1000 * 1000 * 5
