{-# LANGUAGE OverloadedStrings #-}

-- Build using threaded RTS

import Data.Conduit
import Data.Conduit.Network.UDP
import qualified Data.Conduit.List as CL

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 ()
import Network.Socket (addrAddress, connect, sClose)

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
sender = runResourceT $ CL.sourceList (repeat "abc") $$ sink
  where
    sink = bracketP
            (getSocket localhost port)
            (sClose . fst)
            (\(sock, addr) -> do
                liftIO $ connect sock (addrAddress addr)
                sinkSocket sock)

main :: IO ()
main = do
    rt <- forkIO receiver
    st <- forkIO sender

    threadDelay $ 1000 * 1000 * 5

    killThread st
    killThread rt
