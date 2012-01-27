module Data.Conduit.Network
    ( sourceSocket
    , sinkSocket
    ) where

import Data.Conduit
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Monad.IO.Class (liftIO)

-- | Stream data from the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sourceSocket :: ResourceIO m => Socket -> Source m ByteString
sourceSocket socket =
    Source $ return src
  where
    src = PreparedSource pull close

    pull = do
        bs <- liftIO (recv socket 4096)
        return $ if S.null bs then Closed else Open src bs
    close = return ()

-- | Stream data to the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sinkSocket :: ResourceIO m => Socket -> Sink ByteString m ()
sinkSocket socket =
    Sink $ return $ SinkData push close
  where
    push bs = do
        liftIO (sendAll socket bs)
        return (Processing push close)
    close = return ()
