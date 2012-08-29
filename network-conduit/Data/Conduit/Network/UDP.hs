module Data.Conduit.Network.UDP
    ( -- * Basic utilities
      sourceSocket
    , sinkSocket
    , sinkAllSocket
    , sinkToSocket
    , sinkAllToSocket
      -- * Helper Utilities
    , HostPreference (..)
    , bindPort
    , getSocket
    ) where

import Data.Conduit
import Network.Socket (AddrInfo, SockAddr, Socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recvFrom, send, sendAll, sendTo, sendAllTo)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (unless, void)
import Control.Monad.Trans.Class (lift)

import Data.Conduit.Network.Utils (HostPreference)
import qualified Data.Conduit.Network.Utils as Utils

-- | Stream messages from the socket.
--
-- The given @len@ defines the maximum packet size. Every produced item
-- contains the message payload and the origin address.
--
-- This function does /not/ automatically close the socket.
sourceSocket :: MonadIO m => Socket -> Int -> GSource m (ByteString, SockAddr)
sourceSocket socket len = loop
  where
    loop = do
        p@(bs, _) <- lift $ liftIO $ recvFrom socket len
        unless (S.null bs) $ yield p >> loop

-- | Stream messages to the connected socket.
--
-- The payload is sent using @send@, so some of it might be lost.
--
-- This function does /not/ automatically close the socket.
sinkSocket :: MonadIO m => Socket -> GInfSink ByteString m
sinkSocket = sinkSocketHelper (\sock bs -> void $ send sock bs)

-- | Stream messages to the connected socket.
--
-- The payload is sent using @sendAll@, so it might end up in multiple packets.
--
-- This function does /not/ automatically close the socket.
sinkAllSocket :: MonadIO m => Socket -> GInfSink ByteString m
sinkAllSocket = sinkSocketHelper sendAll

-- | Stream messages to the socket.
--
-- Every handled item contains the message payload and the destination
-- address. The payload is sent using @sendTo@, so some of it might be
-- lost.
--
-- This function does /not/ automatically close the socket.
sinkToSocket :: MonadIO m => Socket -> GInfSink (ByteString, SockAddr) m
sinkToSocket = sinkSocketHelper (\sock (bs, addr) -> void $ sendTo sock bs addr)

-- | Stream messages to the socket.
--
-- Every handled item contains the message payload and the destination
-- address. The payload is sent using @sendAllTo@, so it might end up in
-- multiple packets.
--
-- This function does /not/ automatically close the socket.
sinkAllToSocket :: MonadIO m => Socket -> GInfSink (ByteString, SockAddr) m
sinkAllToSocket = sinkSocketHelper $ uncurry . sendAllTo

-- | Attempt to connect to the given host/port.
getSocket :: String -> Int -> IO (Socket, AddrInfo)
getSocket host' port' = Utils.getSocket host' port' NS.Datagram

-- | Attempt to bind a listening @Socket@ on the given host/port. If no host is
-- given, will use the first address available.
bindPort :: Int -> HostPreference -> IO Socket
bindPort p s = Utils.bindPort p s NS.Datagram

-- Internal
sinkSocketHelper :: MonadIO m => (Socket -> a -> IO ())
                              -> Socket
                              -> GInfSink a m
sinkSocketHelper act socket = loop
  where
    loop = awaitE >>= either
                        return
                        (\a -> lift (liftIO $ act socket a) >> loop)
