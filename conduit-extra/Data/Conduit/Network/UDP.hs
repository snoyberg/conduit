{-# LANGUAGE RankNTypes #-}
module Data.Conduit.Network.UDP
    ( -- * UDP message representation
      SN.Message (..)
      -- * Basic utilities
    , sourceSocket
    , sinkSocket
    , sinkAllSocket
    , sinkToSocket
    , sinkAllToSocket
      -- * Helper Utilities
    , SN.HostPreference
    ) where

import Data.Conduit
import Network.Socket (AddrInfo, SockAddr, Socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recvFrom, send, sendAll, sendTo, sendAllTo)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import qualified Data.Streaming.Network as SN

-- | Stream messages from the socket.
--
-- The given @len@ defines the maximum packet size. Every produced item
-- contains the message payload and the origin address.
--
-- This function does /not/ automatically close the socket.
sourceSocket :: MonadIO m => Socket -> Int -> Producer m SN.Message
sourceSocket socket len = loop
  where
    loop = do
        (bs, addr) <- lift $ liftIO $ recvFrom socket len
        yield (SN.Message bs addr) >> loop

-- | Stream messages to the connected socket.
--
-- The payload is sent using @send@, so some of it might be lost.
--
-- This function does /not/ automatically close the socket.
sinkSocket :: MonadIO m => Socket -> Consumer ByteString m ()
sinkSocket = sinkSocketHelper (\sock bs -> void $ send sock bs)

-- | Stream messages to the connected socket.
--
-- The payload is sent using @sendAll@, so it might end up in multiple packets.
--
-- This function does /not/ automatically close the socket.
sinkAllSocket :: MonadIO m => Socket -> Consumer ByteString m ()
sinkAllSocket = sinkSocketHelper sendAll

-- | Stream messages to the socket.
--
-- Every handled item contains the message payload and the destination
-- address. The payload is sent using @sendTo@, so some of it might be
-- lost.
--
-- This function does /not/ automatically close the socket.
sinkToSocket :: MonadIO m => Socket -> Consumer SN.Message m ()
sinkToSocket = sinkSocketHelper (\sock (SN.Message bs addr) -> void $ sendTo sock bs addr)

-- | Stream messages to the socket.
--
-- Every handled item contains the message payload and the destination
-- address. The payload is sent using @sendAllTo@, so it might end up in
-- multiple packets.
--
-- This function does /not/ automatically close the socket.
sinkAllToSocket :: MonadIO m => Socket -> Consumer SN.Message m ()
sinkAllToSocket = sinkSocketHelper (\sock (SN.Message bs addr) -> sendAllTo sock bs addr)

-- Internal
sinkSocketHelper :: MonadIO m => (Socket -> a -> IO ())
                              -> Socket
                              -> Consumer a m ()
sinkSocketHelper act socket = loop
  where
    loop = await >>= maybe
                        (return ())
                        (\a -> lift (liftIO $ act socket a) >> loop)
{-# INLINE sinkSocketHelper #-}
