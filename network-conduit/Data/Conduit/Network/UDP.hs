module Data.Conduit.Network.UDP
    ( -- * Basic utilities
      sourceSocket
    , sinkSocket
    ) where

import Data.Conduit
import Network.Socket (SockAddr, Socket)
import Network.Socket.ByteString (recvFrom, sendAllTo)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)

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

-- | Stream messages to the socket.
--
-- Every handled item contains the message payload and the destination
-- address.
--
-- This function does /not/ automatically close the socket.
sinkSocket :: MonadIO m => Socket -> GInfSink (ByteString, SockAddr) m
sinkSocket socket = loop
  where
    loop = awaitE >>= either
                        return
                        (\(bs, addr) -> lift (liftIO $ sendAllTo socket bs addr) >> loop)
