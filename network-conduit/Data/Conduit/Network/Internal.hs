{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE KindSignatures #-}
module Data.Conduit.Network.Internal
    ( AppData (..)
    , ServerSettings (..)
    , ClientSettings (..)
    ) where

import Data.ByteString (ByteString)
import Network.Socket (Socket, SockAddr)
import Data.Conduit (Source, Sink)
import Data.Conduit.Network.Utils (HostPreference)

-- | The data passed to an @Application@.
--
-- Since 0.6.0
data AppData m = AppData
    { appSource :: Source m ByteString
    , appSink :: Sink ByteString m ()
    , appSockAddr :: SockAddr
    }

-- | Settings for a TCP server. It takes a port to listen on, and an optional
-- hostname to bind to.
--
-- Since 0.6.0
data ServerSettings m = ServerSettings
    { serverPort :: Int
    , serverHost :: HostPreference
    , serverAfterBind :: Socket -> m ()
    }

-- | Settings for a TCP client, specifying how to connect to the server.
--
-- Since 0.6.0
data ClientSettings (m :: * -> *) = ClientSettings
    { clientPort :: Int
    , clientHost :: ByteString
    }
