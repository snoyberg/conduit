{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE KindSignatures #-}
module Data.Conduit.Network.Internal.Unix
    ( AppData (..)
    , ServerSettings (..)
    , ClientSettings (..)
    ) where

import Data.ByteString (ByteString)
import Network.Socket (Socket)
import Data.Conduit (Source, Sink)

-- | The data passed to a Unix domain sockets @Application@.
--
-- Since 1.0.2
data AppData m = AppData
    { appSource :: Source m ByteString
    , appSink :: Sink ByteString m ()
    }

-- | Settings for a Unix domain sockets server.
--
-- Since 1.0.2
data ServerSettings m = ServerSettings
    { serverPath :: FilePath
    , serverAfterBind :: Socket -> m ()
    }

-- | Settings for a Unix domain sockets client.
--
-- Since 1.0.2
data ClientSettings (m :: * -> *) = ClientSettings
    { clientPath :: FilePath
    }
