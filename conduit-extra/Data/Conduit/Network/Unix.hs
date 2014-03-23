{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Conduit.Network.Unix
    ( -- * Basic utilities
      sourceSocket
    , sinkSocket
      -- * Simple server/client interface
    , SN.AppDataUnix
    , appSource
    , appSink
      -- ** Server
    , SN.ServerSettingsUnix
    , serverSettings
    , SN.runUnixServer
      -- ** Client
    , SN.ClientSettingsUnix
    , clientSettings
    , SN.runUnixClient
      -- ** Getters
    , SN.getPath
    , SN.getAfterBind
      -- ** Setters
    , SN.setPath
    , SN.setAfterBind
    ) where

import Data.Conduit
import Network.Socket (Socket)
import qualified Network.Socket as NS
import Data.Conduit.Network (appSource, appSink, sourceSocket, sinkSocket)
import qualified Data.Streaming.Network as SN
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception (throwIO, SomeException, try, finally, bracket,
                          bracketOnError, catch)
import Control.Monad (forever, void)
import Control.Monad.Trans.Control (control)
import Control.Concurrent (forkIO)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import Control.Monad.Trans.Resource (MonadBaseControl)

clientSettings = SN.clientSettingsUnix
serverSettings = SN.serverSettingsUnix
