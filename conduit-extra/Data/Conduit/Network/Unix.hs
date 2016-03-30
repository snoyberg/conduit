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

import Data.Conduit.Network (appSource, appSink, sourceSocket, sinkSocket)
import qualified Data.Streaming.Network as SN

clientSettings :: FilePath -> SN.ClientSettingsUnix
clientSettings = SN.clientSettingsUnix

serverSettings :: FilePath -> SN.ServerSettingsUnix
serverSettings = SN.serverSettingsUnix
