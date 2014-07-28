{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Data.Conduit.Network
    ( -- * Basic utilities
      sourceSocket
    , sinkSocket
      -- * Simple TCP server/client interface.
    , SN.AppData
    , appSource
    , appSink
    , SN.appSockAddr
    , SN.appLocalAddr
      -- ** Server
    , SN.ServerSettings
    , serverSettings
    , SN.runTCPServer
    , SN.runTCPServerWithHandle
    , runGeneralTCPServer
      -- ** Client
    , SN.ClientSettings
    , clientSettings
    , SN.runTCPClient
    , runGeneralTCPClient
      -- ** Getters
    , SN.getPort
    , SN.getHost
    , SN.getAfterBind
    , SN.getNeedLocalAddr
      -- ** Setters
    , SN.setPort
    , SN.setHost
    , SN.setAfterBind
    , SN.setNeedLocalAddr
      -- * Types
    , SN.HostPreference
    ) where

import Prelude hiding (catch)
import Data.Conduit
import qualified Network.Socket as NS
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString (ByteString)
import qualified GHC.Conc as Conc (yield)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception (throwIO, SomeException, try, finally, bracket, IOException, catch)
import Control.Monad (forever, unless, void)
import Control.Monad.Trans.Control (MonadBaseControl, control, liftBaseWith)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Streaming.Network as SN

-- | Stream data from the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sourceSocket :: MonadIO m => Socket -> Producer m ByteString
sourceSocket socket =
    loop
  where
    loop = do
        bs <- lift $ liftIO $ SN.safeRecv socket 4096
        if S.null bs
            then return ()
            else yield bs >> loop

-- | Stream data to the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sinkSocket :: MonadIO m => Socket -> Consumer ByteString m ()
sinkSocket socket =
    loop
  where
    loop = await >>= maybe (return ()) (\bs -> lift (liftIO $ sendAll socket bs) >> loop)

serverSettings = SN.serverSettingsTCP
clientSettings = SN.clientSettingsTCP

appSource :: (SN.HasReadWrite ad, MonadIO m) => ad -> Producer m ByteString
appSource ad =
    loop
  where
    read' = SN.appRead ad
    loop = do
        bs <- liftIO read'
        unless (S.null bs) $ do
            yield bs
            loop

appSink :: (SN.HasReadWrite ad, MonadIO m) => ad -> Consumer ByteString m ()
appSink ad = awaitForever $ \d -> liftIO $ SN.appWrite ad d >> Conc.yield

-- | Run a general TCP server
--
-- Same as 'SN.runTCPServer', except monad can be any instance of
-- 'MonadBaseControl' 'IO'.
--
-- Note that any changes to the monadic state performed by individual
-- client handlers will be discarded. If you have mutable state you want
-- to share among multiple handlers, you need to use some kind of mutable
-- variables.
--
-- Since 1.1.3
runGeneralTCPServer :: MonadBaseControl IO m
                    => SN.ServerSettings
                    -> (SN.AppData -> m ())
                    -> m ()
runGeneralTCPServer set f = liftBaseWith $ \run ->
    SN.runTCPServer set $ void . run . f

-- | Run a general TCP client
--
-- Same as 'SN.runTCPClient', except monad can be any instance of 'MonadBaseControl' 'IO'.
--
-- Since 1.1.3
runGeneralTCPClient :: MonadBaseControl IO m
                    => SN.ClientSettings
                    -> (SN.AppData -> m a)
                    -> m a
runGeneralTCPClient set f = control $ \run ->
    SN.runTCPClient set $ run . f
