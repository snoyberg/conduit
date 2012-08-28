{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.Network
    ( -- * Basic utilities
      sourceSocket
    , sinkSocket
      -- * Simple TCP server/client interface.
    , Application
      -- ** Server
    , ServerSettings (..)
    , runTCPServer
      -- ** Client
    , ClientSettings (..)
    , runTCPClient
      -- * Helper utilities
    , HostPreference (..)
    , bindPort
    , getSocket
    ) where

import Data.Conduit
import qualified Network.Socket as NS
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception (throwIO, SomeException, try, finally, bracket)
import Control.Monad (forever)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent (forkIO)

import Data.Conduit.Network.Utils (HostPreference)
import qualified Data.Conduit.Network.Utils as Utils

-- | Stream data from the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sourceSocket :: MonadIO m => Socket -> GSource m ByteString
sourceSocket socket =
    loop
  where
    loop = do
        bs <- lift $ liftIO $ recv socket 4096
        if S.null bs
            then return ()
            else yield bs >> loop

-- | Stream data to the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sinkSocket :: MonadIO m => Socket -> GInfSink ByteString m
sinkSocket socket =
    loop
  where
    loop = awaitE >>= either return (\bs -> lift (liftIO $ sendAll socket bs) >> loop)

-- | A simple TCP application. It takes two arguments: the @Source@ to read
-- input data from, and the @Sink@ to send output data to.
--
-- Since 0.3.0
type Application m = Source m ByteString
                  -> Sink ByteString m ()
                  -> m ()

-- | Settings for a TCP server. It takes a port to listen on, and an optional
-- hostname to bind to.
--
-- Since 0.3.0
data ServerSettings = ServerSettings
    { serverPort :: Int
    , serverHost :: HostPreference
    }
      deriving (Eq, Show, Read)

-- | Run an @Application@ with the given settings. This function will create a
-- new listening socket, accept connections on it, and spawn a new thread for
-- each connection.
--
-- Since 0.3.0
runTCPServer :: (MonadIO m, MonadBaseControl IO m) => ServerSettings -> Application m -> m ()
runTCPServer (ServerSettings port host) app = control $ \run -> bracket
    (liftIO $ bindPort port host)
    (liftIO . NS.sClose)
    (run . forever . serve)
  where
    serve lsocket = do
        (socket, _addr) <- liftIO $ NS.accept lsocket
        let src = sourceSocket socket
            sink = sinkSocket socket
            app' run = run (app src sink) >> return ()
            appClose run = app' run `finally` NS.sClose socket
        control $ \run -> forkIO (appClose run) >> run (return ())

-- | Settings for a TCP client, specifying how to connect to the server.
--
-- Since 0.2.1
data ClientSettings = ClientSettings
    { clientPort :: Int
    , clientHost :: String
    }
      deriving (Eq, Show, Read)

-- | Run an @Application@ by connecting to the specified server.
--
-- Since 0.2.1
runTCPClient :: (MonadIO m, MonadBaseControl IO m) => ClientSettings -> Application m -> m ()
runTCPClient (ClientSettings port host) app = control $ \run -> bracket
    (getSocket host port)
    NS.sClose
    (\s -> run $ app (sourceSocket s) (sinkSocket s))

-- | Attempt to connect to the given host/port.
--
-- Since 0.2.1
getSocket :: String -> Int -> IO NS.Socket
getSocket host' port' = do
    (sock, addr) <- Utils.getSocket host' port' NS.Stream
    ee <- try' $ NS.connect sock (NS.addrAddress addr)
    case ee of
        Left e -> NS.sClose sock >> throwIO e
        Right () -> return sock
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try

-- | Attempt to bind a listening @Socket@ on the given host/port. If no host is
-- given, will use the first address available.
--
-- Since 0.3.0
bindPort :: Int -> HostPreference -> IO Socket
bindPort p s = do
    sock <- Utils.bindPort p s NS.Stream
    NS.listen sock NS.maxListenQueue
    return sock
