{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Conduit.Network
    ( -- * Basic utilities
      sourceSocket
    , sinkSocket
      -- * Simple TCP server/client interface.
    , Application
    , AppData
    , appSource
    , appSink
    , appSockAddr
    , appLocalAddr
      -- ** Server
    , ServerSettings
    , serverSettings
    , serverPort
    , serverHost
    , serverAfterBind
    , serverNeedLocalAddr
    , runTCPServer
      -- ** Client
    , ClientSettings
    , clientSettings
    , clientPort
    , clientHost
    , runTCPClient
      -- * Helper utilities
    , HostPreference (..)
    , bindPort
    , getSocket
    , acceptSafe
    ) where

import Prelude hiding (catch)
import Data.Conduit
import qualified Network.Socket as NS
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception (throwIO, SomeException, try, finally, bracket, IOException, catch)
import Control.Monad (forever)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent (forkIO, threadDelay)

import Data.Conduit.Network.Internal
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

-- | A simple TCP application.
--
-- Since 0.6.0
type Application m = AppData m -> m ()

-- | Smart constructor.
--
-- Since 0.6.0
serverSettings :: Monad m
               => Int -- ^ port to bind to
               -> HostPreference -- ^ host binding preferences
               -> ServerSettings m
serverSettings port host = ServerSettings
    { serverPort = port
    , serverHost = host
    , serverAfterBind = const $ return ()
    , serverNeedLocalAddr = False
    }

-- | Run an @Application@ with the given settings. This function will create a
-- new listening socket, accept connections on it, and spawn a new thread for
-- each connection.
--
-- Since 0.6.0
runTCPServer :: (MonadIO m, MonadBaseControl IO m) => ServerSettings m -> Application m -> m ()
runTCPServer (ServerSettings port host afterBind needLocalAddr) app = control $ \run -> bracket
    (liftIO $ bindPort port host)
    (liftIO . NS.sClose)
    (\socket -> run $ do
        afterBind socket
        forever $ serve socket)
  where
    serve lsocket = do
        (socket, addr) <- liftIO $ acceptSafe lsocket
        mlocal <- if needLocalAddr
                    then fmap Just $ liftIO (NS.getSocketName socket)
                    else return Nothing
        let ad = AppData
                { appSource = sourceSocket socket
                , appSink = sinkSocket socket
                , appSockAddr = addr
                , appLocalAddr = mlocal
                }
            app' run = run (app ad) >> return ()
            appClose run = app' run `finally` NS.sClose socket
        control $ \run -> forkIO (appClose run) >> run (return ())

-- | Smart constructor.
--
-- Since 0.6.0
clientSettings :: Monad m
               => Int -- ^ port to connect to
               -> ByteString -- ^ host to connect to
               -> ClientSettings m
clientSettings port host = ClientSettings
    { clientPort = port
    , clientHost = host
    }

-- | Run an @Application@ by connecting to the specified server.
--
-- Since 0.6.0
runTCPClient :: (MonadIO m, MonadBaseControl IO m) => ClientSettings m -> Application m -> m ()
runTCPClient (ClientSettings port host) app = control $ \run -> bracket
    (getSocket host port)
    (NS.sClose . fst)
    (\(s, address) -> run $ app AppData
        { appSource = sourceSocket s
        , appSink = sinkSocket s
        , appSockAddr = address
        , appLocalAddr = Nothing
        })

-- | Attempt to connect to the given host/port.
--
-- Since 0.6.0
getSocket :: ByteString -> Int -> IO (NS.Socket, NS.SockAddr)
getSocket host' port' = do
    (sock, addr) <- Utils.getSocket (S8.unpack host') port' NS.Stream
    ee <- try' $ NS.connect sock (NS.addrAddress addr)
    case ee of
        Left e -> NS.sClose sock >> throwIO e
        Right () -> return (sock, NS.addrAddress addr)
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

-- | Try to accept a connection, recovering automatically from exceptions.
--
-- As reported by Kazu against Warp, "resource exhausted (Too many open files)"
-- may be thrown by accept(). This function will catch that exception, wait a
-- second, and then try again.
--
-- Since 0.6.0
acceptSafe :: Socket -> IO (Socket, NS.SockAddr)
acceptSafe socket =
    loop
  where
    loop =
        NS.accept socket `catch` \(_ :: IOException) -> do
            threadDelay 1000000
            loop
