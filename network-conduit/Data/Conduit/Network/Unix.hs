{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Conduit.Network.Unix
    ( -- * Basic utilities
      sourceSocket
    , sinkSocket
      -- * Simple server/client interface
    , Application
    , AppData
    , appSource
    , appSink
      -- ** Server
    , ServerSettings
    , serverSettings
    , serverPath
    , serverAfterBind
    , runUnixServer
      -- ** Client
    , ClientSettings
    , clientSettings
    , clientPath
    , runUnixClient
      -- * Helper utilities
    , bindPath
    , getSocket
    , acceptSafe
    ) where

import Data.Conduit
import Network.Socket (Socket)
import qualified Network.Socket as NS
import Data.Conduit.Network (sourceSocket, sinkSocket, acceptSafe)
import Data.Conduit.Network.Internal.Unix (AppData(..), ClientSettings(..), ServerSettings(..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception (throwIO, SomeException, try, finally, bracket,
                          bracketOnError, catch)
import Control.Monad (forever, void)
import Control.Monad.Trans.Control (control)
import Control.Concurrent (forkIO)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

-- | Attempt to connect to the given Unix domain socket path.
getSocket :: FilePath -> IO Socket
getSocket path = do
    sock <- NS.socket NS.AF_UNIX NS.Stream 0
    ee <- try' $ NS.connect sock (NS.SockAddrUnix path)
    case ee of
        Left e -> NS.sClose sock >> throwIO e
        Right () -> return sock
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try

-- | Attempt to bind a listening Unix domain socket at the given path.
--
-- Since 1.0.2
bindPath :: FilePath -> IO Socket
bindPath path = do
  sock <- bracketOnError
            (NS.socket NS.AF_UNIX NS.Stream 0)
            NS.sClose
            (\sock -> do
                removeFileSafe path  -- Cannot bind if the socket file exists.
                NS.bindSocket sock (NS.SockAddrUnix path)
                return sock)
  NS.listen sock (max 2048 NS.maxListenQueue)
  return sock

removeFileSafe :: FilePath -> IO ()
removeFileSafe path =
    removeFile path `catch` handleExists
  where
    handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | A simple Unix domain sockets application.
--
-- Since 1.0.2
type Application m = AppData m -> m ()

-- | Smart constructor.
--
-- Since 1.0.2
serverSettings :: Monad m
               => FilePath -- ^ path to bind to
               -> ServerSettings m
serverSettings path = ServerSettings
    { serverPath = path
    , serverAfterBind = const $ return ()
    }

-- | Run an @Application@ with the given settings. This function will create a
-- new listening socket, accept connections on it, and spawn a new thread for
-- each connection.
--
-- Since 1.0.2
runUnixServer :: (MonadIO m, MonadBaseControl IO m) => ServerSettings m -> Application m -> m ()
runUnixServer (ServerSettings path afterBind) app = control $ \run -> bracket
    (liftIO $ bindPath path)
    (liftIO . NS.sClose)
    (\socket -> run $ do
        afterBind socket
        forever $ serve socket)
  where
    serve lsocket = do
        (socket, _) <- liftIO $ acceptSafe lsocket
        let ad = AppData
                { appSource = sourceSocket socket
                , appSink = sinkSocket socket
                }
            app' run = void $ run (app ad)
            appClose run = app' run `finally` NS.sClose socket
        control $ \run -> forkIO (appClose run) >> run (return ())

-- | Smart constructor.
--
-- Since 1.0.2
clientSettings :: Monad m
               => FilePath -- ^ path to connect to
               -> ClientSettings m
clientSettings path = ClientSettings
    { clientPath = path
    }

-- | Run an @Application@ by connecting to the specified server.
--
-- Since 1.0.2
runUnixClient :: (MonadIO m, MonadBaseControl IO m) => ClientSettings m -> Application m -> m ()
runUnixClient (ClientSettings path) app = control $ \run -> bracket
    (getSocket path)
    NS.sClose
    (\sock -> run $ app AppData
        { appSource = sourceSocket sock
        , appSink = sinkSocket sock
        })
