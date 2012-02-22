{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
    , bindPort
    , getSocket
    ) where

import Data.Conduit
import qualified Network.Socket as NS
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracketOnError, IOException, throwIO, SomeException, try)
import Control.Exception.Lifted (bracket)
import Control.Monad (forever)
import Control.Monad.Trans.Resource (register)
import Control.Concurrent.Lifted (fork)

-- | Stream data from the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sourceSocket :: ResourceIO m => Socket -> Source m ByteString
sourceSocket socket =
    src
  where
    src = Source pull close

    pull = do
        bs <- liftIO (recv socket 4096)
        return $ if S.null bs then Closed else Open src bs
    close = return ()

-- | Stream data to the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sinkSocket :: ResourceIO m => Socket -> Sink ByteString m ()
sinkSocket socket =
    SinkData push close
  where
    push bs = do
        liftIO (sendAll socket bs)
        return (Processing push close)
    close = return ()

-- | A simple TCP application. It takes two arguments: the @Source@ to read
-- input data from, and the @Sink@ to send output data to.
--
-- Since 0.2.1
type Application = Source IO ByteString
                -> Sink ByteString IO ()
                -> ResourceT IO ()

-- | Same as @Application@, but allows an arbitrary inner monad.
type ApplicationM m = Source m ByteString
                   -> Sink ByteString m ()
                   -> ResourceT m ()

-- | Settings for a TCP server. It takes a port to listen on, and an optional
-- hostname to bind to.
--
-- Since 0.2.1
data ServerSettings = ServerSettings
    { serverPort :: Int
    , serverHost :: Maybe String -- ^ 'Nothing' indicates no preference
    }

-- | Run an @Application@ with the given settings. This function will create a
-- new listening socket, accept connections on it, and spawn a new thread for
-- each connection.
--
-- Since 0.2.1
runTCPServer :: (Base m ~ IO, ResourceIO m) => ServerSettings -> ApplicationM m -> m ()
runTCPServer (ServerSettings port host) app = bracket
    (liftIO $ bindPort host port)
    (liftIO . NS.sClose)
    (forever . serve)
  where
    serve lsocket = do
        (socket, _addr) <- liftIO $ NS.accept lsocket
        fork $ runResourceT $ do
            _ <- register $ NS.sClose socket
            app (sourceSocket socket) (sinkSocket socket)

-- | Settings for a TCP client, specifying how to connect to the server.
data ClientSettings = ClientSettings
    { clientPort :: Int
    , clientHost :: String
    }

-- | Run an @Application@ by connecting to the specified server.
--
-- Since 0.2.1
runTCPClient :: ResourceIO m => ClientSettings -> ApplicationM m -> m ()
runTCPClient (ClientSettings port host) app = bracket
    (liftIO $ getSocket host port)
    (liftIO . NS.sClose)
    (\s -> runResourceT $ app (sourceSocket s) (sinkSocket s))

-- | Attempt to connect to the given host/port.
--
-- Since 0.2.1
getSocket :: String -> Int -> IO NS.Socket
getSocket host' port' = do
    let hints = NS.defaultHints {
                          NS.addrFlags = [NS.AI_ADDRCONFIG]
                        , NS.addrSocketType = NS.Stream
                        }
    (addr:_) <- NS.getAddrInfo (Just hints) (Just host') (Just $ show port')
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr)
                      (NS.addrProtocol addr)
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
-- Since 0.2.1
bindPort :: Maybe String -> Int -> IO Socket
bindPort host p = do
    let hints = NS.defaultHints
            { NS.addrFlags =
                [ NS.AI_PASSIVE
                , NS.AI_NUMERICSERV
                , NS.AI_NUMERICHOST
                ]
            , NS.addrSocketType = NS.Stream
            }
        port = Just . show $ p
    addrs <- NS.getAddrInfo (Just hints) host port
    let
        tryAddrs (addr1:rest@(_:_)) =
                                      catch
                                      (theBody addr1)
                                      (\(_ :: IOException) -> tryAddrs rest)
        tryAddrs (addr1:[])         = theBody addr1
        tryAddrs _                  = error "bindPort: addrs is empty"
        theBody addr =
          bracketOnError
          (NS.socket
            (NS.addrFamily addr)
            (NS.addrSocketType addr)
            (NS.addrProtocol addr))
          NS.sClose
          (\sock -> do
              NS.setSocketOption sock NS.ReuseAddr 1
              NS.bindSocket sock (NS.addrAddress addr)
              NS.listen sock NS.maxListenQueue
              return sock
          )
    tryAddrs addrs
