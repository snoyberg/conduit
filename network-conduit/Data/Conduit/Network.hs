{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
import Control.Exception (bracketOnError, IOException, throwIO, SomeException, try, finally, bracket)
import Control.Monad (forever)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Control.Concurrent (forkIO)
import Data.String (IsString (fromString))
import qualified Control.Exception as E

-- | Stream data from the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sourceSocket :: MonadIO m => Socket -> Source m ByteString
sourceSocket socket =
    src
  where
    src = PipeM pull close

    pull = do
        bs <- liftIO (recv socket 4096)
        return $ if S.null bs then Done Nothing () else HaveOutput src close bs
    close = return ()

-- | Stream data to the socket.
--
-- This function does /not/ automatically close the socket.
--
-- Since 0.0.0
sinkSocket :: MonadIO m => Socket -> Sink ByteString m ()
sinkSocket socket =
    sink
  where
    sink = NeedInput push (Done Nothing ())

    push bs = PipeM (do
        liftIO (sendAll socket bs)
        return sink) (return ())

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

-- | Which host to bind.
--
-- Note: The @IsString@ instance recognizes the following special values:
--
-- * @*@ means @HostAny@
--
-- * @*4@ means @HostIPv4@
--
-- * @*6@ means @HostIPv6@
data HostPreference =
    HostAny
  | HostIPv4
  | HostIPv6
  | Host String
    deriving (Eq, Ord, Show, Read)

instance IsString HostPreference where
    -- The funny code coming up is to get around some irritating warnings from
    -- GHC. I should be able to just write:
    {-
    fromString "*" = HostAny
    fromString "*4" = HostIPv4
    fromString "*6" = HostIPv6
    -}
    fromString s'@('*':s) =
        case s of
            [] -> HostAny
            ['4'] -> HostIPv4
            ['6'] -> HostIPv6
            _ -> Host s'
    fromString s = Host s

-- | Attempt to bind a listening @Socket@ on the given host/port. If no host is
-- given, will use the first address available.
--
-- Since 0.3.0
bindPort :: Int -> HostPreference -> IO Socket
bindPort p s = do
    let hints = NS.defaultHints
            { NS.addrFlags = [ NS.AI_PASSIVE
                             , NS.AI_NUMERICSERV
                             , NS.AI_NUMERICHOST
                             ]
            , NS.addrSocketType = NS.Stream
            }
        host =
            case s of
                Host s' -> Just s'
                _ -> Nothing
        port = Just . show $ p
    addrs <- NS.getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs4 = filter (\x -> NS.addrFamily x /= NS.AF_INET6) addrs
        addrs6 = filter (\x -> NS.addrFamily x == NS.AF_INET6) addrs
        addrs' =
            case s of
                HostIPv4 -> addrs4 ++ addrs6
                HostIPv6 -> addrs6 ++ addrs4
                _ -> addrs

        tryAddrs (addr1:rest@(_:_)) =
                                      E.catch
                                      (theBody addr1)
                                      (\(_ :: IOException) -> tryAddrs rest)
        tryAddrs (addr1:[])         = theBody addr1
        tryAddrs _                  = error "bindPort: addrs is empty"
        theBody addr =
          bracketOnError
          (NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr))
          NS.sClose
          (\sock -> do
              NS.setSocketOption sock NS.ReuseAddr 1
              NS.bindSocket sock (NS.addrAddress addr)
              NS.listen sock NS.maxListenQueue
              return sock
          )
    tryAddrs addrs'
