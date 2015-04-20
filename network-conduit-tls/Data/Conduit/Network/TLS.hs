{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
module Data.Conduit.Network.TLS
    ( -- * Server
      TLSConfig
    , tlsConfigBS
    , tlsConfig
    , tlsConfigChainBS
    , tlsConfigChain
    , tlsHost
    , tlsPort
--    , tlsCertificate
--    , tlsKey
    , tlsNeedLocalAddr
    , tlsAppData
    , runTCPServerTLS
    , runGeneralTCPServerTLS
    , runTCPServerStartTLS
      -- * Client
    , TLSClientConfig
    , tlsClientConfig
    , runTLSClient
    , runTLSClientStartTLS
    , tlsClientPort
    , tlsClientHost
    , tlsClientUseTLS
    , tlsClientTLSSettings
    , tlsClientSockSettings
    , tlsClientConnectionContext
    ) where

import Prelude hiding (FilePath, readFile)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever, void)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem (readFile)
import qualified Data.ByteString.Lazy as L
import qualified Network.TLS as TLS
import Data.Conduit.Network (sinkSocket, runTCPServerWithHandle, serverSettings, sourceSocket)
import Data.Streaming.Network.Internal (AppData (..), HostPreference)
import Data.Streaming.Network (ConnectionHandle, safeRecv)
import Data.Conduit.Network.TLS.Internal
import Data.Conduit (yield, awaitForever, Producer, Consumer)
import qualified Data.Conduit.List as CL
import Network.Socket (SockAddr (SockAddrInet), sClose)
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception (bracket)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Network.TLS.Extra as TLSExtra
import Network.Socket (Socket)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Crypto.Random.AESCtr
import qualified Network.Connection as NC
import Control.Monad.Trans.Control
import Data.Default


makeCertDataPath :: FilePath -> [FilePath] -> FilePath -> TlsCertData
makeCertDataPath certPath chainCertPaths keyPath =
    TlsCertData
      (readFile certPath)
      (mapM readFile chainCertPaths)
      (readFile keyPath)

makeCertDataBS :: S.ByteString -> [S.ByteString] -> S.ByteString ->
                  TlsCertData
makeCertDataBS certBS chainCertsBS keyBS =
    TlsCertData (return certBS) (return chainCertsBS) (return keyBS)

tlsConfig :: HostPreference
          -> Int -- ^ port
          -> FilePath -- ^ certificate
          -> FilePath -- ^ key
          -> TLSConfig
tlsConfig a b c d = tlsConfigChain a b c [] d


-- | allow to build a server config directly from raw bytestring data (exact same
-- string as if the certificates were read from the filesystem).
-- this enables to plug another backend to fetch certifcates (other than FS) 
tlsConfigBS :: HostPreference
            -> Int          -- ^ port
            -> S.ByteString -- ^ Certificate raw data
            -> S.ByteString -- ^ Key file raw data
            -> TLSConfig
tlsConfigBS a b c d = tlsConfigChainBS a b c [] d

-- | Like 'tlsConfig', but also allow specifying chain certificates.
--
-- Since 1.1.1
tlsConfigChain :: HostPreference
               -> Int -- ^ Port
               -> FilePath -- ^ Certificate
               -> [FilePath] -- ^ Chain certificates
               -> FilePath -- ^ Key
               -> TLSConfig
tlsConfigChain a b c d e = TLSConfig a b (makeCertDataPath c d e) False


-- | Like 'tlsConfigBS', but also allow specifying chain certificates.
--
-- Since 1.1.1
tlsConfigChainBS :: HostPreference
                 -> Int          -- ^ Port
                 -> S.ByteString -- ^ Certificate raw data
                 -> [S.ByteString] -- ^ Chain certificate raw data
                 -> S.ByteString -- ^ Key file raw data
                 -> TLSConfig
tlsConfigChainBS a b c d e = TLSConfig a b (makeCertDataBS c d e) False

serverHandshake :: Socket -> TLS.Credentials -> IO (TLS.Context)
serverHandshake socket creds = do
    gen <- Crypto.Random.AESCtr.makeSystem

    ctx <- TLS.contextNew
           TLS.Backend
                    { TLS.backendFlush = return ()
                    , TLS.backendClose = return ()
                    , TLS.backendSend = sendAll socket
                    , TLS.backendRecv = recvExact socket
                    }
            params
            gen

    TLS.handshake ctx
    return ctx

  where
    params = def
        { TLS.serverWantClientCert = False
        , TLS.serverSupported = def
            { TLS.supportedCiphers = ciphers
            , TLS.supportedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
            }
        , TLS.serverShared = def
            { TLS.sharedCredentials = creds
            }
        }

runTCPServerTLS :: TLSConfig -> (AppData -> IO ()) -> IO ()
runTCPServerTLS TLSConfig{..} app = do
    creds <- readCreds tlsCertData

    runTCPServerWithHandle settings (wrapApp creds)

    where
      -- convert tls settings to regular conduit network ones
      settings = serverSettings tlsPort tlsHost  -- (const $ return () ) tlsNeedLocalAddr

      wrapApp creds = app'
        where
          app' socket addr mlocal = do
            ctx <- serverHandshake socket creds
            app (tlsAppData ctx addr mlocal)
            TLS.bye ctx

type ApplicationStartTLS = (AppData, (AppData -> IO ()) -> IO ()) -> IO ()

-- | Like 'runTCPServerTLS', but monad can be any instance of 'MonadBaseControl' 'IO'.
--
-- Note that any changes to the monadic state performed by individual
-- client handlers will be discarded. If you have mutable state you want
-- to share among multiple handlers, you need to use some kind of mutable
-- variables.
runGeneralTCPServerTLS :: MonadBaseControl IO m => TLSConfig -> (AppData -> m ()) -> m ()
runGeneralTCPServerTLS config app = liftBaseWith $ \run ->
  runTCPServerTLS config $ void . run . app

-- | run a server un-crypted but also pass a call-back to trigger a StartTLS handshake
-- on the underlying connection
--
-- example usage :
--  @
--  runTCPServerStartTLS serverConfig $ (appData,startTLS) -> do
--    abortTLS <- doSomethingInClear appData
--    unless (abortTLS) $ startTls $ appDataTls -> do
--      doSomethingSSL appDataTls
--  @
runTCPServerStartTLS :: TLSConfig -> ApplicationStartTLS -> IO ()
runTCPServerStartTLS TLSConfig{..} app = do
    creds <- readCreds tlsCertData

    runTCPServerWithHandle settings (wrapApp creds)

    where
      -- convert tls settings to regular conduit network ones
      settings = serverSettings tlsPort tlsHost  -- (const $ return () ) tlsNeedLocalAddr

      wrapApp creds = clearapp
        where clearapp socket addr mlocal = let
                -- setup app data for the clear part of the connection
                clearData = AppData
                  { appRead' = safeRecv socket 4096
                  , appWrite' = sendAll socket
                  , appSockAddr' = addr
                  , appLocalAddr' = mlocal
#if MIN_VERSION_streaming_commons(0,1,6)
                  , appCloseConnection' = sClose socket
#endif
#if MIN_VERSION_streaming_commons(0,1,12)
                  , appRawSocket' = Just socket
#endif
                  }
                -- wrap up the current connection with TLS
                startTls = \app' -> do
                  ctx <- serverHandshake socket creds
                  app' (tlsAppData ctx addr mlocal)
                  TLS.bye ctx
                in
                 app (clearData, startTls)

-- | Create an @AppData@ from an existing tls @Context@ value. This is a lower level function, allowing you to create a connection in any way you want.
--
-- Sample usage:
--
-- > import Network.Simple.TCP.TLS
-- >
-- > myapp :: Application IO
-- > ...
-- > main = do
-- >     cset <- getDefaultClientSettings
-- >     connect cset "host" "port" $
-- >         (\(ctx, addr) -> myapp $ tlsAppData ctx addr Nothing)
--
-- Since 1.0.1
tlsAppData :: TLS.Context       -- ^ a TLS context
           -> SockAddr          -- ^ remote address
           -> Maybe SockAddr    -- ^ local address
           -> AppData
tlsAppData ctx addr mlocal = AppData
    { appRead' = TLS.recvData ctx
    , appWrite' = TLS.sendData ctx . L.fromChunks . return
    , appSockAddr' = addr
    , appLocalAddr' = mlocal
#if MIN_VERSION_streaming_commons(0,1,6)
    , appCloseConnection' = TLS.contextClose ctx
#endif
#if MIN_VERSION_streaming_commons(0,1,12)
    , appRawSocket' = Nothing
#endif
    }

-- taken from stunnel example in tls-extra
ciphers :: [TLS.Cipher]
ciphers =
    [ TLSExtra.cipher_AES128_SHA1
    , TLSExtra.cipher_AES256_SHA1
    , TLSExtra.cipher_RC4_128_MD5
    , TLSExtra.cipher_RC4_128_SHA1
    ]

readCreds :: TlsCertData -> IO TLS.Credentials
readCreds (TlsCertData iocert iochains iokey) =
    (TLS.credentialLoadX509ChainFromMemory <$> iocert <*> iochains <*> iokey)
    >>= either
        (error . ("Error reading TLS credentials: " ++))
        (return . TLS.Credentials . return)

-- | TLS requires exactly the number of bytes requested to be returned.
recvExact :: Socket -> Int -> IO S.ByteString
recvExact socket =
    loop id
  where
    loop front rest
        | rest < 0 = error "Data.Conduit.Network.TLS.recvExact: rest < 0"
        | rest == 0 = return $ S.concat $ front []
        | otherwise = do
            next <- recv socket rest
            if S.length next == 0
                then return $ S.concat $ front []
                else loop (front . (next:)) $ rest - S.length next

-- | Settings type for TLS client connection.
--
-- Since 1.0.2
data TLSClientConfig = TLSClientConfig
    { tlsClientPort :: Int
    -- ^
    --
    -- Since 1.0.2
    , tlsClientHost :: S.ByteString
    -- ^
    --
    -- Since 1.0.2
    , tlsClientUseTLS :: Bool
    -- ^ Default is True. If set to @False@, will make a non-TLS connection.
    --
    -- Since 1.0.2
    , tlsClientTLSSettings :: NC.TLSSettings
    -- ^ TLS settings to use. If not provided, defaults will be provided.
    --
    -- Since 1.0.2
    , tlsClientSockSettings :: Maybe NC.SockSettings
    -- ^ Socks configuration; default is @Nothing@. If absent, Socks will not be used.
    --
    -- Since 1.0.2
    , tlsClientConnectionContext :: Maybe NC.ConnectionContext
    -- ^ Connection context. Default is @Nothing@, which will generate a new
    -- context automatically. If you will be making many connections, it's
    -- recommended to call 'NC.initConnectionContext' yourself.
    --
    -- Since 1.0.2
    }

-- | Smart constructor for @TLSClientConfig@.
--
-- Since 1.0.2
tlsClientConfig :: Int -- ^ port
                -> S.ByteString -- ^ host
                -> TLSClientConfig
tlsClientConfig port host = TLSClientConfig
    { tlsClientPort = port
    , tlsClientHost = host
    , tlsClientUseTLS = True
    , tlsClientTLSSettings = def
    , tlsClientSockSettings = Nothing
    , tlsClientConnectionContext = Nothing
    }

-- | Run an application with the given configuration.
--
-- Since 1.0.2
runTLSClient :: (MonadIO m, MonadBaseControl IO m)
             => TLSClientConfig
             -> (AppData -> m a)
             -> m a
runTLSClient TLSClientConfig {..} app = do
    context <- maybe (liftIO NC.initConnectionContext) return tlsClientConnectionContext
    let params = NC.ConnectionParams
            { NC.connectionHostname = S8.unpack tlsClientHost
            , NC.connectionPort = fromIntegral tlsClientPort
            , NC.connectionUseSecure =
                if tlsClientUseTLS
                    then Just tlsClientTLSSettings
                    else Nothing
            , NC.connectionUseSocks = tlsClientSockSettings
            }
    control $ \run -> bracket
        (NC.connectTo context params)
        NC.connectionClose
        (\conn -> run $ app AppData
            { appRead' = NC.connectionGetChunk conn
            , appWrite' = NC.connectionPut conn
            , appSockAddr' = SockAddrInet (fromIntegral tlsClientPort) 0 -- FIXME
            , appLocalAddr' = Nothing
#if MIN_VERSION_streaming_commons(0,1,6)
            , appCloseConnection' = NC.connectionClose conn
#endif
#if MIN_VERSION_streaming_commons(0,1,12)
            , appRawSocket' = Nothing
#endif
            })


-- | Run an application with the given configuration. starting with a clear connection
--   but provide also a call back to trigger a StartTLS handshake on the connection
--
-- Since 1.0.2
runTLSClientStartTLS :: TLSClientConfig
                     -> ApplicationStartTLS
                     -> IO ()
runTLSClientStartTLS TLSClientConfig {..} app = do
    context <- maybe (liftIO NC.initConnectionContext) return tlsClientConnectionContext
    let params = NC.ConnectionParams
            { NC.connectionHostname = S8.unpack tlsClientHost
            , NC.connectionPort = fromIntegral tlsClientPort
            , NC.connectionUseSecure = Nothing
            , NC.connectionUseSocks = tlsClientSockSettings
            }
    control $ \run -> bracket
        (NC.connectTo context params)
        NC.connectionClose
        (\conn -> run $ app (
            AppData
            { appRead' = NC.connectionGetChunk conn
            , appWrite' = NC.connectionPut conn
            , appSockAddr' = SockAddrInet (fromIntegral tlsClientPort) 0 -- FIXME
            , appLocalAddr' = Nothing
#if MIN_VERSION_streaming_commons(0,1,6)
            , appCloseConnection' = NC.connectionClose conn
#endif
#if MIN_VERSION_streaming_commons(0,1,12)
            , appRawSocket' = Nothing
#endif
            }
            , \app' -> do
                 NC.connectionSetSecure context conn tlsClientTLSSettings
                 app' AppData
                   { appRead' = NC.connectionGetChunk conn
                   , appWrite' = NC.connectionPut conn
                   , appSockAddr' = SockAddrInet (fromIntegral tlsClientPort) 0 -- FIXME
                   , appLocalAddr' = Nothing
#if MIN_VERSION_streaming_commons(0,1,6)
                   , appCloseConnection' = NC.connectionClose conn
#endif
#if MIN_VERSION_streaming_commons(0,1,12)
                    , appRawSocket' = Nothing
#endif
                   }
            )
            )


-- | Read from a 'NC.Connection'.
--
-- Since 1.0.2
sourceConnection :: MonadIO m => NC.Connection -> Producer m S.ByteString
sourceConnection conn =
    loop
  where
    loop = do
        bs <- liftIO $ NC.connectionGetChunk conn
        if S.null bs
            then return ()
            else yield bs >> loop

-- | Write to a 'NC.Connection'.
--
-- Since 1.0.2
sinkConnection :: MonadIO m => NC.Connection -> Consumer S.ByteString m ()
sinkConnection conn = awaitForever (liftIO . NC.connectionPut conn)
