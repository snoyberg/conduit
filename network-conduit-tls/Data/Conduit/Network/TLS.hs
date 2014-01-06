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
    , tlsHost
    , tlsPort
--    , tlsCertificate
--    , tlsKey
    , tlsNeedLocalAddr
    , tlsAppData
    , runTCPServerTLS
      -- * Client
    , TLSClientConfig
    , tlsClientConfig
    , runTLSClient
    , tlsClientPort
    , tlsClientHost
    , tlsClientUseTLS
    , tlsClientTLSSettings
    , tlsClientSockSettings
    , tlsClientConnectionContext
    ) where

import Prelude hiding (FilePath, readFile)
import Data.Aeson (FromJSON (parseJSON), (.:), (.:?), (.!=), Value (Object))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, forever)
import Data.String (fromString)
import Filesystem.Path.CurrentOS ((</>), FilePath)
import Filesystem (readFile)
import qualified Data.ByteString.Lazy as L
import qualified Data.Certificate.KeyRSA as KeyRSA
import qualified Data.PEM as PEM
import qualified Network.TLS as TLS
import qualified Data.Certificate.X509 as X509
import Data.Conduit.Network (HostPreference, Application, bindPort, sinkSocket, acceptSafe, runTCPServerWithHandle, ConnectionHandle(..), serverSettings, sourceSocket)
import Data.Conduit.Network.Internal (AppData (..))
import Data.Conduit.Network.TLS.Internal
import Data.Conduit (($$), yield, awaitForever, Producer, Consumer)
import qualified Data.Conduit.List as CL
import Data.Either (rights)
import Network.Socket (sClose, getSocketName, SockAddr (SockAddrInet))
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Network.TLS.Extra as TLSExtra
#if MIN_VERSION_tls(1, 1, 0)
import Crypto.Random.API (getSystemRandomGen, SystemRandom)
#else
import Crypto.Random (newGenIO, SystemRandom)
#endif
import Network.Socket (Socket)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
#if MIN_VERSION_tls(1, 1, 3)
import qualified Crypto.Random.AESCtr
#endif
import qualified Network.Connection as NC
import Control.Monad.Trans.Control
import Data.Default



makeCertDataPath :: FilePath -> FilePath -> TlsCertData
makeCertDataPath certPath keyPath = TlsCertData (readFile certPath) (readFile keyPath)

makeCertDataBS :: S.ByteString -> S.ByteString -> TlsCertData
makeCertDataBS certBS keyBS = TlsCertData (return certBS) (return keyBS)


tlsConfig :: HostPreference
          -> Int -- ^ port
          -> FilePath -- ^ certificate
          -> FilePath -- ^ key
          -> TLSConfig
tlsConfig a b c d = TLSConfig a b (makeCertDataPath c d) False


-- allow to build a server config directly from bytestring data (if the certifcates and all
-- comes from somewhere else than the filesystem
tlsConfigBS :: HostPreference
            -> Int          -- ^ port 
            -> S.ByteString -- ^ Certificate raw data 
            -> S.ByteString -- ^ Key file raw data 
            -> TLSConfig
tlsConfigBS a b c d = TLSConfig a b (makeCertDataBS c d ) False               


serverHandshake :: Socket -> [X509.X509] -> TLS.PrivateKey -> IO (TLS.Context) 
serverHandshake socket certs key = do
#if MIN_VERSION_tls(1, 1, 3)
    gen <- Crypto.Random.AESCtr.makeSystem
#elif MIN_VERSION_tls(1, 1, 0)
    gen <- getSystemRandomGen
#else
    gen <- newGenIO
#endif

#if MIN_VERSION_tls(1, 0, 0)
    ctx <- TLS.contextNew
           TLS.Backend
                    { TLS.backendFlush = return ()
                    , TLS.backendClose = return ()
                    , TLS.backendSend = sendAll socket
                    , TLS.backendRecv = recvExact socket
                    }
            params
#if MIN_VERSION_tls(1, 1, 3)
            gen
#else
            (gen :: SystemRandom)
#endif
#else
    ctx <- TLS.serverWith
                params
                (gen :: SystemRandom)
                socket
                (return ()) -- flush
                (\bs -> yield bs $$ sinkSocket socket)
                (recvExact socket)
#endif

    TLS.handshake ctx
    return ctx

  where
    params =
#if MIN_VERSION_tls(1, 0, 0)
        TLS.updateServerParams
           (\sp -> sp { TLS.serverWantClientCert = False }) $
           TLS.defaultParamsServer
              { TLS.pAllowedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
              , TLS.pCiphers         = ciphers
              , TLS.pCertificates    = zip certs $ Just key : repeat Nothing
              }
#else
    TLS.defaultParams
            { TLS.pWantClientCert = False
            , TLS.pAllowedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
            , TLS.pCiphers         = ciphers
            , TLS.pCertificates    = zip certs $ Just key : repeat Nothing
            }
#endif

runTCPServerTLS :: TLSConfig -> Application IO -> IO ()
runTCPServerTLS TLSConfig{..} app = do  
    certs <- readCertificates tlsCertData
    key <- readPrivateKey tlsCertData

    runTCPServerWithHandle settings (wrapApp certs key)
    
    where
      -- convert tls settings to regular conduit network ones 
      settings = serverSettings tlsPort tlsHost  -- (const $ return () ) tlsNeedLocalAddr

      wrapApp certs key = ConnectionHandle app'
        where
          app' socket addr mlocal = do
            ctx <- serverHandshake socket certs key
            app (tlsAppData ctx addr mlocal)



data AppDataSTLS m = AppDataSTLS
    { appSSource :: Producer m S.ByteString
    , appSSink :: Consumer S.ByteString m ()
    , appSSockAddr :: SockAddr
    , appSLocalAddr :: Maybe SockAddr
    , startTls :: Application m -> m ()  -- wrap the existing connection with SSL...  
    }

type ApplicationSTLS = AppDataSTLS IO -> IO ()

runTCPServerStartTLS :: TLSConfig -> ApplicationSTLS -> IO ()
runTCPServerStartTLS TLSConfig{..} app = do 
    certs <- readCertificates tlsCertData
    key <- readPrivateKey tlsCertData

    runTCPServerWithHandle settings (wrapApp certs key)
    
    where
      -- convert tls settings to regular conduit network ones 
      settings = serverSettings tlsPort tlsHost  -- (const $ return () ) tlsNeedLocalAddr

      wrapApp certs key = ConnectionHandle clearapp
        where clearapp socket addr mlocal = let 
                startTLSData = AppDataSTLS {
                  appSSource = sourceSocket socket
                  , appSSink = sinkSocket socket
                  , appSSockAddr = addr
                  , appSLocalAddr = mlocal
                  , startTls = \app' -> do
                    ctx <- serverHandshake socket certs key
                    app' (tlsAppData ctx addr mlocal)
                  }
                in
                 app startTLSData

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
           -> AppData IO
tlsAppData ctx addr mlocal = AppData
    { appSource = forever $ lift (TLS.recvData ctx) >>= yield
    , appSink = CL.mapM_ $ TLS.sendData ctx . L.fromChunks . return
    , appSockAddr = addr
    , appLocalAddr = mlocal
    }

-- taken from stunnel example in tls-extra
ciphers :: [TLS.Cipher]
ciphers =
    [ TLSExtra.cipher_AES128_SHA1
    , TLSExtra.cipher_AES256_SHA1
    , TLSExtra.cipher_RC4_128_MD5
    , TLSExtra.cipher_RC4_128_SHA1
    ]

readCertificates :: TlsCertData -> IO [X509.X509]
readCertificates certData = do
    certs <- rights . parseCerts . PEM.pemParseBS <$> getTLSCert certData
    case certs of
        []    -> error "no valid certificate found"
        (_:_) -> return certs
    where parseCerts (Right pems) = map (X509.decodeCertificate . L.fromChunks . (:[]) . PEM.pemContent)
                                  $ filter (flip elem ["CERTIFICATE", "TRUSTED CERTIFICATE"] . PEM.pemName) pems
          parseCerts (Left err) = error $ "cannot parse PEM file: " ++ err

readPrivateKey :: TlsCertData -> IO TLS.PrivateKey
readPrivateKey certData = do
    pk <- rights . parseKey . PEM.pemParseBS <$> getTLSKey certData
    case pk of
        []    -> error "no valid RSA key found"
        (x:_) -> return x

    where parseKey (Right pems) = map (fmap (TLS.PrivRSA . snd) . KeyRSA.decodePrivate . L.fromChunks . (:[]) . PEM.pemContent)
                                $ filter ((== "RSA PRIVATE KEY") . PEM.pemName) pems
          parseKey (Left err) = error $ "Cannot parse PEM file: " ++ err

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
data TLSClientConfig (m :: * -> *) = TLSClientConfig
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
                -> TLSClientConfig m
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
             => TLSClientConfig m
             -> Application m
             -> m ()
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
            { appSource = sourceConnection conn
            , appSink = sinkConnection conn
            , appSockAddr = SockAddrInet (fromIntegral tlsClientPort) 0 -- FIXME
            , appLocalAddr = Nothing
            })

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
