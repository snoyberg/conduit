{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Data.Conduit.Network.TLS
    ( TLSConfig
    , tlsConfig
    , tlsHost
    , tlsPort
    , tlsCertificate
    , tlsKey
    , tlsNeedLocalAddr
    , tlsAppData
    , runTCPServerTLS
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
import Data.Conduit.Network (HostPreference, Application, bindPort, sinkSocket, acceptSafe)
import Data.Conduit.Network.Internal (AppData (..))
import Data.Conduit.Network.TLS.Internal
import Data.Conduit (($$), yield)
import qualified Data.Conduit.List as CL
import Data.Either (rights)
import Network.Socket (sClose, getSocketName, SockAddr)
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.TLS.Extra as TLSExtra
#if MIN_VERSION_tls(1, 1, 0)
import Crypto.Random.API (getSystemRandomGen, SystemRandom)
#else
import Crypto.Random (newGenIO, SystemRandom)
#endif
import Network.Socket (Socket)
import qualified Data.ByteString as S

tlsConfig :: HostPreference
          -> Int -- ^ port
          -> FilePath -- ^ certificate
          -> FilePath -- ^ key
          -> TLSConfig
tlsConfig a b c d = TLSConfig a b c d False

runTCPServerTLS :: TLSConfig -> Application IO -> IO ()
runTCPServerTLS TLSConfig{..} app = do
    certs <- readCertificates tlsCertificate
    key <- readPrivateKey tlsKey
    bracket
        (bindPort tlsPort tlsHost)
        sClose
        (forever . serve certs key)
  where
    serve certs key lsocket = do
        (socket, addr) <- acceptSafe lsocket
        mlocal <- if tlsNeedLocalAddr
                    then fmap Just $ getSocketName socket
                    else return Nothing
        _ <- forkIO $ handle socket addr mlocal
        return ()
      where
        handle socket addr mlocal = do
#if MIN_VERSION_tls(1, 1, 0)
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
                (gen :: SystemRandom)
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

            app (tlsAppData ctx addr mlocal) `finally` sClose socket

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

readCertificates :: FilePath -> IO [X509.X509]
readCertificates filepath = do
    certs <- rights . parseCerts . PEM.pemParseBS <$> readFile filepath
    case certs of
        []    -> error "no valid certificate found"
        (_:_) -> return certs
    where parseCerts (Right pems) = map (X509.decodeCertificate . L.fromChunks . (:[]) . PEM.pemContent)
                                  $ filter (flip elem ["CERTIFICATE", "TRUSTED CERTIFICATE"] . PEM.pemName) pems
          parseCerts (Left err) = error $ "cannot parse PEM file: " ++ err

readPrivateKey :: FilePath -> IO TLS.PrivateKey
readPrivateKey filepath = do
    pk <- rights . parseKey . PEM.pemParseBS <$> readFile filepath
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
