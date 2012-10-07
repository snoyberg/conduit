{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Conduit.Network.TLS
    ( TLSConfig (..)
    , runTCPServerTLS
    ) where

import Prelude hiding (FilePath, readFile)
import Data.Yaml (FromJSON (parseJSON), (.:), (.:?), (.!=), Value (Object))
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
import Data.Conduit (($$), yield)
import qualified Data.Conduit.List as CL
import Data.Either (rights)
import Network.Socket (sClose, getSocketName)
import Network.Socket.ByteString (recv)
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.TLS.Extra as TLSExtra
import Crypto.Random (newGenIO, SystemRandom)

data TLSConfig = TLSConfig
    { tlsHost :: HostPreference
    , tlsPort :: Int
    , tlsCertificate :: FilePath
    , tlsKey :: FilePath
    , tlsNeedLocalAddr :: Bool
    }

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
            gen <- newGenIO
            ctx <- TLS.serverWith
                params
                (gen :: SystemRandom)
                socket
                (return ()) -- flush
                (\bs -> yield bs $$ sinkSocket socket)
                (recv socket)

            TLS.handshake ctx

            let ad = AppData
                    { appSource =
                        let src = lift (TLS.recvData ctx) >>= yield >> src
                         in src
                    , appSink = CL.mapM_ $ TLS.sendData ctx . L.fromChunks . return
                    , appSockAddr = addr
                    , appLocalAddr = mlocal
                    }


            app ad `finally` sClose socket

        params = TLS.defaultParams
            { TLS.pWantClientCert = False
            , TLS.pAllowedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
            , TLS.pCiphers         = ciphers
            , TLS.pCertificates    = zip certs $ Just key : repeat Nothing
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
