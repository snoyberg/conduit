{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.HUnit
import Data.Conduit
import Data.Conduit.Network (appSource, appSink)
import Data.Conduit.Network.TLS
import Control.Concurrent (forkIO, threadDelay, killThread)
import qualified Network.Connection as NC
import qualified Data.ByteString as BS

#if MIN_VERSION_crypton_connection(0,4,0)
import qualified Data.Default.Class
#endif

testKeyRaw :: BS.ByteString
testKeyRaw = "-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEAwAU371YZKOVON+S/TpNERcSbe5vWk0kdodR/cC7iwQ40ukO6\nIH7H40THVAWQwlD6kasRdsxcsk+KcOpoBgivw9izJ7ggBp7reFe8mJRp2qMGyK+n89ZRHNlVWl1qSAC/o0A1ldvyfZ2X4nNYHVAFqwhPSsFTxQgxORJbL7qdKy1tirqg\nWpHQMgK6dQJjOEEhrMKmOC2q6l9vbTYuAghDsdtbbEc8FWWVeExiIj8RopPY9+if\nj3BoXxp4WhfiDWmnnBWp71oJIfB1uziLV6PJdA1nKfVbPUeAM0wCFFUCbrjaxdg3\n4RenckCZIJwDo+ff/OSpKynrwznunZW847m2lwIDAQABAoIBAEqjPKS5MLpmt0qe\njYX7VDRSQaWAY52IdA4tTQPMFbO40+H65WQwI35Bg8EzEJuXYm4wsm8c7IMay9Ms\nKhb+VWOo3ap4tWodZ6W1ZMdiGOs1JzPmoz/ydEDkcXrYiLFIKTVJhgqkHdOZ6CnL\nb9qk+i8K4ddK4kbZ8lgevHcG8ISRTV2B8dRc3iohGJ0F6VlL62GnjbExjegsUs4N\n4Ozy8xI4oxlKdZcgutBkfPqdJOWixWPnMXf0PtJVFMzKzVujZlupoonqUUGn51c6\nTVVXAh1pcF0XrmKNscuODFMwBtVfIrfNf/iL1KvIIlKFbUSb/Yu9/9KBvLmfKdxf\nyrtvNBECgYEA5rRdd8IaskROgQxRTJagZn39Sl6oBVFLQ+fy0LGXV3bDbgl7myx8\nOtkKiTMHGT8g6JWv5NMWUgGSZBkMnZSQ/QCtbCxpuDjajxY2GVKU+1EbJjPccuWH\nTnopBuss6WiDbI/Jl9JjPBmhs8EsuAgAOo9yPzgs6SLiMfUwWKkPRdUCgYEA1RMH\nhhKUULqE+/xF214aUqcIk38BCw9g9Uo0pGp4cIfA8iuRachZGsbRpDQyaGRWL+4A\n9hOLPdV2ey6TvNcP/7H6dXrvj4TXLqrxPC2ne2zawqeCkqigxq8Rk55pBF5c52Xz\nX5Rie98TC++gf+fyUTIUS4OqMLg4q1Erk23g5LsCgYEApZg3MtvXj7ep5cUyodfI\nYGj0oyoYTmDQtnhJ+PRQHk637kbOO06OCSt6/YnsAXono+q1q3i8n7ZTHphATuex\nvnh7ApdKdxoP/v7BbCGzoETSSPSWur34BiN3SWkK/qqvEwCOgfRYmG4JfF4fPCU6\nDM6kAa7PxbPtSlClGC6ZMNUCgYEAwp+tIaPa4ZpdWiXmUSe1d4Wm6cL6WvXjJGpx\nhzTRakg1z35IRo2ABltQpmIfIQd1SjZlnl/fsc1HeeDjhXwT2wTgt2phY4B9ZN0z\nmDpDXxPhBigntnpc0N6ceXAakKj4x0xybv2Er4zlQuPQgMSGq+/IZemQDQxYhvOP\nkAyvfX0CgYBEVKvhcXQ9ETmEsk0FxPvpS9CtWXaNWItVzC/z3+mrU2B5JPcBQF72\nBsuoupeq52S+SGH7el5Xp2AoLXjZYsQ9S0t76p6G3lE/cHmnc/QNt4kT6oe5mpv1\nYXIo3/044Cbw2FEkEaj0iucagYCoqhlZTFN8aR6dXFTmvU+k6VP7pg==\n-----END RSA PRIVATE KEY-----"


-- self signed certificate corresponding to private key above.
-- this certificate will expire circa january 2015 ... 
testCertificateRaw :: BS.ByteString
testCertificateRaw = "-----BEGIN CERTIFICATE-----\nMIIDBjCCAe4CCQDBE77UEng3SDANBgkqhkiG9w0BAQsFADBFMQswCQYDVQQGEwJG\nUjETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0\ncyBQdHkgTHRkMB4XDTE0MDEwNjIxNTA1OVoXDTE1MDEwNjIxNTA1OVowRTELMAkG\nA1UEBhMCRlIxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoMGEludGVybmV0\nIFdpZGdpdHMgUHR5IEx0ZDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB\nAMAFN+9WGSjlTjfkv06TREXEm3ub1pNJHaHUf3Au4sEONLpDuiB+x+NEx1QFkMJQ\n+pGrEXbMXLJPinDqaAYIr8PYsye4IAae63hXvJiUadqjBsivp/PWURzZVVpdakgA\nv6NANZXb8n2dl+JzWB1QBasIT0rBU8UIMTkSWy+6nSstbYq6oFqR0DICunUCYzhB\nIazCpjgtqupfb202LgIIQ7HbW2xHPBVllXhMYiI/EaKT2Pfon49waF8aeFoX4g1p\np5wVqe9aCSHwdbs4i1ejyXQNZyn1Wz1HgDNMAhRVAm642sXYN+EXp3JAmSCcA6Pn\n3/zkqSsp68M57p2VvOO5tpcCAwEAATANBgkqhkiG9w0BAQsFAAOCAQEAq1Vy0VBj\nKxuXrpzU8O8bMNrH571Mtjb7tNAhpv77HeyfssW151Rltn71DDPIOqwhoA9zN47I\ns/t/aq1+BmXSdEEb9chbOkZ+KOsJlG/Y0Io4jSK4j4JHlnSBhjItTaoEkkvQtr45\nbyrLYSeixGY5JZd8hIOUcGuru+PPx+SKtuZrnxHF+oXyT9O4BLIe9BYWHvE0Qpop\nvc060w8CIDW4gfYcxxMsA45IrULv5mq2J8bLAtcI9hQY3Z8dPNejsChYTHK6JDEL\n7/G6POAMxenO5cg+Y6Y3OKp5+LrzJNIwfnAnLLFl+/Gb2kC+GcfwZDojuiCJ9iIG\njPwFEAl/7WuMlg==\n-----END CERTIFICATE-----"


serverConfig :: TLSConfig
serverConfig = tlsConfigBS "*4" 4242 testCertificateRaw testKeyRaw               

clientConfig :: TLSClientConfig
clientConfig = tlsClientConfig 4242 "127.0.0.1"

clientConfigNoCA :: TLSClientConfig
clientConfigNoCA = clientConfig
  { tlsClientTLSSettings = NC.TLSSettingsSimple True False False
#if MIN_VERSION_crypton_connection(0,4,0)
      Data.Default.Class.def
#endif
  }

testSimpleServerClient :: IO ()
testSimpleServerClient = do
    -- a simple server that says hello over tls 
    serverThreadId <- forkIO $ runTCPServerTLS serverConfig $ \ad ->
      runConduit $ yield "hello world" .| appSink ad
      
    -- wait for server to be ready 
    threadDelay 1000000
    
    -- default settings checks CA, the test cert is self-signed. should
    runTLSClient clientConfigNoCA $ \ad -> do
      d <- runConduit $ appSource ad .| (await >>= return)
      assertEqual "client receives hello world" (Just "hello world") d
      
    -- kill the server 
    killThread serverThreadId


testSimpleServerClientStartTLS :: IO ()
testSimpleServerClientStartTLS = do
  serverThreadId <- forkIO $ runTCPServerStartTLS serverConfig serve
  threadDelay 100000

  runTLSClientStartTLS clientConfigNoCA client

  killThread serverThreadId

  where
    serve (ad, startTls) = do
      runConduit $ yield "proceed" .| appSink ad
      startTls $ \app -> runConduit $ (yield "crypted") .| appSink app


    client (ad, startTls) = do
      -- reads one message from server
      msg <- runConduit $ appSource ad .| (await >>= return)
      assertEqual "server sends proceed" (Just "proceed") msg
      startTls $ \app -> do
        msgTls <- runConduit $ appSource app .| (await >>= return)
        assertEqual "server sends crypted" (Just "crypted") msgTls


main :: IO (Counts)
main = runTestTT $ TestList [ TestLabel "TLS Server" $ TestCase testSimpleServerClient
                            , TestLabel "StartTLS" $ TestCase testSimpleServerClientStartTLS ]
        
    

  
  

