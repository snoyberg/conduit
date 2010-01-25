{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Handler.SimpleServer
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- A simplistic HTTP server handler for Wai.
--
---------------------------------------------------------
module Network.Wai.Handler.SimpleServer
    ( run
    ) where

import Network.Wai
import qualified System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Network
    ( listenOn, accept, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Control.Exception (bracket, finally, Exception)
import System.IO (Handle, hClose)
import Control.Concurrent
import Control.Monad (unless)
import Data.Maybe (isJust, fromJust, fromMaybe)

import Control.Failure
import Data.Typeable (Typeable)

import Web.Encodings.StringLike (StringLike)
import qualified Web.Encodings.StringLike as SL

import qualified Safe
import Network.Socket.SendFile

run :: Port -> Application -> IO ()
run port = withSocketsDo .
    bracket
        (listenOn $ PortNumber $ fromIntegral port)
        sClose .
        serveConnections port
type Port = Int

serveConnections :: Port -> Application -> Socket -> IO ()
serveConnections port app socket = do
    (conn, remoteHost', _) <- accept socket
    _ <- forkIO $ serveConnection port app conn remoteHost'
    serveConnections port app socket

serveConnection :: Port -> Application -> Handle -> String -> IO ()
serveConnection port app conn remoteHost' =
    finally
        serveConnection'
        (hClose conn)
    where
        serveConnection' = do
            env <- hParseRequest port conn remoteHost'
            res <- app env
            sendResponse conn res

hParseRequest :: Port -> Handle -> String -> IO Request
hParseRequest port conn remoteHost' = do
    headers' <- takeUntilBlank conn id
    parseRequest port headers' conn remoteHost'

takeUntilBlank :: Handle
               -> ([BS.ByteString] -> [BS.ByteString])
               -> IO [BS.ByteString]
takeUntilBlank h front = do
    l <- stripCR `fmap` BS.hGetLine h
    if BS.null l
        then return $ front []
        else takeUntilBlank h $ front . (:) l

stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
    | BS.null bs = bs
    | B8.last bs == '\r' = BS.init bs
    | otherwise = bs

data InvalidRequest =
    NotEnoughLines [String]
    | HostNotIncluded
    | BadFirstLine String
    | NonHttp11
    deriving (Show, Typeable)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest :: Port
             -> [BS.ByteString]
             -> Handle
             -> String
             -> IO Request
parseRequest port lines' handle remoteHost' = do
    case lines' of
        (_:_:_) -> return ()
        _ -> failure $ NotEnoughLines $ map SL.unpack lines'
    (method', rpath', gets) <- parseFirst $ head lines'
    let method = methodFromBS method'
    let rpath = '/' : case SL.unpack rpath' of
                        ('/':x) -> x
                        _ -> SL.unpack rpath'
    let heads = map parseHeaderNoAttr $ tail lines'
    let host' = lookup (SL.pack "Host") heads
    unless (isJust host') $ failure HostNotIncluded
    let host = fromJust host'
    let len = fromMaybe 0 $ do
                bs <- lookup (SL.pack "Content-Length") heads
                let str = SL.unpack bs
                Safe.readMay str
    mlen <- newMVar len
    let (serverName', _) = SL.breakChar ':' host
    return $ Request
                { requestMethod = method
                , httpVersion = Http11
                , pathInfo = SL.pack rpath
                , queryString = gets
                , serverName = serverName'
                , serverPort = port
                , httpHeaders = heads
                , urlScheme = HTTP
                , requestBody = requestBodyHandle handle mlen
                , errorHandler = System.IO.hPutStr System.IO.stderr
                , remoteHost = remoteHost'
                }

requestBodyHandle :: Handle -> MVar Int -> IO (Maybe BS.ByteString)
requestBodyHandle h mlen = modifyMVar mlen helper where
    helper :: Int -> IO (Int, Maybe BS.ByteString)
    helper 0 = return (0, Nothing)
    helper len = do
        bs <- BS.hGet h len
        let newLen = len - BS.length bs
        return (newLen, Just bs)

parseFirst :: (StringLike s, MonadFailure InvalidRequest m) =>
              s
           -> m (s, s, s)
parseFirst s = do
    let pieces = SL.split ' ' s
    (method, query, http') <-
        case pieces of
            [x, y, z] -> return (x, y, z)
            _ -> failure $ BadFirstLine $ SL.unpack s
    unless (http' == SL.pack "HTTP/1.1") $ failure NonHttp11
    let (rpath, qstring) = SL.breakChar '?' query
    return (method, rpath, qstring)

sendResponse :: Handle -> Response -> IO ()
sendResponse h res = do
    BS.hPut h $ SL.pack "HTTP/1.1 "
    BS.hPut h $ SL.pack $ show $ statusCode $ status res
    BS.hPut h $ statusMessage $ status res
    BS.hPut h $ SL.pack "\r\n"
    mapM_ putHeader $ headers res
    BS.hPut h $ SL.pack "\r\n"
    case body res of
        Left fp -> unsafeSendFile h fp
        Right enum -> enum $ BS.hPut h
    where
        putHeader (x, y) = do
            BS.hPut h x
            BS.hPut h $ SL.pack ": "
            BS.hPut h y
            BS.hPut h $ SL.pack "\r\n"

parseHeaderNoAttr :: StringLike a => a -> (a, a)
parseHeaderNoAttr s =
    let (k, rest) = SL.span (/= ':') s
     in (k, SL.dropPrefix' (SL.pack ": ") rest)
