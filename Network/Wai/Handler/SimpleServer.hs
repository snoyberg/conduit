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
import Network.Wai.Handler.Helper
import qualified System.IO

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Network
    ( listenOn, accept, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Control.Exception (bracket, finally, Exception)
import System.IO (Handle, hClose)
import Control.Concurrent (forkIO)
import Control.Monad (unless)
import Data.Maybe (isJust, fromJust, fromMaybe)

import Control.Failure
import Data.Typeable (Typeable)

import qualified Web.Encodings.StringLike as SL

import qualified Safe
import Network.Socket.SendFile
import Control.Arrow (first)

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
            sendResponse (httpVersion env) conn res

hParseRequest :: Port -> Handle -> String -> IO Request
hParseRequest port conn remoteHost' = do
    headers' <- takeUntilBlank conn id
    parseRequest port headers' conn remoteHost'

takeUntilBlank :: Handle
               -> ([ByteString] -> [ByteString])
               -> IO [ByteString]
takeUntilBlank h front = do
    l <- stripCR `fmap` B.hGetLine h
    if B.null l
        then return $ front []
        else takeUntilBlank h $ front . (:) l

stripCR :: ByteString -> ByteString
stripCR bs
    | B.null bs = bs
    | B.last bs == '\r' = B.init bs
    | otherwise = bs

data InvalidRequest =
    NotEnoughLines [String]
    | HostNotIncluded
    | BadFirstLine String
    | NonHttp
    deriving (Show, Typeable)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest :: Port
             -> [ByteString]
             -> Handle
             -> String
             -> IO Request
parseRequest port lines' handle remoteHost' = do
    case lines' of
        (_:_:_) -> return ()
        _ -> failure $ NotEnoughLines $ map B.unpack lines'
    (method', rpath', gets, httpversion) <- parseFirst $ head lines'
    let method = methodFromBS method'
    let rpath = '/' : case B.unpack rpath' of
                        ('/':x) -> x
                        _ -> B.unpack rpath'
    let heads = map (first requestHeaderFromBS . parseHeaderNoAttr)
              $ tail lines'
    let host' = lookup Host heads
    unless (isJust host') $ failure HostNotIncluded
    let host = fromJust host'
    let len = fromMaybe 0 $ do
                bs <- lookup ReqContentLength heads
                let str = B.unpack bs
                Safe.readMay str
    let (serverName', _) = B.break (== ':') host
    return $ Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = B.pack rpath
                , queryString = gets
                , serverName = serverName'
                , serverPort = port
                , requestHeaders = heads
                , urlScheme = HTTP
                , requestBody = requestBodyHandle handle len
                , errorHandler = System.IO.hPutStr System.IO.stderr
                , remoteHost = B.pack remoteHost'
                }

parseFirst :: ByteString
           -> IO (ByteString, ByteString, ByteString, HttpVersion)
parseFirst s = do
    let pieces = SL.split ' ' s
    (method, query, http') <-
        case pieces of
            [x, y, z] -> return (x, y, z)
            _ -> failure $ BadFirstLine $ B.unpack s
    let (hfirst, hsecond) = B.splitAt 5 http'
    unless (hfirst == B.pack "HTTP/") $ failure NonHttp
    let (rpath, qstring) = B.break (== '?') query
    return (method, rpath, qstring, httpVersionFromBS hsecond)

sendResponse :: HttpVersion -> Handle -> Response -> IO ()
sendResponse httpversion h res = do
    B.hPut h $ B.pack "HTTP/"
    B.hPut h $ httpVersionToBS httpversion
    B.hPut h $ B.pack " "
    B.hPut h $ B.pack $ show $ statusCode $ status res
    B.hPut h $ B.pack " "
    B.hPut h $ statusMessage $ status res
    B.hPut h $ B.pack "\r\n"
    mapM_ putHeader $ responseHeaders res
    B.hPut h $ B.pack "\r\n"
    case responseBody res of
        Left fp -> unsafeSendFile h fp
        Right (Enumerator enum) -> enum myPut h >> return ()
    where
        myPut _ bs = do
            B.hPut h bs
            return (Right h)
        putHeader (x, y) = do
            B.hPut h $ responseHeaderToBS x
            B.hPut h $ B.pack ": "
            B.hPut h y
            B.hPut h $ B.pack "\r\n"

parseHeaderNoAttr :: ByteString -> (ByteString, ByteString)
parseHeaderNoAttr s =
    let (k, rest) = B.span (/= ':') s
     in (k, SL.dropPrefix' (B.pack ": ") rest)
