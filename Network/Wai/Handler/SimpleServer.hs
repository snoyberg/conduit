{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , sendResponse
    , parseRequest
    ) where

import Network.Wai
import Network.Wai.Handler.Helper
import qualified System.IO

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network
    ( listenOn, accept, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Control.Exception (bracket, finally, Exception, throwIO)
import System.IO (Handle, hClose)
import Control.Concurrent (forkIO)
import Control.Monad (unless)
import Data.Maybe (isJust, fromJust, fromMaybe)

import Data.Typeable (Typeable)

import Control.Arrow (first)
import Numeric (showHex)

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
            env <- parseRequest port conn remoteHost'
            res <- app env
            sendResponse (httpVersion env) conn res

parseRequest :: Port -> Handle -> String -> IO Request
parseRequest port conn remoteHost' = do
    headers' <- takeUntilBlank conn id
    parseRequest' port headers' conn remoteHost'

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
parseRequest' :: Port
              -> [ByteString]
              -> Handle
              -> String
              -> IO Request
parseRequest' port lines' handle remoteHost' = do
    case lines' of
        (_:_:_) -> return ()
        _ -> throwIO $ NotEnoughLines $ map B.unpack lines'
    (method, rpath', gets, httpversion) <- parseFirst $ head lines'
    let rpath = '/' : case B.unpack rpath' of
                        ('/':x) -> x
                        _ -> B.unpack rpath'
    let heads = map (first mkCIByteString . parseHeaderNoAttr) $ tail lines'
    let host' = lookup "Host" heads
    unless (isJust host') $ throwIO HostNotIncluded
    let host = fromJust host'
    let len = fromMaybe 0 $ do
                bs <- lookup "Content-Length" heads
                let str = B.unpack bs
                case reads str of
                    (x, _):_ -> Just x
                    _ -> Nothing
    let (serverName', _) = B.break (== ':') host
    return $ Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = B.pack rpath
                , queryString = gets
                , serverName = serverName'
                , serverPort = port
                , requestHeaders = heads
                , isSecure = False
                , requestBody = requestBodyHandle handle len
                , errorHandler = System.IO.hPutStr System.IO.stderr
                , remoteHost = B.pack remoteHost'
                }

parseFirst :: ByteString
           -> IO (ByteString, ByteString, ByteString, HttpVersion)
parseFirst s = do
    let pieces = B.words s
    (method, query, http') <-
        case pieces of
            [x, y, z] -> return (x, y, z)
            _ -> throwIO $ BadFirstLine $ B.unpack s
    let (hfirst, hsecond) = B.splitAt 5 http'
    unless (hfirst == B.pack "HTTP/") $ throwIO NonHttp
    let (rpath, qstring) = B.break (== '?') query
    return (method, rpath, qstring, hsecond)

sendResponse :: HttpVersion -> Handle -> Response -> IO ()
sendResponse httpversion h res = do
    B.hPut h $ B.pack "HTTP/"
    B.hPut h $ httpversion
    B.hPut h $ B.pack " "
    B.hPut h $ B.pack $ show $ statusCode $ status res
    B.hPut h $ B.pack " "
    B.hPut h $ statusMessage $ status res
    B.hPut h $ B.pack "\r\n"
    mapM_ putHeader $ responseHeaders res
    B.hPut h $ B.pack "Transfer-Encoding: chunked\r\n\r\n"
    case responseBody res of
        ResponseFile fp -> do
            -- FIXME this is lazy I/O
            lbs <- L.readFile fp
            mapM_ myPut $ L.toChunks lbs
        ResponseEnumerator (Enumerator enum) ->
            enum (const myPut) h >> return ()
        ResponseLBS lbs -> mapM_ myPut $ L.toChunks lbs
    B.hPut h $ B.pack "0\r\n\r\n"
    where
        myPut bs = do
            B.hPut h $ B.pack $ showHex (B.length bs) " \r\n"
            B.hPut h bs
            B.hPut h $ B.pack "\r\n"
            return (Right h)
        putHeader (x, y) = do
            B.hPut h $ ciOriginal x
            B.hPut h $ B.pack ": "
            B.hPut h y
            B.hPut h $ B.pack "\r\n"

parseHeaderNoAttr :: ByteString -> (ByteString, ByteString)
parseHeaderNoAttr s =
    let (k, rest) = B.span (/= ':') s
        rest' = if not (B.null rest) &&
                   B.head rest == ':' &&
                   not (B.null $ B.tail rest) &&
                   B.head (B.tail rest) == ' '
                    then B.drop 2 rest
                    else rest
     in (k, rest')
