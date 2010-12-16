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

import Data.Enumerator (($$), Enumerator, enumList, (>>==))
import qualified Data.Enumerator as E
import Data.Enumerator.IO (iterHandle)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder (fromByteString, Builder)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Data.Monoid (mconcat)

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

headers :: HttpVersion -> Status -> ResponseHeaders -> Builder
headers httpversion status responseHeaders = mconcat
    [ fromByteString "HTTP/"
    , fromByteString httpversion
    , fromChar ' '
    , fromString $ show $ statusCode status
    , fromChar ' '
    , fromByteString $ statusMessage status
    , fromByteString "\r\n"
    , mconcat $ map go responseHeaders
    , fromByteString "Transfer-Encoding: chunked\r\n\r\n"
    ]
  where
    go (x, y) = mconcat
        [ fromByteString $ ciOriginal x
        , fromByteString ": "
        , fromByteString y
        , fromByteString "\r\n"
        ]

sendResponse :: HttpVersion -> Handle -> Response -> IO ()
sendResponse hv handle res = do
    responseEnumerator res $ \s hs ->
        enumList 1 [headers hv s hs]
     $$ E.joinI $ after (enumList 1 [fromByteString "\r\n"])
     $$ E.joinI $ chunk
     $$ E.joinI $ builderToByteString
     $$ iterHandle handle
  where
    chunk :: E.Enumeratee Builder Builder IO ()
    chunk = E.checkDone $ E.continue . step
    step k E.EOF = k (E.Chunks [chunkedTransferTerminator]) >>== return
    step k (E.Chunks []) = E.continue $ step k
    step k (E.Chunks builders) =
        k (E.Chunks [chunked]) >>== chunk
      where
        chunked = chunkedTransferEncoding $ mconcat builders

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

after :: Enumerator Builder IO b -> E.Enumeratee Builder Builder IO b
after enum =
    loop
  where
    loop = E.checkDone $ E.continue . step
    step k E.EOF = enum (E.Continue k) >>== return
    step k s = k s >>== loop
