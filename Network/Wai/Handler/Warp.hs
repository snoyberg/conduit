{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Handler.Warp
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- A fast, light-weight HTTP server handler for WAI.
--
---------------------------------------------------------
module Network.Wai.Handler.Warp
    ( run
    , sendResponse
    , parseRequest
    ) where

import Network.Wai
import qualified System.IO

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Network
    ( listenOn, accept, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Control.Exception (bracket, finally, Exception, throwIO)
import System.IO (Handle, hClose, hFlush)
import System.IO.Error (isEOFError, ioeGetHandle)
import Control.Concurrent (forkIO)
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

import Control.Arrow (first)

import Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import Data.Enumerator.IO (iterHandle)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder (fromByteString, Builder, toLazyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Data.Monoid (mconcat)
import Network.Socket.SendFile (unsafeSendFile)

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Internal (defaultChunkSize)

import Data.IORef (IORef, writeIORef, readIORef, newIORef)

run :: Port -> Application -> IO ()
run port = withSocketsDo .
    bracket
        (listenOn $ PortNumber $ fromIntegral port)
        sClose .
        serveConnections port
type Port = Int

serveConnections :: Port -> Application -> Socket -> IO ()
serveConnections port app socket = do
    (conn, remoteHost', _) <- accept socket -- FIXME use sockets directly instead of Handlers?
    _ <- forkIO $ serveConnection port app conn remoteHost'
    serveConnections port app socket

serveConnection :: Port -> Application -> Handle -> String -> IO ()
serveConnection port app conn remoteHost' = do
  catch
    (finally
      serveConnection'
      (hClose conn))
    catchEOFError
  where serveConnection' = do
          (ilen, env) <- parseRequest port conn remoteHost'
          res <- app env
          remaining <- readIORef ilen
          _ <- B.hGet conn remaining -- FIXME just skip, don't read
          keepAlive <- sendResponse (httpVersion env) conn res
          hFlush conn
          when keepAlive serveConnection'

        catchEOFError :: IOError -> IO ()
        catchEOFError e | isEOFError e = case ioeGetHandle e of
                                              Just h  -> unless (h == conn) (ioError e)
                                              Nothing -> ioError e
                        | otherwise = ioError e

parseRequest :: Port -> Handle -> String -> IO (IORef Int, Request)
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
    | BadFirstLine String
    | NonHttp
    deriving (Show, Typeable)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Port
              -> [ByteString]
              -> Handle
              -> String
              -> IO (IORef Int, Request)
parseRequest' port lines' handle remoteHost' = do
    (firstLine, otherLines) <-
        case lines' of
            x:xs -> return (x, xs)
            [] -> throwIO $ NotEnoughLines $ map B.unpack lines'
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let rpath = '/' : case B.unpack rpath' of
                        ('/':x) -> x
                        _ -> B.unpack rpath'
    let heads = map (first mkCIByteString . parseHeaderNoAttr) otherLines
    let host = fromMaybe "" $ lookup "host" heads
    let len = fromMaybe 0 $ do
                bs <- lookup "Content-Length" heads
                let str = B.unpack bs
                case reads str of
                    (x, _):_ -> Just x
                    _ -> Nothing
    let (serverName', _) = B.break (== ':') host
    ilen <- newIORef len
    return (ilen, Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = B.pack rpath
                , queryString = gets
                , serverName = serverName'
                , serverPort = port
                , requestHeaders = heads
                , isSecure = False
                , requestBody = requestBodyHandle handle ilen len
                , errorHandler = System.IO.hPutStr System.IO.stderr
                , remoteHost = B.pack remoteHost'
                })

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

headers :: HttpVersion -> Status -> ResponseHeaders -> Bool -> Builder
headers httpversion status responseHeaders isChunked' = mconcat
    [ fromByteString "HTTP/"
    , fromByteString httpversion
    , fromChar ' '
    , fromString $ show $ statusCode status
    , fromChar ' '
    , fromByteString $ statusMessage status
    , fromByteString "\r\n"
    , mconcat $ map go responseHeaders
    , if isChunked'
        then fromByteString "Transfer-Encoding: chunked\r\n\r\n"
        else fromByteString "\r\n"
    ]
  where
    go (x, y) = mconcat
        [ fromByteString $ ciOriginal x
        , fromByteString ": "
        , fromByteString y
        , fromByteString "\r\n"
        ]

isChunked :: HttpVersion -> Bool
isChunked = (==) http11

sendResponse :: HttpVersion -> Handle -> Response -> IO Bool
sendResponse hv handle (ResponseFile s hs fp) = do
    mapM_ (S.hPut handle) $ L.toChunks $ toLazyByteString $ headers hv s hs False
    unsafeSendFile handle fp
    if lookup "content-length" hs == Nothing
        then return False
        else S.hPut handle "\r\n\r\n" >> return True
sendResponse hv handle (ResponseEnumerator res) =
    res go
  where
    go s hs =
            chunk'
          $ E.enumList 1 [headers hv s hs isChunked']
         $$ E.joinI $ builderToByteString
         $$ (iterHandle handle >> return isKeepAlive)
      where
        hasLength = lookup "content-length" hs /= Nothing
        isChunked' = isChunked hv && not hasLength
        isKeepAlive = isChunked' || hasLength
        chunk' i =
            if isChunked'
                then E.joinI $ chunk $$ i
                else
                    (if isKeepAlive
                        then E.joinI $ after $$ i
                        else i)
        chunk :: E.Enumeratee Builder Builder IO Bool
        chunk = E.checkDone $ E.continue . step
        step k E.EOF = k (E.Chunks [chunkedTransferTerminator]) >>== return
        step k (E.Chunks []) = E.continue $ step k
        step k (E.Chunks builders) =
            k (E.Chunks [chunked]) >>== chunk
          where
            chunked = chunkedTransferEncoding $ mconcat builders
        after = E.checkDone $ E.continue . after'
        after' k E.EOF = k (E.Chunks [fromByteString "\r\n\r\n"]) >>== return
        after' k chunks = k chunks >>== after

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

requestBodyHandle :: Handle
                  -> IORef Int
                  -> Int
                  -> E.Enumerator B.ByteString IO a
requestBodyHandle handle ilen initLen =
    go initLen
  where
    go 0 step = E.returnI step
    go len (E.Continue k) = do
        bs <- liftIO $ B.hGet handle (min len defaultChunkSize) -- FIXME play with defaultChunkSize
        let newLen = len - B.length bs
        liftIO $ writeIORef ilen newLen
        k (E.Chunks [bs]) >>== go newLen
    go _ step = E.returnI step
