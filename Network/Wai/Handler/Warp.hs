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
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network
    ( listenOn, accept, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Control.Exception (bracket, finally, Exception)
import System.IO (Handle, hClose, hFlush)
import System.IO.Error (isEOFError, ioeGetHandle)
import Control.Concurrent (forkIO)
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

import Control.Arrow (first)

import Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import Data.Enumerator.IO (iterHandle, enumHandle)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder (fromByteString, Builder, toLazyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Data.Monoid (mconcat)
import Network.Socket.SendFile (unsafeSendFile)

import Control.Monad.IO.Class (liftIO)

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
    -- FIXME disable buffering
    _ <- forkIO $ serveConnection port app conn remoteHost'
    serveConnections port app socket

serveConnection :: Port -> Application -> Handle -> String -> IO ()
serveConnection port app conn remoteHost' = do
    catch
        (finally
          (E.run_ $ fromClient $$ serveConnection')
          (hClose conn))
        catchEOFError
  where
    fromClient = enumHandle 4096 conn
    serveConnection' = do
        (enumeratee, env) <- parseRequest port remoteHost'
        res <- E.joinI $ enumeratee $$ app env
        keepAlive <- liftIO $ sendResponse env (httpVersion env) conn res
        liftIO $ hFlush conn
        when keepAlive serveConnection'

    catchEOFError :: IOError -> IO ()
    catchEOFError e
        | isEOFError e =
            case ioeGetHandle e of
                Just h -> unless (h == conn) (ioError e)
                Nothing -> ioError e
        | otherwise = ioError e

parseRequest :: Port -> String -> E.Iteratee S.ByteString IO (E.Enumeratee ByteString ByteString IO a, Request)
parseRequest port remoteHost' = do
    headers' <- takeUntilBlank 0 id
    parseRequest' port headers' remoteHost'

-- FIXME come up with good values here
maxHeaders, maxHeaderLength :: Int
maxHeaders = 30
maxHeaderLength = 1024

takeUntilBlank :: Int
               -> ([ByteString] -> [ByteString])
               -> E.Iteratee S.ByteString IO [ByteString]
takeUntilBlank count _
    | count > maxHeaders = E.throwError TooManyHeaders
takeUntilBlank count front = do
    l <- takeLine 0 id
    if B.null l
        then return $ front []
        else takeUntilBlank (count + 1) $ front . (:) l

takeLine :: Int
         -> ([ByteString] -> [ByteString])
         -> E.Iteratee ByteString IO ByteString
takeLine len front = do
    mbs <- E.head
    case mbs of
        Nothing -> E.throwError IncompleteHeaders
        Just bs -> do
            let (x, y) = S.breakByte 10 bs
                x' = if S.length x > 0 && S.last x == 13
                        then S.init x
                        else x
            let len' = len + B.length x
            case () of
                ()
                    | len' > maxHeaderLength -> E.throwError OverLargeHeader
                    | B.null y -> takeLine len' $ front . (:) x
                    | otherwise -> do
                        E.yield () $ E.Chunks [B.drop 1 y]
                        return $ B.concat $ front [x']

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | TooManyHeaders
    | IncompleteHeaders
    | OverLargeHeader
    deriving (Show, Typeable)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Port
              -> [ByteString]
              -> String
              -> E.Iteratee S.ByteString IO (E.Enumeratee S.ByteString S.ByteString IO a, Request)
parseRequest' port lines' remoteHost' = do
    (firstLine, otherLines) <-
        case lines' of
            x:xs -> return (x, xs)
            [] -> E.throwError $ NotEnoughLines $ map B.unpack lines'
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
    return (requestBodyHandle len, Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = B.pack rpath
                , queryString = gets
                , serverName = serverName'
                , serverPort = port
                , requestHeaders = heads
                , isSecure = False
                , errorHandler = System.IO.hPutStr System.IO.stderr
                , remoteHost = B.pack remoteHost'
                })

parseFirst :: ByteString
           -> E.Iteratee S.ByteString IO (ByteString, ByteString, ByteString, HttpVersion)
parseFirst s = do
    let pieces = B.words s
    (method, query, http') <-
        case pieces of
            [x, y, z] -> return (x, y, z)
            _ -> E.throwError $ BadFirstLine $ B.unpack s
    let (hfirst, hsecond) = B.splitAt 5 http'
    unless (hfirst == "HTTP/") $ E.throwError NonHttp
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

hasBody :: Status -> Request -> Bool
hasBody s req = s /= (Status 204 "") && requestMethod req /= "HEAD"

sendResponse :: Request -> HttpVersion -> Handle -> Response -> IO Bool
sendResponse req hv handle (ResponseFile s hs fp) = do
    mapM_ (S.hPut handle) $ L.toChunks $ toLazyByteString $ headers hv s hs False
    if hasBody s req
        then do
            unsafeSendFile handle fp
            return $ lookup "content-length" hs /= Nothing
        else return True
sendResponse req hv handle (ResponseEnumerator res) =
    res go
  where
    go s hs
        | not (hasBody s req) = do
            liftIO $ mapM_ (S.hPut handle)
                   $ L.toChunks $ toLazyByteString
                   $ headers hv s hs False
            return True
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
                else i
        chunk :: E.Enumeratee Builder Builder IO Bool
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

requestBodyHandle :: Int
                  -> E.Enumeratee ByteString ByteString IO a
requestBodyHandle initLen =
    go initLen
  where
    go 0 step = return step
    go len (E.Continue k) = do
        x <- E.head
        case x of
            Nothing -> return $ E.Continue k
            Just bs -> do
                let newlen = max 0 $ len - B.length bs
                k (E.Chunks [bs]) >>== go newlen
    go len step = do
        drain len
        return step
    drain 0 = return ()
    drain len = do
        mbs <- E.head
        case mbs of
            Nothing -> return ()
            Just bs -> do
                let newlen = len - B.length bs
                if newlen <= 0
                    then return ()
                    else drain newlen
