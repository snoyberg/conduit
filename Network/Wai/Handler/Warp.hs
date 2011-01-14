{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
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

import Prelude hiding (catch)
import Network.Wai
import qualified System.IO

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network
    ( listenOn, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Network.Socket
    ( accept, SockAddr
    )
import qualified Network.Socket.ByteString as Sock
import Control.Exception (bracket, finally, Exception, SomeException, catch)
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

import Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder
    (copyByteString, Builder, toLazyByteString, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Data.Monoid (mconcat, mappend)
import Network.Socket.SendFile (sendFile)

import Control.Monad.IO.Class (liftIO)
import System.Timeout (timeout)
import Data.Word (Word8)

run :: Port -> Application -> IO ()
run port = withSocketsDo .
    bracket
        (listenOn $ PortNumber $ fromIntegral port)
        sClose .
        serveConnections port
type Port = Int

serveConnections :: Port -> Application -> Socket -> IO ()
serveConnections port app socket = do
    (conn, sa) <- accept socket
    _ <- forkIO $ serveConnection port app conn sa
    serveConnections port app socket

serveConnection :: Port -> Application -> Socket -> SockAddr -> IO ()
serveConnection port app conn remoteHost' = do
    catch
        (finally
          (E.run_ $ fromClient $$ serveConnection')
          (sClose conn))
        ignoreAll
  where
    ignoreAll :: SomeException -> IO ()
    ignoreAll _ = return ()
    fromClient = enumSocket bytesPerRead conn
    serveConnection' = do
        (enumeratee, env) <- parseRequest port remoteHost'
        res <- E.joinI $ enumeratee $$ app env
        keepAlive <- liftIO $ sendResponse env (httpVersion env) conn res
        if keepAlive then serveConnection' else return ()

parseRequest :: Port -> SockAddr -> E.Iteratee S.ByteString IO (E.Enumeratee ByteString ByteString IO a, Request)
parseRequest port remoteHost' = do
    headers' <- takeUntilBlank 0 id
    parseRequest' port headers' remoteHost'

-- FIXME come up with good values here
maxHeaders, maxHeaderLength, bytesPerRead, readTimeout :: Int
maxHeaders = 30
maxHeaderLength = 1024
bytesPerRead = 4096
readTimeout = 3000000

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
    | SocketTimeout
    deriving (Show, Typeable)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Port
              -> [ByteString]
              -> SockAddr
              -> E.Iteratee S.ByteString IO (E.Enumeratee S.ByteString S.ByteString IO a, Request)
parseRequest' _ [] _ = E.throwError $ NotEnoughLines []
parseRequest' port (firstLine:otherLines) remoteHost' = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let rpath =
            if S.null rpath'
                then "/"
                else if '/' == B.head rpath'
                        then "/"
                        else B.cons '/' rpath'
    let heads = map parseHeaderNoAttr otherLines
    let host = fromMaybe "" $ lookup "host" heads
    let len =
            case lookup "content-length" heads of
                Nothing -> 0
                Just bs ->
                    case reads $ B.unpack bs of -- FIXME could probably be optimized
                        (x, _):_ -> x
                        [] -> 0
    let serverName' = takeUntil 58 host -- ':'
    return (requestBodyHandle len, Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = rpath
                , queryString = gets
                , serverName = serverName'
                , serverPort = port
                , requestHeaders = heads
                , isSecure = False
                , errorHandler = System.IO.hPutStr System.IO.stderr
                , remoteHost = remoteHost'
                })


takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

parseFirst :: ByteString
           -> E.Iteratee S.ByteString IO (ByteString, ByteString, ByteString, HttpVersion)
parseFirst s = do
    let pieces = S.split 32 s  -- ' '
    (method, query, http') <-
        case pieces of
            [x, y, z] -> return (x, y, z)
            _ -> E.throwError $ BadFirstLine $ B.unpack s
    let (hfirst, hsecond) = B.splitAt 5 http'
    if (hfirst == "HTTP/")
        then
            let (rpath, qstring) = B.break (== '?') query
             in return (method, rpath, qstring, hsecond)
        else E.throwError NonHttp

headers :: HttpVersion -> Status -> ResponseHeaders -> Bool -> Builder
headers httpversion status responseHeaders isChunked' =
    copyByteString "HTTP/"
    `mappend` copyByteString httpversion
    `mappend` fromChar ' '
    `mappend` fromShow (statusCode status)
    `mappend` fromChar ' '
    `mappend` copyByteString (statusMessage status)
    `mappend` copyByteString "\r\n"
    `mappend` mconcat (map go responseHeaders)
    `mappend`
        if isChunked'
            then copyByteString "Transfer-Encoding: chunked\r\n\r\n"
            else copyByteString "\r\n"
  where
    go (x, y) =
        copyByteString (ciOriginal x)
        `mappend` copyByteString ": "
        `mappend` copyByteString y
        `mappend` copyByteString "\r\n"

isChunked :: HttpVersion -> Bool
isChunked = (==) http11

hasBody :: Status -> Request -> Bool
hasBody s req = s /= (Status 204 "") && requestMethod req /= "HEAD"

sendResponse :: Request -> HttpVersion -> Socket -> Response -> IO Bool
sendResponse req hv socket (ResponseFile s hs fp) = do
    Sock.sendMany socket $ L.toChunks $ toLazyByteString $ headers hv s hs False
    if hasBody s req
        then do
            sendFile socket fp
            return $ lookup "content-length" hs /= Nothing
        else return True
sendResponse req hv socket (ResponseBuilder s hs b) = do
    toByteStringIO (Sock.sendAll socket) $
        if hasBody s req
            then b'
            else headers'
    return isKeepAlive
  where
    headers' = headers hv s hs True
    b' =
        if isChunked'
            then headers'
                     `mappend` chunkedTransferEncoding b
                     `mappend` chunkedTransferTerminator
            else headers hv s hs False `mappend` b
    hasLength = lookup "content-length" hs /= Nothing
    isChunked' = isChunked hv && not hasLength
    isKeepAlive = isChunked' || hasLength
sendResponse req hv socket (ResponseEnumerator res) =
    res go
  where
    go s hs
        | not (hasBody s req) = do
            liftIO $ Sock.sendMany socket
                   $ L.toChunks $ toLazyByteString
                   $ headers hv s hs False
            return True
    go s hs =
            chunk'
          $ E.enumList 1 [headers hv s hs isChunked']
         $$ E.joinI $ builderToByteString
         $$ (iterSocket socket >> return isKeepAlive)
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
        -- FIXME ensure that the mconcat on the concents here is unnecessary
        step k x = k x >>== chunk

parseHeaderNoAttr :: ByteString -> (CIByteString, ByteString)
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        restLen = S.length rest
        -- FIXME check for colon without following space?
        rest' = if restLen > 1 && SU.unsafeTake 2 rest == ": "
                    then SU.unsafeDrop 2 rest
                    else rest
     in (mkCIByteString k, rest')

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
                (bs', newlen) <- yieldExtra len bs
                k (E.Chunks [bs']) >>== go newlen
    go len step = do
        drain len
        return step
    drain 0 = return ()
    drain len = do
        mbs <- E.head
        case mbs of
            Nothing -> return ()
            Just bs -> do
                (_, newlen) <- yieldExtra len bs
                drain newlen
    yieldExtra len bs
        | B.length bs == len = return (bs, 0)
        | B.length bs < len = return (bs, len - B.length bs)
        | otherwise = do
            let (x, y) = B.splitAt len bs
            E.yield () $ E.Chunks [y]
            return (x, 0)

iterSocket :: Socket -> E.Iteratee ByteString IO ()
iterSocket socket =
    E.continue go
  where
    go E.EOF = E.yield () E.EOF
    go (E.Chunks cs) = liftIO (Sock.sendMany socket cs) >> E.continue go

enumSocket :: Int -> Socket -> E.Enumerator ByteString IO a
enumSocket len socket (E.Continue k) = do
#if NO_TIMEOUT_PROTECTION
    bs <- liftIO $ Sock.recv socket len
    go bs
#else
    mbs <- liftIO $ timeout readTimeout $ Sock.recv socket len
    case mbs of
        Nothing -> E.throwError SocketTimeout
        Just bs -> go bs
#endif
  where
    go bs
        | S.length bs == 0 = E.continue k
        | otherwise = k (E.Chunks [bs]) >>== enumSocket len socket
enumSocket _ _ step = E.returnI step
