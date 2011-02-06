{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
---------------------------------------------------------
--
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

-- | A fast, light-weight HTTP server handler for WAI. Some random notes (a FAQ, if you will):
--
-- * When a 'ResponseFile' indicates a file which does not exist, an exception
--   is thrown. This will close the connection to the client as well. You should
--   handle file existance checks at the application level.
module Network.Wai.Handler.Warp
    ( -- * Run a Warp server
      run
    , runEx
    , serveConnections
      -- * Run a Warp server with full settings control
    , runSettings
    , Settings
    , defaultSettings
    , settingsPort
    , settingsOnException
    , settingsTimeout
      -- * Datatypes
    , Port
    , InvalidRequest (..)
      -- * Utility functions for other packages
    , sendResponse
    , parseRequest
#if TEST
    , takeLineMax
    , takeHeaders
#endif
    ) where

import Prelude hiding (catch)
import Network.Wai
import qualified System.IO

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network (listenOn, sClose, PortID(PortNumber), Socket)
import Network.Socket
    ( accept, SockAddr
    )
import qualified Network.Socket.ByteString as Sock
import Control.Exception
    ( bracket, finally, Exception, SomeException, catch
    , fromException
    )
import Control.Concurrent (forkIO, threadWaitWrite)
import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

import Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder
    (copyByteString, Builder, toLazyByteString, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Data.Monoid (mappend, mconcat)
import Network.Socket.SendFile (sendFileIterWith, Iter (..))

import Control.Monad.IO.Class (liftIO)
import qualified Timeout as T
import Data.Word (Word8)
import Data.List (foldl')
import Control.Monad (forever)

#if WINDOWS
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MV
import Network.Socket (withSocketsDo)
#endif

-- | Run an 'Application' on the given port, ignoring all exceptions.
run :: Port -> Application -> IO ()
run = runEx (const $ return ())

-- | Run an 'Application' on the given port, with the given exception handler.
-- Please note that you will also receive 'InvalidRequest' exceptions.
runEx :: (SomeException -> IO ()) -> Port -> Application -> IO ()
runEx onE port = runSettings Settings
    { settingsPort = port
    , settingsOnException = onE
    , settingsTimeout = 30
    }

runSettings :: Settings -> Application -> IO ()
#if WINDOWS
runSettings set app = withSocketsDo $ do
    var <- MV.newMVar Nothing
    let clean = MV.modifyMVar_ var $ \s -> maybe (return ()) sClose s >> return Nothing
    _ <- forkIO $ bracket
        (listenOn $ PortNumber $ fromIntegral $ settingsPort set)
        (const clean)
        (\s -> do
            MV.modifyMVar_ var (\_ -> return $ Just s)
            serveConnections' set app s)
    forever (threadDelay maxBound) `finally` clean
#else
runSettings set =
    bracket
        (listenOn $ PortNumber $ fromIntegral $ settingsPort set)
        sClose .
        serveConnections' set
#endif

type Port = Int

-- | Runs a server, listening on the given socket. The user is responsible for
-- closing the socket after 'runWithSocket' completes. You must also supply a
-- 'Port' argument for use in the 'serverPort' record; however, this field is
-- only used for informational purposes. If you are in fact listening on a
-- non-TCP socket, this can be a ficticious value.
serveConnections :: (SomeException -> IO ())
                 -> Port -> Application -> Socket -> IO ()
serveConnections onE port = serveConnections' defaultSettings
    { settingsOnException = onE
    , settingsPort = port
    }

serveConnections' :: Settings
                  -> Application -> Socket -> IO ()
serveConnections' set app socket = do
    let onE = settingsOnException set
        port = settingsPort set
    tm <- T.initialize $ settingsTimeout set * 1000000
    forever $ do
        (conn, sa) <- accept socket
        _ <- forkIO $ do
            th <- T.register tm $ sClose conn
            serveConnection th onE port app conn sa
            T.cancel th
        return ()

serveConnection :: T.Handle
                -> (SomeException -> IO ())
                -> Port -> Application -> Socket -> SockAddr -> IO ()
serveConnection th onException port app conn remoteHost' = do
    catch
        (finally
          (E.run_ $ fromClient $$ serveConnection')
          (sClose conn))
        onException
  where
    fromClient = enumSocket th bytesPerRead conn
    serveConnection' = do
        (enumeratee, env) <- parseRequest port remoteHost'
        -- Let the application run for as long as it wants
        liftIO $ T.pause th
        res <- E.joinI $ enumeratee $$ app env
        liftIO $ T.resume th
        keepAlive <- liftIO $ sendResponse th env (httpVersion env) conn res
        if keepAlive then serveConnection' else return ()

parseRequest :: Port -> SockAddr -> E.Iteratee S.ByteString IO (E.Enumeratee ByteString ByteString IO a, Request)
parseRequest port remoteHost' = do
    headers' <- takeHeaders
    parseRequest' port headers' remoteHost'

-- FIXME come up with good values here
maxHeaders, maxHeaderLength, bytesPerRead :: Int
maxHeaders = 30
maxHeaderLength = 1024
bytesPerRead = 4096

sendFileCount :: Integer
sendFileCount = 65536

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | TooManyHeaders
    | IncompleteHeaders
    | OverLargeHeader
    | SocketTimeout
    deriving (Show, Typeable, Eq)
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
                else rpath'
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
    -- FIXME isolate takes an Integer instead of Int or Int64. If this is a
    -- performance penalty, we may need our own version.
    return (EB.isolate len, Request
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
{-# INLINE parseFirst #-} -- FIXME is this inline necessary? the function is only called from one place and not exported

httpBuilder, spaceBuilder, newlineBuilder, transferEncodingBuilder
           , colonSpaceBuilder :: Builder
httpBuilder = copyByteString "HTTP/"
spaceBuilder = fromChar ' '
newlineBuilder = copyByteString "\r\n"
transferEncodingBuilder = copyByteString "Transfer-Encoding: chunked\r\n\r\n"
colonSpaceBuilder = copyByteString ": "

headers :: HttpVersion -> Status -> ResponseHeaders -> Bool -> Builder
headers !httpversion !status !responseHeaders !isChunked' = {-# SCC "headers" #-}
    let !start = httpBuilder
                `mappend` copyByteString httpversion
                `mappend` spaceBuilder
                `mappend` fromShow (statusCode status)
                `mappend` spaceBuilder
                `mappend` copyByteString (statusMessage status)
                `mappend` newlineBuilder
        !start' = foldl' responseHeaderToBuilder start responseHeaders
        !end = if isChunked'
                 then transferEncodingBuilder
                 else newlineBuilder
    in start' `mappend` end

responseHeaderToBuilder :: Builder -> (CIByteString, ByteString) -> Builder
responseHeaderToBuilder b (x, y) = b
  `mappend` (copyByteString $ ciOriginal x)
  `mappend` colonSpaceBuilder
  `mappend` copyByteString y
  `mappend` newlineBuilder

isChunked :: HttpVersion -> Bool
isChunked = (==) http11

hasBody :: Status -> Request -> Bool
hasBody s req = s /= (Status 204 "") && requestMethod req /= "HEAD"

sendResponse :: T.Handle
             -> Request -> HttpVersion -> Socket -> Response -> IO Bool
sendResponse th req hv socket (ResponseFile s hs fp) = do
    Sock.sendMany socket $ L.toChunks $ toLazyByteString $ headers hv s hs False
    if hasBody s req
        then do
            sendFileIterWith tickler socket fp sendFileCount
            return $ lookup "content-length" hs /= Nothing
        else return True
  where
    tickler iter = do
        r <- iter
        case r of
            Done _ -> return ()
            Sent _ cont -> T.tickle th >> tickler cont
            WouldBlock _ fd cont -> do
                -- FIXME do we want to tickle here?
                threadWaitWrite fd
                tickler cont
sendResponse th req hv socket (ResponseBuilder s hs b)
    | hasBody s req = do
          toByteStringIO (\bs -> do
            Sock.sendAll socket bs
            T.tickle th) b'
          return isKeepAlive
    | otherwise = do
        Sock.sendMany socket
            $ L.toChunks
            $ toLazyByteString
            $ headers hv s hs False
        T.tickle th
        return True
  where
    headers' = headers hv s hs isChunked'
    b' =
        if isChunked'
            then headers'
                 `mappend` chunkedTransferEncoding b
                 `mappend` chunkedTransferTerminator
            else headers hv s hs False `mappend` b
    hasLength = lookup "content-length" hs /= Nothing
    isChunked' = isChunked hv && not hasLength
    isKeepAlive = isChunked' || hasLength
sendResponse th req hv socket (ResponseEnumerator res) =
    res go
  where
    -- FIXME perhaps alloca a buffer per thread and reuse that in all functiosn below. Should lessen greatly the GC burden (I hope)
    go s hs
        | not (hasBody s req) = do
            liftIO $ Sock.sendMany socket
                   $ L.toChunks $ toLazyByteString
                   $ headers hv s hs False
            return True
    go s hs =
            chunk'
          $ E.enumList 1 [headers hv s hs isChunked']
         $$ E.joinI $ builderToByteString -- FIXME unsafeBuilderToByteString
         $$ (iterSocket th socket >> return isKeepAlive)
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
        step k (E.Chunks [x]) = k (E.Chunks [chunkedTransferEncoding x]) >>== chunk
        step k (E.Chunks xs) = k (E.Chunks [chunkedTransferEncoding $ mconcat xs]) >>== chunk

parseHeaderNoAttr :: ByteString -> (CIByteString, ByteString)
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        restLen = S.length rest
        -- FIXME check for colon without following space?
        rest' = if restLen > 1 && SU.unsafeTake 2 rest == ": "
                    then SU.unsafeDrop 2 rest
                    else rest
     in (mkCIByteString k, rest')

enumSocket :: T.Handle -> Int -> Socket -> E.Enumerator ByteString IO a
enumSocket th len socket =
    inner
  where
    inner (E.Continue k) = do
        bs <- liftIO $ Sock.recv socket len
        liftIO $ T.tickle th
        if S.null bs
            then E.throwError SocketTimeout
            else go k bs
    inner step = E.returnI step
    go k bs
        | S.length bs == 0 = E.continue k
        | otherwise = k (E.Chunks [bs]) >>== enumSocket th len socket

------ The functions below are not warp-specific and could be split out into a
--separate package.

takeHeaders :: E.Iteratee ByteString IO [ByteString]
takeHeaders = takeUntilBlank 0 id

takeUntilBlank :: Int
               -> ([ByteString] -> [ByteString])
               -> E.Iteratee S.ByteString IO [ByteString]
takeUntilBlank count _
    | count > maxHeaders = E.throwError TooManyHeaders
takeUntilBlank count front = do
    l <- takeLineMax 0 id
    if B.null l
        then return $ front []
        else takeUntilBlank (count + 1) $ front . (:) l

takeLineMax :: Int
            -> ([ByteString] -> [ByteString])
            -> E.Iteratee ByteString IO ByteString
takeLineMax len front = do
    mbs <- EL.head
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
                    | B.null y -> takeLineMax len' $ front . (:) x
                    | otherwise -> do
                        E.yield () $ E.Chunks [B.drop 1 y]
                        return $ B.concat $ front [x']

iterSocket :: T.Handle
           -> Socket
           -> E.Iteratee B.ByteString IO ()
iterSocket th sock =
    E.continue step
  where
    step E.EOF = E.yield () E.EOF
    step (E.Chunks []) = E.continue step
    step (E.Chunks xs) = do
        liftIO $ Sock.sendMany sock xs
        liftIO $ T.tickle th
        E.continue step

data Settings = Settings
    { settingsPort :: Int
    , settingsOnException :: SomeException -> IO ()
    , settingsTimeout :: Int -- ^ seconds
    }

defaultSettings :: Settings
defaultSettings = Settings
    { settingsPort = 3000
    , settingsOnException = \e ->
        case fromException e of
            Just x -> go x
            Nothing -> print e
    , settingsTimeout = 30
    }
  where
    go :: InvalidRequest -> IO ()
    go _ = return ()
