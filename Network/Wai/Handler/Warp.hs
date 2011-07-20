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
    , runSettings
    , runSettingsSocket
      -- * Settings
    , Settings
    , defaultSettings
    , settingsPort
    , settingsHost
    , settingsOnException
    , settingsTimeout
      -- * Datatypes
    , Port
    , InvalidRequest (..)
      -- * Internal
    , Manager
    , withManager
    , parseRequest
    , sendResponse
    , registerKillThread
    , bindPort
    , enumSocket
    , pause
    , resume
#if TEST
    , takeHeaders
    , readInt
#endif
    ) where

import Prelude hiding (catch, lines)
import Network.Wai

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network (sClose, Socket)
import Network.Socket
    ( accept, Family (..)
    , SocketType (Stream), listen, bindSocket, setSocketOption, maxListenQueue
    , SockAddr, SocketOption (ReuseAddr)
    , AddrInfo(..), AddrInfoFlag(..), defaultHints, getAddrInfo
    )
import qualified Network.Socket
import qualified Network.Socket.ByteString as Sock
import Control.Exception
    ( bracket, finally, Exception, SomeException, catch
    , fromException, AsyncException (ThreadKilled)
    , bracketOnError
    )
import Control.Concurrent (forkIO)
import qualified Data.Char as C
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
import Network.Sendfile

import qualified System.PosixCompat.Files as P

import Control.Monad.IO.Class (liftIO)
import qualified Timeout as T
import Timeout (Manager, registerKillThread, pause, resume)
import Data.Word (Word8)
import Data.List (foldl')
import Control.Monad (forever)
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import System.IO (hPutStrLn, stderr)

#if WINDOWS
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MV
import Network.Socket (withSocketsDo)
#endif

bindPort :: Int -> String -> IO Socket
bindPort p s = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE
                                         , AI_NUMERICSERV
                                         , AI_NUMERICHOST]
                             , addrSocketType = Stream }
        host = if s == "*" then Nothing else Just s
        port = Just . show $ p
    addrs <- getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
        addr = if null addrs' then head addrs else head addrs'
    bracketOnError
        (Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (addrAddress addr)
            listen sock maxListenQueue
            return sock
        )

-- | Run an 'Application' on the given port. This calls 'runSettings' with
-- 'defaultSettings'.
run :: Port -> Application -> IO ()
run p = runSettings defaultSettings { settingsPort = p }

-- | Run a Warp server with the given settings.
runSettings :: Settings -> Application -> IO ()
#if WINDOWS
runSettings set app = withSocketsDo $ do
    var <- MV.newMVar Nothing
    let clean = MV.modifyMVar_ var $ \s -> maybe (return ()) sClose s >> return Nothing
    _ <- forkIO $ bracket
        (bindPort (settingsPort set) (settingsHost set))
        (const clean)
        (\s -> do
            MV.modifyMVar_ var (\_ -> return $ Just s)
            runSettingsSocket set s app)
    forever (threadDelay maxBound) `finally` clean
#else
runSettings set =
    bracket
        (bindPort (settingsPort set) (settingsHost set))
        sClose .
        (flip (runSettingsSocket set))
#endif

type Port = Int

-- | Same as 'runSettings', but uses a user-supplied socket instead of opening
-- one. This allows the user to provide, for example, Unix named socket, which
-- can be used when reverse HTTP proxying into your application.
--
-- Note that the 'settingsPort' will still be passed to 'Application's via the
-- 'serverPort' record.
runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket set socket app = do
    let onE = settingsOnException set
        port = settingsPort set
    tm <- T.initialize $ settingsTimeout set * 1000000
    forever $ do
        (conn, sa) <- accept socket
        _ <- forkIO $ do
            th <- T.registerKillThread tm
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
        (len, env) <- parseRequest port remoteHost'
        -- Let the application run for as long as it wants
        liftIO $ T.pause th
        res <- E.joinI $ EB.isolate len $$ app env
        liftIO $ T.resume th
        keepAlive <- liftIO $ sendResponse th env conn res
        if keepAlive then serveConnection' else return ()

parseRequest :: Port -> SockAddr -> E.Iteratee S.ByteString IO (Integer, Request)
parseRequest port remoteHost' = do
    headers' <- takeHeaders
    parseRequest' port headers' remoteHost'

-- FIXME come up with good values here
bytesPerRead, maxTotalHeaderLength :: Int
bytesPerRead = 4096
maxTotalHeaderLength = 50 * 1024

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | IncompleteHeaders
    | OverLargeHeader
    deriving (Show, Typeable, Eq)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Port
              -> [ByteString]
              -> SockAddr
              -> E.Iteratee S.ByteString IO (Integer, Request)
parseRequest' _ [] _ = E.throwError $ NotEnoughLines []
parseRequest' port (firstLine:otherLines) remoteHost' = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let (host',rpath) =
            if S.null rpath'
                then ("","/")
                else if "http://" `S.isPrefixOf` rpath'
                         then S.breakByte 47 $ S.drop 7 rpath' -- '/'
                         else ("", rpath')
    let heads = map parseHeaderNoAttr otherLines
    let host = fromMaybe host' $ lookup "host" heads
    let len =
            case lookup "content-length" heads of
                Nothing -> 0
                Just bs -> fromIntegral $ B.foldl' (\i c -> i * 10 + C.digitToInt c) 0 $ B.takeWhile C.isDigit bs
    let serverName' = takeUntil 58 host -- ':'
    -- FIXME isolate takes an Integer instead of Int or Int64. If this is a
    -- performance penalty, we may need our own version.
    return (len, Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = H.decodePathSegments rpath
                , rawPathInfo = rpath
                , rawQueryString = gets
                , queryString = H.parseQuery gets
                , serverName = serverName'
                , serverPort = port
                , requestHeaders = heads
                , isSecure = False
                , remoteHost = remoteHost'
                })


takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

parseFirst :: ByteString
           -> E.Iteratee S.ByteString IO (ByteString, ByteString, ByteString, H.HttpVersion)
parseFirst s = 
    case S.split 32 s of  -- ' '
        [method, query, http'] -> do
            let (hfirst, hsecond) = B.splitAt 5 http'
            if hfirst == "HTTP/"
               then let (rpath, qstring) = S.breakByte 63 query  -- '?'
                        hv =
                            case hsecond of
                                "1.1" -> H.http11
                                _ -> H.http10
                    in return (method, rpath, qstring, hv)
               else E.throwError NonHttp
        _ -> E.throwError $ BadFirstLine $ B.unpack s
{-# INLINE parseFirst #-} -- FIXME is this inline necessary? the function is only called from one place and not exported

httpBuilder, spaceBuilder, newlineBuilder, transferEncodingBuilder
           , colonSpaceBuilder :: Builder
httpBuilder = copyByteString "HTTP/"
spaceBuilder = fromChar ' '
newlineBuilder = copyByteString "\r\n"
transferEncodingBuilder = copyByteString "Transfer-Encoding: chunked\r\n\r\n"
colonSpaceBuilder = copyByteString ": "

headers :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> Builder
headers !httpversion !status !responseHeaders !isChunked' = {-# SCC "headers" #-}
    let !start = httpBuilder
                `mappend` (copyByteString $
                            case httpversion of
                                H.HttpVersion 1 1 -> "1.1"
                                _ -> "1.0")
                `mappend` spaceBuilder
                `mappend` fromShow (H.statusCode status)
                `mappend` spaceBuilder
                `mappend` copyByteString (H.statusMessage status)
                `mappend` newlineBuilder
        !start' = foldl' responseHeaderToBuilder start responseHeaders
        !end = if isChunked'
                 then transferEncodingBuilder
                 else newlineBuilder
    in start' `mappend` end

responseHeaderToBuilder :: Builder -> H.Header -> Builder
responseHeaderToBuilder b (x, y) = b
  `mappend` copyByteString (CI.original x)
  `mappend` colonSpaceBuilder
  `mappend` copyByteString y
  `mappend` newlineBuilder

checkPersist :: Request -> Bool
checkPersist req
    | ver == H.http11 = checkPersist11 conn
    | otherwise       = checkPersist10 conn
  where
    ver = httpVersion req
    conn = lookup "connection" $ requestHeaders req
    checkPersist11 (Just x)
        | CI.foldCase x == "close"      = False
    checkPersist11 _                    = True
    checkPersist10 (Just x)
        | CI.foldCase x == "keep-alive" = True
    checkPersist10 _                    = False

isChunked :: H.HttpVersion -> Bool
isChunked = (==) H.http11

hasBody :: H.Status -> Request -> Bool
hasBody s req = s /= (H.Status 204 "") && requestMethod req /= "HEAD"

sendResponse :: T.Handle
             -> Request -> Socket -> Response -> IO Bool
sendResponse th req socket (ResponseFile s hs fp mpart) = do
    (hs', cl) <-
        case (readInt `fmap` lookup "content-length" hs, mpart) of
            (Just cl, _) -> return (hs, cl)
            (Nothing, Nothing) -> do
                cl <- P.fileSize `fmap` P.getFileStatus fp
                return (("Content-Length", B.pack $ show cl):hs, fromIntegral cl)
            (Nothing, Just part) -> do
                let cl = filePartByteCount part
                return (("Content-Length", B.pack $ show cl):hs, fromIntegral cl)
    Sock.sendMany socket $ L.toChunks $ toLazyByteString $ headers (httpVersion req) s hs' False
    let isPersist= checkPersist req
    if hasBody s req
        then do
            case mpart of
                Nothing   -> sendfile socket fp PartOfFile {
                    rangeOffset = 0
                  , rangeLength = cl
                  } (T.tickle th)
                Just part -> sendfile socket fp PartOfFile {
                    rangeOffset = filePartOffset part
                  , rangeLength = filePartByteCount part
                  } (T.tickle th)
            T.tickle th
            return isPersist
        else return isPersist
sendResponse th req socket (ResponseBuilder s hs b)
    | hasBody s req = do
          toByteStringIO (\bs -> do
            Sock.sendAll socket bs
            T.tickle th) b'
          return isKeepAlive
    | otherwise = do
        Sock.sendMany socket
            $ L.toChunks
            $ toLazyByteString
            $ headers (httpVersion req) s hs False
        T.tickle th
        return True
  where
    headers' = headers (httpVersion req) s hs isChunked'
    b' = if isChunked'
            then headers'
                 `mappend` chunkedTransferEncoding b
                 `mappend` chunkedTransferTerminator
            else headers (httpVersion req) s hs False `mappend` b
    hasLength = lookup "content-length" hs /= Nothing
    isChunked' = isChunked (httpVersion req) && not hasLength
    isPersist = checkPersist req
    isKeepAlive = isPersist && (isChunked' || hasLength)
sendResponse th req socket (ResponseEnumerator res) =
    res go
  where
    -- FIXME perhaps alloca a buffer per thread and reuse that in all functiosn below. Should lessen greatly the GC burden (I hope)
    go s hs | not (hasBody s req) = do
            liftIO $ Sock.sendMany socket
                   $ L.toChunks $ toLazyByteString
                   $ headers (httpVersion req) s hs False
            return True
    go s hs = chunk'
          $ E.enumList 1 [headers (httpVersion req) s hs isChunked']
         $$ E.joinI $ builderToByteString -- FIXME unsafeBuilderToByteString
         $$ (iterSocket th socket >> return isKeepAlive)
      where
        hasLength = lookup "content-length" hs /= Nothing
        isChunked' = isChunked (httpVersion req) && not hasLength
        isPersist = checkPersist req
        isKeepAlive = isPersist && (isChunked' || hasLength)
        chunk' i = if isChunked'
                      then E.joinI $ chunk $$ i
                      else i
        chunk :: E.Enumeratee Builder Builder IO Bool
        chunk = E.checkDone $ E.continue . step
        step k E.EOF = k (E.Chunks [chunkedTransferTerminator]) >>== return
        step k (E.Chunks []) = E.continue $ step k
        step k (E.Chunks [x]) = k (E.Chunks [chunkedTransferEncoding x]) >>== chunk
        step k (E.Chunks xs) = k (E.Chunks [chunkedTransferEncoding $ mconcat xs]) >>== chunk

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        restLen = S.length rest
        -- FIXME check for colon without following space?
        rest' = if restLen > 1 && SU.unsafeTake 2 rest == ": "
                   then SU.unsafeDrop 2 rest
                   else rest
     in (CI.mk k, rest')

enumSocket :: T.Handle -> Int -> Socket -> E.Enumerator ByteString IO a
enumSocket th len socket =
    inner
  where
    inner (E.Continue k) = do
        bs <- liftIO $ Sock.recv socket len
        liftIO $ T.tickle th
        if S.null bs
            then E.continue k
            else k (E.Chunks [bs]) >>== inner
    inner step = E.returnI step
------ The functions below are not warp-specific and could be split out into a
--separate package.

iterSocket :: T.Handle
           -> Socket
           -> E.Iteratee B.ByteString IO ()
iterSocket th sock =
    E.continue step
  where
    -- We pause timeouts before passing control back to user code. This ensures
    -- that a timeout will only ever be executed when Warp is in control. We
    -- also make sure to resume the timeout after the completion of user code
    -- so that we can kill idle connections.
    step E.EOF = liftIO (T.resume th) >> E.yield () E.EOF
    step (E.Chunks []) = E.continue step
    step (E.Chunks xs) = do
        liftIO $ T.resume th
        liftIO $ Sock.sendMany sock xs
        liftIO $ T.pause th
        E.continue step

-- | Various Warp server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and record syntax to modify individual records. For example:
--
-- > defaultSettings { settingsTimeout = 20 }
data Settings = Settings
    { settingsPort :: Int -- ^ Port to listen on. Default value: 3000
    , settingsHost :: String -- ^ Host to bind to, or * for all. Default value: *
    , settingsOnException :: SomeException -> IO () -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , settingsTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    }

-- | The default settings for the Warp server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings = Settings
    { settingsPort = 3000
    , settingsHost = "*"
    , settingsOnException = \e ->
        case fromException e of
            Just x -> go x
            Nothing ->
                if go' $ fromException e
                    then hPutStrLn stderr $ show e
                    else return ()
    , settingsTimeout = 30
    }
  where
    go :: InvalidRequest -> IO ()
    go _ = return ()
    go' (Just ThreadKilled) = False
    go' _ = True

takeHeaders :: E.Iteratee ByteString IO [ByteString]
takeHeaders = do
  !x <- forceHead
  takeHeaders' 0 id id x

{-# INLINE takeHeaders #-}

takeHeaders' :: Int
             -> ([ByteString] -> [ByteString])
             -> ([ByteString] -> [ByteString])
             -> ByteString
             -> E.Iteratee S.ByteString IO [ByteString]
takeHeaders' !len _ _ _ | len > maxTotalHeaderLength = E.throwError OverLargeHeader
takeHeaders' !len !lines !prepend !bs = do
  let !bsLen = {-# SCC "takeHeaders'.bsLen" #-} S.length bs
      !mnl = {-# SCC "takeHeaders'.mnl" #-} S.elemIndex 10 bs
  case mnl of
       -- no newline.  prepend entire bs to next line
       !Nothing -> {-# SCC "takeHeaders'.noNewline" #-} do
         let !len' = len + bsLen
         !more <- forceHead 
         takeHeaders' len' lines (prepend . (:) bs) more
       Just !nl -> {-# SCC "takeHeaders'.newline" #-} do
         let !end = nl 
             !start = nl + 1
             !line = {-# SCC "takeHeaders'.line" #-}
                     if end > 0
                        -- line data included in this chunk
                        then S.concat $! prepend [SU.unsafeTake (checkCR bs end) bs]
                        --then S.concat $! prepend [SU.unsafeTake (end-1) bs]
                        -- no line data in this chunk (all in prepend, or empty line)
                        else S.concat $! prepend []
         if S.null line
            -- no more headers
            then {-# SCC "takeHeaders'.noMoreHeaders" #-} do
              let !lines' = {-# SCC "takeHeaders'.noMoreHeaders.lines'" #-} lines []
              if start < bsLen
                 then {-# SCC "takeHeaders'.noMoreHeaders.yield" #-} do
                   let !rest = {-# SCC "takeHeaders'.noMoreHeaders.yield.rest" #-} SU.unsafeDrop start bs
                   E.yield lines' $! E.Chunks [rest]
                 else return lines'

            -- more headers
            else {-# SCC "takeHeaders'.moreHeaders" #-} do
              let !len' = len + start 
                  !lines' = {-# SCC "takeHeaders.lines'" #-} lines . (:) line
              !more <- {-# SCC "takeHeaders'.more" #-} 
                       if start < bsLen
                          then return $! SU.unsafeDrop start bs
                          else forceHead
              {-# SCC "takeHeaders'.takeMore" #-} takeHeaders' len' lines' id more
{-# INLINE takeHeaders' #-}

forceHead :: E.Iteratee ByteString IO ByteString
forceHead = do
  !mx <- EL.head
  case mx of
       !Nothing -> E.throwError IncompleteHeaders
       Just !x -> return x
{-# INLINE forceHead #-}

checkCR :: ByteString -> Int -> Int
checkCR bs pos = 
  let !p = pos - 1
  in if '\r' == B.index bs p
        then p
        else pos
{-# INLINE checkCR #-}

-- Note: This function produces garbage on invalid input. But serving an
-- invalid content-length is a bad idea, mkay?
readInt :: S.ByteString -> Integer
readInt = S.foldl' (\x w -> x * 10 + fromIntegral w - 48) 0

-- | Call the inner function with a timeout manager.
withManager :: Int -- ^ timeout in microseconds
            -> (Manager -> IO a)
            -> IO a
withManager timeout f = do
    -- FIXME when stopManager is available, use it
    man <- T.initialize timeout
    f man
