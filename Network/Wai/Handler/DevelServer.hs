{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.DevelServer (run) where

import Language.Haskell.Interpreter
import Network.Wai
import qualified System.IO

import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Network
    ( listenOn, accept, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Control.Exception (bracket, finally, Exception, throwIO)
import System.IO (Handle, hClose)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (unless, forM)
import Data.Maybe (isJust, fromJust, fromMaybe)

import Data.Typeable (Typeable)

import Network.Socket.SendFile
import Control.Arrow (first)
import qualified Control.Concurrent.MVar as M
import System.Directory (getModificationTime)

type FunctionName = String

run :: Port -> ModuleName -> FunctionName -> IO ()
run port modu func = do
    app <- M.newMVar $ loadingApp Nothing
    _ <- forkIO $ fillApp modu func app
    run' port app

fillApp :: String -> String -> M.MVar Application -> IO ()
fillApp modu func mapp =
    go Nothing []
  where
    go prevError prevFiles = do
        toReload <-
            if null prevFiles
                then return True
                else do
                    times <- mapM (getModificationTime . fst) prevFiles
                    return $ times /= map snd prevFiles
        (newError, newFiles) <-
            if toReload
                then reload prevError
                else return (prevError, prevFiles)
        threadDelay 1000000
        go newError newFiles
    reload prevError = do
        putStrLn "Attempting to interpret your app..."
        _ <- M.swapMVar mapp $ loadingApp prevError
        res <- theapp modu func
        case res of
            Left err -> do
                putStrLn $ "Compile failed: " ++ show err
                _ <- M.swapMVar mapp $ loadingApp $ Just err
                return (Just err, [])
            Right (app, files) -> do
                putStrLn "Interpreting success, new app loaded"
                app $ \app' -> (forkIO $ do
                    _ <- M.swapMVar mapp app'
                    return ()) >> return ()
                files' <- forM files $ \f -> do
                    t <- getModificationTime f
                    return (f, t)
                return (Nothing, files')


loadingApp :: Maybe InterpreterError -> Application
loadingApp err _ =
    return $ Response status200
        [ ("Content-Type", "text/plain")
        , ("Refresh", "1")
        ] $ ResponseLBS $ L8.pack $ toMessage err
  where
    toMessage Nothing = "Loading code changes, please wait"
    toMessage (Just err') = "Error loading code: " ++ show err'

type Handler = (Application -> IO ()) -> IO ()
type MHandler = M.MVar Application

theapp :: String -> String -> IO (Either InterpreterError (Handler, [FilePath]))
theapp modu func =
    runInterpreter $ do
        loadModules [modu]
        mods <- getLoadedModules
        setImports ["Prelude", "Network.Wai", modu]
        app <- interpret func infer
        return (app, map toFile mods)
  where
    toFile s = map toSlash s ++ ".hs"
    toSlash '.' = '/'
    toSlash c   = c

run' :: Port -> MHandler -> IO ()
run' port = withSocketsDo .
    bracket
        (listenOn $ PortNumber $ fromIntegral port)
        sClose .
        serveConnections port
type Port = Int

serveConnections :: Port -> MHandler -> Socket -> IO ()
serveConnections port app socket = do
    (conn, remoteHost', _) <- accept socket
    _ <- forkIO $ serveConnection port app conn remoteHost'
    serveConnections port app socket

serveConnection :: Port -> MHandler -> Handle -> String -> IO ()
serveConnection port handler conn remoteHost' =
    finally
        serveConnection'
        (hClose conn)
    where
        serveConnection' = do
            env <- hParseRequest port conn remoteHost'
            putStrLn "reading handler"
            app <- M.readMVar handler
            putStrLn "done reading handler"
            res <- app env
            putStrLn "got result"
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
    B.hPut h $ B.pack "\r\n"
    case responseBody res of
        ResponseFile fp -> unsafeSendFile h fp
        ResponseEnumerator (Enumerator enum) -> enum myPut h >> return ()
        ResponseLBS lbs -> L.hPut h lbs
    where
        myPut _ bs = do
            B.hPut h bs
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

requestBodyHandle :: Handle -> Int -> Source
requestBodyHandle h =
    requestBodyFunc go
  where
    go i = Just `fmap` B.hGet h (min i defaultChunkSize)

requestBodyFunc :: (Int -> IO (Maybe B.ByteString)) -> Int -> Source
requestBodyFunc _ 0 = Source $ return Nothing
requestBodyFunc h len = Source $ do
    mbs <- h len
    case mbs of
        Nothing -> return Nothing
        Just bs -> do
            let newLen = len - B.length bs
            return $ Just (bs, requestBodyFunc h $ max 0 newLen)
