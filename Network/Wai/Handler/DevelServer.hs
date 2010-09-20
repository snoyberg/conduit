{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.DevelServer (run) where

import Language.Haskell.Interpreter
import Network.Wai

import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy.Char8 as L8
import Network
    ( listenOn, accept, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Control.Exception (bracket, finally, Exception,
                          SomeException, toException)
import qualified Control.Exception as E
import System.IO (Handle, hClose)
import Control.Concurrent (forkIO, threadDelay)

import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.Chan as C
import System.Directory (getModificationTime)
import Network.Wai.Handler.SimpleServer (parseRequest, sendResponse)

type FunctionName = String

run :: Port -> ModuleName -> FunctionName -> IO ()
run port modu func = do
    queue <- C.newChan
    mqueue <- M.newMVar queue
    startApp queue $ loadingApp Nothing
    _ <- forkIO $ fillApp modu func mqueue
    run' port mqueue

startApp :: Queue -> Handler -> IO ()
startApp queue withApp = do
    forkIO (withApp go) >> return ()
  where
    go app = do
        msession <- C.readChan queue
        case msession of
            Nothing -> return ()
            Just (req, onRes) -> do
                void $ forkIO $ (handle onErr $ app req) >>= onRes
                go app
    onErr :: SomeException -> IO Response
    onErr e = return
            $ Response status500 [("Content-Type", "text/plain; charset=utf-8")]
            $ ResponseLBS $ U.fromString
            $ "Exception thrown while running application\n\n" ++ show e
    void x = x >> return ()

fillApp :: String -> String -> M.MVar Queue -> IO ()
fillApp modu func mqueue =
    go Nothing []
  where
    constSE :: x -> SomeException -> x
    constSE = const
    getTimes = handle (constSE $ return []) . mapM getModificationTime
    go prevError prevFiles = do
        toReload <-
            if null prevFiles
                then return True
                else do
                    times <- getTimes $ map fst prevFiles
                    return $ times /= map snd prevFiles
        (newError, newFiles) <-
            if toReload
                then reload prevError
                else return (prevError, prevFiles)
        threadDelay 1000000
        go newError newFiles
    reload prevError = do
        putStrLn "Attempting to interpret your app..."
        loadingApp' prevError mqueue
        res <- theapp modu func
        case res of
            Left err -> do
                putStrLn $ "Compile failed: " ++ show err
                loadingApp' (Just $ toException err) mqueue
                return (Just $ toException err, [])
            Right (app, files) -> do
                putStrLn "Interpreting success, new app loaded"
                E.handle onInitErr $ do
                    swapApp app mqueue
                    times <- getTimes files
                    return (Nothing, zip files times)
    onInitErr e = do
        putStrLn $ "Error initializing application: " ++ show e
        loadingApp' (Just e) mqueue
        return (Just e, [])

loadingApp' :: Maybe SomeException -> M.MVar Queue -> IO ()
loadingApp' err mqueue = swapApp (loadingApp err) mqueue

swapApp :: Handler -> M.MVar Queue -> IO ()
swapApp app mqueue = do
    oldqueue <- M.takeMVar mqueue
    C.writeChan oldqueue Nothing
    queue <- C.newChan
    M.putMVar mqueue queue
    startApp queue app

loadingApp :: Maybe SomeException -> Handler
loadingApp err f =
    f $ const $ return $ Response status200
        ( ("Content-Type", "text/plain")
        : case err of
            Nothing -> [("Refresh", "1")]
            Just _ -> []
        ) $ ResponseLBS $ L8.pack $ toMessage err
  where
    toMessage Nothing = "Loading code changes, please wait"
    toMessage (Just err') = "Error loading code: " ++ show err'

type Handler = (Application -> IO ()) -> IO ()

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

run' :: Port -> M.MVar Queue -> IO ()
run' port = withSocketsDo .
    bracket
        (listenOn $ PortNumber $ fromIntegral port)
        sClose .
        serveConnections port
type Port = Int

serveConnections :: Port -> M.MVar Queue -> Socket -> IO ()
serveConnections port mqueue socket = do
    (conn, remoteHost', _) <- accept socket
    _ <- forkIO $ serveConnection port mqueue conn remoteHost'
    serveConnections port mqueue socket

type Queue = C.Chan (Maybe (Request, Response -> IO ()))

serveConnection :: Port -> M.MVar Queue -> Handle -> String -> IO ()
serveConnection port mqueue conn remoteHost' = do
    env <- parseRequest port conn remoteHost'
    let onRes res =
            finally
                (sendResponse (httpVersion env) conn res)
                (hClose conn)
    queue <- M.readMVar mqueue
    C.writeChan queue $ Just (env, onRes)
