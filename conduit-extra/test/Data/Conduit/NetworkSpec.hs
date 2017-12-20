{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.NetworkSpec (spec) where

import Data.Conduit
import Data.Conduit.Network
import Control.Concurrent (forkIO, threadDelay, putMVar, newEmptyMVar, takeMVar, killThread)
import Control.Monad (replicateM_)
import Test.Hspec

spec :: Spec
spec = describe "Data.Conduit.Network" $ do
    describe "run general server" $ do
        it "running tcp server" $ do
            _ <- forkIO $ runTCPServer (serverSettings 4009 "*4") echo
            threadDelay 1000000
            replicateM_ 100
                $ runTCPClient (clientSettings 4009 "127.0.0.1") doNothing
    describe "fork server" $ do
        it "can connect to server" $ do
            let set = serverSettings 4010 "*4"
            threadId <- forkTCPServer set echo
            replicateM_ 100
                $ runTCPClient (clientSettings 4010 "127.0.0.1") doNothing
            killThread threadId

        it "fork server also executes custom afterBind" $ do
            assertMVar <- newEmptyMVar
            let set = serverSettings 4010 "*4"
                setWithAfterBind = setAfterBind (\_ -> putMVar assertMVar ()) set
            threadId <- forkTCPServer setWithAfterBind echo
            takeMVar assertMVar
            killThread threadId

        it "fork server really waits for server to be finalized before returning" $ do
            let set = serverSettings 4010 "*4"
                setWithAfterBind = setAfterBind (\_ -> threadDelay 1000000) set
            threadId <- forkTCPServer setWithAfterBind echo
            replicateM_ 100
                $ runTCPClient (clientSettings 4010 "127.0.0.1") doNothing
            killThread threadId



echo :: AppData -> IO ()
echo ad = runConduit $ appSource ad .| appSink ad

doNothing :: AppData -> IO ()
doNothing _ = return ()
