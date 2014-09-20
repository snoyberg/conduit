{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.NetworkSpec (spec) where

import Data.Conduit
import Data.Conduit.Network
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM_)
import Test.Hspec


spec :: Spec
spec = describe "Data.Conduit.Network" $ do

    describe "run server" $ do
        it "can connect" $ do
            _ <- forkIO $ runTCPServer (serverSettings 4009 "*4") echo
            threadDelay 1000000
            replicateM_ 10000
                $ runTCPClient (clientSettings 4009 "127.0.0.1") doNothing

        it "can connect to general server" $ do
            let set = serverSettings 4010 "*4"
            runGeneralTCPServer set echo
            replicateM_ 10000
                $ runTCPClient (clientSettings 4010 "127.0.0.1") doNothing

        it "run server also executes custom afterBind" $ do
            assertMVar <- newEmptyMVar
            let set = serverSettings 4011 "*4"
                setWithAfterBind = setAfterBind (\_ -> putMVar assertMVar ()) set 
            runGeneralTCPServer setWithAfterBind echo
            takeMVar assertMVar

        it "run server really waits for server to be finalized before returning" $ do
            let set = serverSettings 4012 "*4"
                setWithAfterBind = setAfterBind (\_ -> threadDelay 1000000) set 
            runGeneralTCPServer setWithAfterBind echo
            replicateM_ 10000
                $ runTCPClient (clientSettings 4010 "127.0.0.1") doNothing

echo :: AppData -> IO ()
echo ad = appSource ad $$ appSink ad

doNothing :: AppData -> IO ()
doNothing _ = return ()
