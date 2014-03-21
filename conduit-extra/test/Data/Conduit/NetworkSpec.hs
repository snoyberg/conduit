{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.NetworkSpec (spec) where

import Data.Conduit
import Data.Conduit.Network
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM_)
import Test.Hspec

spec :: Spec
spec = it "Data.Conduit.Network" $ do
    _ <- forkIO $ runTCPServer (serverSettings 4009 "*4") echo
    threadDelay 1000000
    replicateM_ 10000
        $ runTCPClient (clientSettings 4009 "127.0.0.1") doNothing

echo :: Application IO
echo ad = appSource ad $$ appSink ad

doNothing :: Application IO
doNothing _ = return ()
