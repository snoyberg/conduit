{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.Network

main :: IO ()
main = runTCPServer (serverSettings 3001 "127.0.0.1") echo

echo :: Application IO
echo app = (appSource app) $$ (appSink app)
