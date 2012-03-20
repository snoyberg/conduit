{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.Network

main :: IO ()
main = runTCPServer (ServerSettings 3001 "127.0.0.1") echo

echo :: Application IO
echo src sink = src $$ sink
