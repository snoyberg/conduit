{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Debug (debug) where

import Network.Wai (Middleware, requestMethod, requestHeaders, pathInfo, queryString)
import Data.ByteString.Char8 (unpack)
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe

-- | Prints a message to 'stderr' for each request.
debug :: Middleware
debug app req = do
    liftIO $ hPutStrLn stderr $ concat
        [ unpack $ requestMethod req
        , " "
        , unpack $ pathInfo req
        , unpack $ queryString req
        , "\n"
        , (++) "Accept: " $ unpack $ fromMaybe "" $ lookup "Accept" $ requestHeaders req
        , "\n"
        ]
    app req
