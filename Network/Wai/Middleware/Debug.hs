{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Debug (debug) where

import Network.Wai (Middleware, requestMethod, requestHeaders, rawPathInfo, rawQueryString)
import Data.ByteString.Char8 (unpack)
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (liftIO)

-- | Prints a message to 'stderr' for each request.
debug :: Middleware
debug app req = do
    liftIO $ hPutStrLn stderr $ concat
        [ unpack $ requestMethod req
        , " "
        , unpack $ rawPathInfo req
        , unpack $ rawQueryString req
        , "\n"
        , (++) "Accept: " $ maybe "" unpack $ lookup "Accept" $ requestHeaders req
        , "\n"
        ]
    app req
