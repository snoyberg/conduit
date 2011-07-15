{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Debug
    ( debug
    , debugDest
    ) where

import Network.Wai (Middleware, requestMethod, requestHeaders, rawPathInfo, rawQueryString)
import Network.Wai.Parse (parseRequestBody, lbsSink, fileName)
import Data.ByteString.Char8 (unpack)
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import Data.Enumerator (run_, ($$), enumList)
import Data.Enumerator.List (consume)

-- | Prints a message to 'stderr' for each request.
debug :: Middleware
debug = debugDest $ hPutStrLn stderr . T.unpack

-- | Prints a message using the given callback function for each request.
debugDest :: (T.Text -> IO ()) -> Middleware
debugDest cb app req = do
    body <- consume
    (params, files) <- liftIO $ run_ $ enumList 1 body $$ parseRequestBody lbsSink req
    liftIO $ cb $ T.pack $ concat
        [ unpack $ requestMethod req
        , " "
        , unpack $ rawPathInfo req
        , unpack $ rawQueryString req
        , "\n"
        , (++) "Accept: " $ maybe "" unpack $ lookup "Accept" $ requestHeaders req
        , "\n"
        , if null params then "" else "Post parameters: " ++ show params ++ "\n"
        , if null files then "" else "Post file names: " ++ show (map (fileName . snd) files) ++ "\n"
        ]
    liftIO $ run_ $ enumList 1 body $$ app req
