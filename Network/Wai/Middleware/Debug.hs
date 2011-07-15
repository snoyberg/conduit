{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Debug
    ( debug
    , debugDest
    ) where

import Network.Wai (Request(..), Middleware)
import Network.Wai.Parse (parseRequestBody, lbsSink, fileName, Param, File)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import Data.Enumerator (run_, ($$), enumList)
import Data.Enumerator.List (consume)

-- | Prints a message to 'stderr' for each request.
debug :: Middleware
debug = debugDest $ hPutStrLn stderr . T.unpack

-- | Prints a message using the given callback function for each request.
-- This is not for serious production use- it is inefficient.
-- It immediately consumes a POST body and fills it back in and is otherwise inefficient
debugDest :: (T.Text -> IO ()) -> Middleware
debugDest cb app req = do
    body <- consume
    params <- if any (requestMethod req ==) ["GET", "HEAD"]
      then return []
      else do postParams <- liftIO $ allPostParams req body
              return $ collectPostParams postParams
    let allParams = params ++ (map emptyGetParam $ queryString req)

    liftIO $ cb $ T.pack $ concat
        [ unpack $ requestMethod req
        , " "
        , unpack $ rawPathInfo req
        , "\n"
        , (++) "Accept: " $ maybe "" unpack $ lookup "Accept" $ requestHeaders req
        , "\n"
        , show allParams
        ]
    -- we just consumed the body- fill the enumerator back up so it is available again
    liftIO $ run_ $ enumList 1 body $$ app req
  where
    allPostParams req body = run_ $ enumList 1 body $$ parseRequestBody lbsSink req

    collectPostParams :: ([Param], [File L.ByteString]) -> [Param]
    collectPostParams (postParams, files) = postParams ++
      (map (\(k,v) -> (k, S.append "FILE: " (fileName v))) files)

    emptyGetParam :: (S.ByteString, Maybe S.ByteString) -> (S.ByteString, S.ByteString)
    emptyGetParam (k, Just v) = (k,v)
    emptyGetParam (k, Nothing) = (k,"")
