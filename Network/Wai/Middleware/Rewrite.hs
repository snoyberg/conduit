{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Rewrite
    ( rewrite, autoHtmlRewrite
    ) where

import Network.Wai
import qualified Data.ByteString.Char8 as B
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (liftIO)
import Web.Routes.Base (decodePathInfo)

-- | rewrite based on your own conversion rules
-- Example usage: rewrite (autoHtmlRewrite "static")
rewrite :: (B.ByteString -> IO B.ByteString) -> Middleware
rewrite convert app req = do
  newPathInfo <- liftIO $ convert $ pathInfo req
  app req { pathInfo = newPathInfo }

-- | example rewriter
--   implements 2 rules for static html re-writes
--   1) for a directory foo/, check for foo/index.html
--   2) for a non-directory bar, check for bar.html
--   if the file exists, do the rewrite
autoHtmlRewrite :: String -> B.ByteString -> IO B.ByteString
autoHtmlRewrite staticDir pInfo = do
    fe <- doesFileExist $ staticDir ++ "/" ++ reWritePath
    return $ if fe then B.pack reWritePath else pInfo
  where
    reWritePath = concat $ map ((:) '/') reWritePieces
    pieces = decodePathInfo $ B.unpack $ pInfo
    reWritePieces =
       if (null pieces) || (null $ last pieces)
          then pieces ++  ["index.html"]
          else (init pieces) ++ [(last pieces) ++ ".html"]
