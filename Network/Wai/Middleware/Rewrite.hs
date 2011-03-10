{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Rewrite
    ( rewrite, autoHtmlRewrite
    ) where

import Network.Wai
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, unpack, pack)

-- | rewrite based on your own conversion rules
-- Example usage: rewrite (autoHtmlRewrite "static")
rewrite :: ([Text] -> IO [Text]) -> Middleware
rewrite convert app req = do
  newPathInfo <- liftIO $ convert $ pathInfo req
  app req { pathInfo = newPathInfo }

-- | example rewriter
--   implements 2 rules for static html re-writes
--   1) for a directory foo/, check for foo/index.html
--   2) for a non-directory bar, check for bar.html
--   if the file exists, do the rewrite
autoHtmlRewrite :: String -> [Text] -> IO [Text]
autoHtmlRewrite staticDir pieces' = do
    fe <- doesFileExist $ staticDir ++ "/" ++ reWritePath
    return $ if fe then map pack reWritePieces else pieces'
  where
    pieces = map unpack pieces'
    reWritePath = concat $ map ((:) '/') reWritePieces
    reWritePieces =
       if (null pieces) || (null $ last pieces)
          then pieces ++  ["index.html"]
          else (init pieces) ++ [(last pieces) ++ ".html"]
