{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.CleanPath
    ( cleanPath
    ) where

import Network.Wai
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types (status301)
import Data.Text (Text)
import Data.Monoid (mconcat)

cleanPath :: ([Text] -> Either B.ByteString [Text])
          -> B.ByteString
          -> ([Text] -> Application)
          -> Application
cleanPath splitter prefix app env =
    case splitter $ pathInfo env of
        Right pieces -> app pieces env
        Left p -> return
                $ responseLBS status301
                  [("Location", mconcat [prefix, p, suffix])]
                $ L.empty
    where
        -- include the query string if present
        suffix =
            case B.uncons $ rawQueryString env of
                Nothing -> B.empty
                Just ('?', _) -> rawQueryString env
                _ -> B.cons '?' $ rawQueryString env
