{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.CleanPath
    ( cleanPath
    ) where

import Network.Wai
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types (status301)
import Data.Text (Text)
import qualified Data.Ascii as A
import Data.Monoid (mconcat)

cleanPath :: ([Text] -> Either A.Ascii [Text])
          -> A.Ascii
          -> ([Text] -> Application)
          -> Application
cleanPath splitter prefix app env =
    case splitter $ pathInfo env of
        Right pieces -> app pieces env
        Left p -> return
                $ responseLBS status301
                  [("Location", mconcat [prefix, p, A.unsafeFromByteString suffix])]
                $ L.empty
    where
        -- include the query string if present
        suffix =
            case B.uncons $ rawQueryString env of
                Nothing -> B.empty
                Just ('?', _) -> rawQueryString env
                _ -> B.cons '?' $ rawQueryString env
