{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.CleanPath
    ( cleanPath
    ) where

import Network.Wai
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

cleanPath :: (B.ByteString -> Either B.ByteString [String])
          -> B.ByteString
          -> ([String] -> Request -> IO Response)
          -> Request
          -> IO Response
cleanPath splitter prefix app env =
    case splitter $ pathInfo env of
        Right pieces -> app pieces env
        Left p -> return
                $ ResponseLBS status301
                  [("Location", B.concat [prefix, p, suffix])]
                $ L.empty
    where
        -- include the query string if present
        suffix =
            case B.uncons $ queryString env of
                Nothing -> B.empty
                Just ('?', _) -> queryString env
                _ -> B.cons '?' $ queryString env
