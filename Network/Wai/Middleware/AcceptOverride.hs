{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.AcceptOverride
    ( acceptOverride
    ) where

import Network.Wai
import Network.Wai.Parse (parseQueryString)
import qualified Data.ByteString.Char8 as B8

acceptOverride :: Middleware
acceptOverride app req =
    app req'
  where
    req' =
        case lookup "_accept" $ parseQueryString $ queryString req of
            Nothing -> req
            Just a -> req { requestHeaders = changeVal "Accept" (B8.unpack a) $ requestHeaders req}

changeVal :: Eq a
          => a
          -> String
          -> [(a, B8.ByteString)]
          -> [(a, B8.ByteString)]
changeVal key val old = (key, B8.pack val)
                      : filter (\(k, _) -> k /= key) old
