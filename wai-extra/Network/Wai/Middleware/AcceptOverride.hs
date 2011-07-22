{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.AcceptOverride
    ( acceptOverride
    ) where

import Network.Wai
import Control.Monad (join)
import Data.ByteString (ByteString)

acceptOverride :: Middleware
acceptOverride app req =
    app req'
  where
    req' =
        case join $ lookup "_accept" $ queryString req of
            Nothing -> req
            Just a -> req { requestHeaders = changeVal "Accept" a $ requestHeaders req}

changeVal :: Eq a
          => a
          -> ByteString
          -> [(a, ByteString)]
          -> [(a, ByteString)]
changeVal key val old = (key, val)
                      : filter (\(k, _) -> k /= key) old
