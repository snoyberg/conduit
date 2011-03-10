{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.AcceptOverride
    ( acceptOverride
    ) where

import Network.Wai
import Control.Monad (join)
import qualified Data.Ascii as A

acceptOverride :: Middleware
acceptOverride app req =
    app req'
  where
    req' =
        case join $ lookup "_accept" $ queryString req of
            Nothing -> req
            Just a -> req { requestHeaders = changeVal "Accept" (A.unsafeFromByteString a) $ requestHeaders req}

changeVal :: Eq a
          => a
          -> A.Ascii
          -> [(a, A.Ascii)]
          -> [(a, A.Ascii)]
changeVal key val old = (key, val)
                      : filter (\(k, _) -> k /= key) old
