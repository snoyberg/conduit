{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.MethodOverride
    ( methodOverride
    ) where

import Network.Wai
import Control.Monad (join)
import Data.Ascii (unsafeFromByteString)

methodOverride :: Middleware
methodOverride app req =
    app req'
  where
    req' =
        case join $ lookup "_method" $ queryString req of
            Nothing -> req
            Just m -> req { requestMethod = unsafeFromByteString m }
