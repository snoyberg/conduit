{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.MethodOverride
    ( methodOverride
    ) where

import Network.Wai
import Network.Wai.Parse (parseQueryString)

methodOverride :: Middleware
methodOverride app req =
    app req'
  where
    req' =
        case lookup "_method" $ parseQueryString $ queryString req of
            Nothing -> req
            Just m -> req { requestMethod = m }
