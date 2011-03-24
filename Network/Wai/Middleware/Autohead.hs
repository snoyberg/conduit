{-# LANGUAGE OverloadedStrings #-}
-- | Automatically produce responses to HEAD requests based on the underlying
-- applications GET response.
module Network.Wai.Middleware.Autohead (autohead) where

import Network.Wai
import Data.Monoid (mempty)
import Data.Enumerator (enumEOF, ($$))

autohead :: Middleware
autohead app req
    | requestMethod req == "HEAD" = do
        res <- app req { requestMethod = "GET" }
        case res of
            ResponseFile s hs _ _ -> return $ ResponseBuilder s hs mempty
            ResponseBuilder s hs _ -> return $ ResponseBuilder s hs mempty
            ResponseEnumerator e -> do
                let helper f =
                        let helper' s hs = enumEOF $$ f s hs
                         in e helper'
                return $ ResponseEnumerator helper
    | otherwise = app req

