---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.MethodOverride
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Override the HTTP method based on either:
--   The X-HTTP-Method-Override header.
--   The _method_override GET parameter.
--
---------------------------------------------------------
module Network.Wai.Middleware.MethodOverride (methodOverride) where

import Network.Wai
import Web.Encodings (decodeUrlPairs)
import Data.Monoid (mappend)
import Data.Char
import qualified Data.ByteString.Char8 as B8

moHeader :: RequestHeader
moHeader = requestHeaderFromBS $ B8.pack "X-HTTP-Method-Override"

moParam :: B8.ByteString
moParam = B8.pack "_method_override"

methodOverride :: Middleware
methodOverride app env = do
    let mo1 = lookup moHeader $ requestHeaders env
        gets = decodeUrlPairs $ queryString env
        mo2 = lookup moParam gets
    app $
        case mo1 `mappend` mo2 of
            Nothing -> env
            Just nm -> env { requestMethod = methodFromBS $ B8.map toUpper nm }
