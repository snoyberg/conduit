{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.Gzip
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic gzip compression of responses.
--
---------------------------------------------------------
module Network.Wai.Middleware.Gzip (gzip) where

import Network.Wai
import Network.Wai.Zlib
import Data.Maybe (fromMaybe)
import Data.Enumerator (($$), joinI)
import qualified Data.ByteString.Char8 as S8

-- | Use gzip to compress the body of the response.
--
-- Analyzes the \"Accept-Encoding\" header from the client to determine
-- if gzip is supported.
--
-- Possible future enhancements:
--
-- * Only compress if the response is above a certain size.
--
-- * I've read that IE can\'t support compression for Javascript files.
gzip :: Bool -- ^ should we gzip files?
     -> Middleware
gzip files app env = do
    res <- app env
    return $
        case res of
            ResponseFile{} | not files -> res
            _ -> if "gzip" `elem` enc
                    then ResponseEnumerator $ compressE $ responseEnumerator res
                    else res
  where
    enc = fromMaybe [] $ (splitCommas . S8.unpack)
                    `fmap` lookup "Accept-Encoding" (requestHeaders env)

compressE :: (forall a. ResponseEnumerator a)
          -> (forall a. ResponseEnumerator a)
compressE re f =
    re f'
    --e s hs'
  where
    f' s hs =
        joinI $ compress $$ f s hs'
      where
        -- Remove Content-Length header, since we will certainly have a
        -- different length after gzip compression.
        hs' = ("Content-Encoding", "gzip") : filter notLength hs
        notLength (x, _) = x /= "content-length"

splitCommas :: String -> [String]
splitCommas [] = []
splitCommas x =
    let (y, z) = break (== ',') x
     in y : splitCommas (drop 1 z)
