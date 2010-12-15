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
import qualified Data.ByteString.Char8 as B
import Data.Enumerator (($$), joinI)

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
     -> Middleware a
gzip files app env = do
    res <- app env
    case shouldGzip res of
        Nothing -> return res
        Just enum -> return $ ResponseEnumerator $ compressE enum
  where
    shouldGzip ResponseFile{} | not files = Nothing
    shouldGzip res =
        if "gzip" `elem` enc
            then Just $ responseEnumerator res
            else Nothing
    enc = fromMaybe [] $ (splitCommas . B.unpack)
                    `fmap` lookup "Accept-Encoding" (requestHeaders env)

compressE :: ResponseEnumerator a -> ResponseEnumerator a
compressE re f =
    re f'
    --e s hs'
  where
    f' s hs =
        joinI $ compress $$ f s hs'
      where
        hs' = ("Content-Encoding", "gzip") : hs

splitCommas :: String -> [String]
splitCommas [] = []
splitCommas x =
    let (y, z) = break (== ',') x
     in y : splitCommas (drop 1 z)
