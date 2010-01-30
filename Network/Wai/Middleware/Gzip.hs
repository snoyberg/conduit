{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Network.Wai.Enumerator (mapE)
import Codec.Compression.GZip (compress)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOneOf)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

-- | Use gzip to compress the body of the response.
--
-- Analyzes the \"Accept-Encoding\" header from the client to determine
-- if gzip is supported.
--
-- Possible future enhancements:
--
-- * Only compress if the response is above a certain size.
--
-- * Add Content-Length.
--
-- * I read somewhere that \"the beast\" (MSIE) can\'t support compression
-- for Javascript files..
gzip :: Middleware
gzip app env = do
    res <- app env
    case body res of
        Left _ -> return res
        Right _ -> do
            let enc = fromMaybe []
                    $ (splitOneOf "," . B.unpack)
                    `fmap` lookup AcceptEncoding
                      (httpHeaders env)
            if "gzip" `elem` enc
                then return res
                    { body = compressE $ body res
                    , headers = (ContentEncoding, B.pack "gzip")
                              : headers res
                    }
                else return res

compressE :: Either FilePath (Enumerator a) -> Either FilePath (Enumerator a)
compressE = either (Left . id) (Right . mapE compress')

-- | Note: this might lose a lot of the compression power by dealing with
-- smaller chunks, I'm not certain.
compress' :: B.ByteString -> B.ByteString
compress' bs = B.concat $ L.toChunks $ compress $ L.fromChunks [bs]
