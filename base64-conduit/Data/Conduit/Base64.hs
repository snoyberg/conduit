module Data.Conduit.Base64
    ( encode
    , decode
    , encodeURL
    , decodeURL
    ) where

import Control.Monad (unless)
import Control.Exception (assert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U

import Data.Conduit

encode :: Monad m => GInfConduit ByteString m ByteString
encode = codeWith 3 B64.encode

decode :: Monad m => GInfConduit ByteString m ByteString
decode = codeWith 4 B64.decodeLenient

encodeURL :: Monad m => GInfConduit ByteString m ByteString
encodeURL = codeWith 3 B64U.encode

decodeURL :: Monad m => GInfConduit ByteString m ByteString
decodeURL = codeWith 4 B64U.decodeLenient

codeWith :: Monad m => Int -> (ByteString -> ByteString) -> GInfConduit ByteString m ByteString
codeWith size f =
    loop
  where
    loop = awaitE >>= either return push

    loopWith bs
        | S.null bs = loop
        | otherwise = awaitE >>= either (\r -> yield (f bs) >> return r) (pushWith bs)

    push bs = do
        let (x, y) = S.splitAt (len - (len `mod` size)) bs
        unless (S.null x) $ yield $ f x
        loopWith y
      where
        len = S.length bs

    pushWith bs1 bs2 | S.length bs1 + S.length bs2 < size = loopWith (S.append bs1 bs2)
    pushWith bs1 bs2 = assertion1 $ assertion2 $ do
        yield $ f bs1'
        push y
      where
        m = S.length bs1 `mod` size
        (x, y) = S.splitAt (size - m) bs2
        bs1' = S.append bs1 x

        assertion1 = assert $ S.length bs1 < size
        assertion2 = assert $ S.length bs1' `mod` size == 0
