module Data.Conduit.Base64
    ( encode
    , decode
    , encodeURL
    , decodeURL
    ) where

import Control.Monad (unless)
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
    go id
  where
    go front = awaitE >>= either (close front) (push front)

    close front r = do
        unless (S.null bs) $ yield $ f bs
        return r
      where
        bs = front S.empty

    push front bs' = do
        unless (S.null x) $ yield $ f x
        go $ S.append y
      where
        bs = front bs'
        (x, y) = S.splitAt (size * (S.length bs `div` size)) bs
