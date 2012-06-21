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
    go S.empty
  where
    go bs = awaitE >>= either (close bs) (push bs)

    close bs r = do
        unless (S.null bs) $ yield $ f bs
        return r

    push bs1 bs2 | S.length bs1 + S.length bs2 < size = go (S.append bs1 bs2)
    push bs1 bs2 = assert (S.length bs1 < size) $ do
        bs <-
            case S.length bs1 `mod` size of
                0 -> return bs2
                m -> do
                    let (x, y) = S.splitAt (size - m) bs2
                        bs1' = S.append bs1 x
                    assert (S.length bs1' `mod` size == 0) $ yield $ f bs1'
                    return y
        let (x, y) = S.splitAt (size * (S.length bs `div` size)) bs
        unless (S.null x) $ yield $ f x
        go y
