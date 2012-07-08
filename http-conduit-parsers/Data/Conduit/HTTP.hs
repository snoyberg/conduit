{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit.HTTP
    ( headerLines
    , HttpParseException (..)
    ) where

import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.Conduit

data HttpParseException = OverLargeHeader
                        | IncompleteHeaders
    deriving (Show, Typeable)
instance Exception HttpParseException

headerLines :: MonadThrow m
            => Int -- ^ maximum byte count
            -> GLConduit ByteString m ByteString
headerLines =
    go id
  where
    go _ remaining | remaining <= 0 = lift $ monadThrow OverLargeHeader
    go front remaining = await >>= maybe
        (lift $ monadThrow IncompleteHeaders)
        (push' front remaining)

    push' front remaining bs = push (remaining - S.length bs) $ front bs

    push remaining bs =
        case S.elemIndex 10 bs of
            Nothing -> go (S.append bs) remaining
            Just nl -> do
                let here = killCR $ S.take nl bs
                    rest = S.drop (nl + 1) bs
                checkMulti remaining here rest

    checkMulti remaining here rest
        | S.null here = leftover rest
        | S.null rest = await >>= maybe (lift $ monadThrow IncompleteHeaders) (checkMulti remaining here)
        | S.head rest == 9 || S.head rest == 32 = push remaining $ S.append here rest
        | otherwise = yield here >> push remaining rest

killCR :: ByteString -> ByteString
killCR bs
    | S.null bs = bs
    | S.last bs == 13 = S.init bs
    | otherwise = bs
