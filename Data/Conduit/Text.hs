{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module: Data.Enumerator.Text
-- Copyright: 2010-2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Character-oriented alternatives to "Data.Enumerator.List". Note that the
-- enumeratees in this module must unpack their inputs to work properly. If
-- you do not need to handle leftover input on a char-by-char basis, the
-- chunk-oriented versions will be much faster.
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Data.Enumerator.Text as ET
-- @
--
-- Since: 0.2
module Data.Conduit.Text
    (

{-
    -- * IO
      sourceHandle
    , enumFile
    , iterHandle

    -- * List analogues

    -- ** Folds
    , fold
    , foldM

    -- ** Maps
    , Data.Enumerator.Text.map
    , Data.Enumerator.Text.mapM
    , Data.Enumerator.Text.mapM_
    , Data.Enumerator.Text.concatMap
    , concatMapM

    -- ** Accumulating maps
    , mapAccum
    , mapAccumM
    , concatMapAccum
    , concatMapAccumM

    -- ** Infinite streams
    , Data.Enumerator.Text.iterate
    , iterateM
    , Data.Enumerator.Text.repeat
    , repeatM

    -- ** Bounded streams
    , Data.Enumerator.Text.replicate
    , replicateM
    , generateM
    , unfold
    , unfoldM

    -- ** Dropping input
    , Data.Enumerator.Text.drop
    , Data.Enumerator.Text.dropWhile
    , Data.Enumerator.Text.filter
    , filterM

    -- ** Consumers
    , Data.Enumerator.Text.head
    , head_
    , Data.Enumerator.Text.take
    , takeWhile
    , consume

    -- ** Zipping
    , zip
    , zip3
    , zip4
    , zip5
    , zip6
    , zip7
    , zipWith
    , zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , zipWith7

    -- ** Unsorted
    , require
    , isolate
    , isolateWhile
    , splitWhen
    , lines
-}

    -- * Text codecs
      Codec
    , encode
    , decode
    , utf8
    , utf16_le
    , utf16_be
    , utf32_le
    , utf32_be
    , ascii
    , iso8859_1

    ) where

import qualified Prelude
import           Prelude hiding (head, drop, takeWhile, lines, zip, zip3, zipWith, zipWith3)

import           Control.Arrow (first)
import qualified Control.Exception as Exc
import           Control.Monad.Trans.Class (lift)
import           Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (ord)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word8, Word16)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Typeable (Typeable)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (ResourceThrow (..))

data Codec = Codec
    { codecName :: T.Text
    , codecEncode
        :: T.Text
        -> (B.ByteString, Maybe (TextException, T.Text))
    , codecDecode
        :: B.ByteString
        -> (T.Text, Either
            (TextException, B.ByteString)
            B.ByteString)
    }

instance Show Codec where
    showsPrec d c = showParen (d > 10) $
        showString "Codec " . shows (codecName c)

-- | Convert text into bytes, using the provided codec. If the codec is
-- not capable of representing an input character, an error will be thrown.
--
-- Since: 0.2
encode :: ResourceThrow m => Codec -> C.Conduit T.Text m B.ByteString
encode codec = CL.mapM $ \t -> do
    let (bs, mexc) = codecEncode codec t
    maybe (return bs) (resourceThrow . fst) mexc


-- | Convert bytes into text, using the provided codec. If the codec is
-- not capable of decoding an input byte sequence, an error will be thrown.
--
-- Since: 0.2
decode :: ResourceThrow m => Codec -> C.Conduit B.ByteString m T.Text
decode codec = C.conduitState
    Nothing
    push
    close
  where
    push mb input = do
        (mb', ts) <- go' mb input
        return $ (mb', C.ConduitResult C.Processing ts)
    close mb =
        case mb of
            Nothing -> return $ C.ConduitResult [] []
            Just b
                | B.null b -> error "Data.Conduit.Text.decode: Received a null chunk"
                | otherwise -> lift $ resourceThrow $ DecodeException codec (B.head b)

    go' mb input = do
        let bss = maybe id (:) mb input
        either (lift . resourceThrow) return $ go bss id

    go [] front = Right (Nothing, front [])
    go (x:xs) front
        | B.null x = go xs front
    go (x:xs) front =
        case extra of
            Left (exc, _) -> Left exc
            Right bs
                | B.null bs -> go xs front'
                | otherwise ->
                    case xs of
                        y:ys -> go (B.append bs y:ys) front'
                        [] -> Right (Just bs, front' [])
      where
        (text, extra) = codecDecode codec x
        front' = front . (text:)

data TextException = DecodeException Codec Word8
                   | EncodeException Codec Char
    deriving (Show, Typeable)
instance Exc.Exception TextException

byteSplits :: B.ByteString
           -> [(B.ByteString, B.ByteString)]
byteSplits bytes = loop (B.length bytes) where
    loop 0 = [(B.empty, bytes)]
    loop n = B.splitAt n bytes : loop (n - 1)

splitSlowly :: (B.ByteString -> T.Text)
            -> B.ByteString
            -> (T.Text, Either
                (TextException, B.ByteString)
                B.ByteString)
splitSlowly dec bytes = valid where
    valid = firstValid (Prelude.map decFirst splits)
    splits = byteSplits bytes
    firstValid = Prelude.head . catMaybes
    tryDec = tryEvaluate . dec

    decFirst (a, b) = case tryDec a of
        Left _ -> Nothing
        Right text -> Just (text, case tryDec b of
            Left exc -> Left (exc, b)

            -- this case shouldn't occur, since splitSlowly
            -- is only called when parsing failed somewhere
            Right _ -> Right B.empty)

utf8 :: Codec
utf8 = Codec name enc dec where
    name = T.pack "UTF-8"
    enc text = (TE.encodeUtf8 text, Nothing)
    dec bytes = case splitQuickly bytes of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf8 bytes

    splitQuickly bytes = loop 0 >>= maybeDecode where
        required x0
            | x0 .&. 0x80 == 0x00 = 1
            | x0 .&. 0xE0 == 0xC0 = 2
            | x0 .&. 0xF0 == 0xE0 = 3
            | x0 .&. 0xF8 == 0xF0 = 4

            -- Invalid input; let Text figure it out
            | otherwise           = 0

        maxN = B.length bytes

        loop n | n == maxN = Just (TE.decodeUtf8 bytes, B.empty)
        loop n = let
            req = required (B.index bytes n)
            tooLong = first TE.decodeUtf8 (B.splitAt n bytes)
            decodeMore = loop $! n + req
            in if req == 0
                then Nothing
                else if n + req > maxN
                    then Just tooLong
                    else decodeMore

utf16_le :: Codec
utf16_le = Codec name enc dec where
    name = T.pack "UTF-16-LE"
    enc text = (TE.encodeUtf16LE text, Nothing)
    dec bytes = case splitQuickly bytes of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf16LE bytes

    splitQuickly bytes = maybeDecode (loop 0) where
        maxN = B.length bytes

        loop n |  n      == maxN = decodeAll
               | (n + 1) == maxN = decodeTo n
        loop n = let
            req = utf16Required
                (B.index bytes n)
                (B.index bytes (n + 1))
            decodeMore = loop $! n + req
            in if n + req > maxN
                then decodeTo n
                else decodeMore

        decodeTo n = first TE.decodeUtf16LE (B.splitAt n bytes)
        decodeAll = (TE.decodeUtf16LE bytes, B.empty)

utf16_be :: Codec
utf16_be = Codec name enc dec where
    name = T.pack "UTF-16-BE"
    enc text = (TE.encodeUtf16BE text, Nothing)
    dec bytes = case splitQuickly bytes of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf16BE bytes

    splitQuickly bytes = maybeDecode (loop 0) where
        maxN = B.length bytes

        loop n |  n      == maxN = decodeAll
               | (n + 1) == maxN = decodeTo n
        loop n = let
            req = utf16Required
                (B.index bytes (n + 1))
                (B.index bytes n)
            decodeMore = loop $! n + req
            in if n + req > maxN
                then decodeTo n
                else decodeMore

        decodeTo n = first TE.decodeUtf16BE (B.splitAt n bytes)
        decodeAll = (TE.decodeUtf16BE bytes, B.empty)

utf16Required :: Word8 -> Word8 -> Int
utf16Required x0 x1 = required where
    required = if x >= 0xD800 && x <= 0xDBFF
        then 4
        else 2
    x :: Word16
    x = (fromIntegral x1 `shiftL` 8) .|. fromIntegral x0

utf32_le :: Codec
utf32_le = Codec name enc dec where
    name = T.pack "UTF-32-LE"
    enc text = (TE.encodeUtf32LE text, Nothing)
    dec bs = case utf32SplitBytes TE.decodeUtf32LE bs of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf32LE bs

utf32_be :: Codec
utf32_be = Codec name enc dec where
    name = T.pack "UTF-32-BE"
    enc text = (TE.encodeUtf32BE text, Nothing)
    dec bs = case utf32SplitBytes TE.decodeUtf32BE bs of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf32BE bs

utf32SplitBytes :: (B.ByteString -> T.Text)
                -> B.ByteString
                -> Maybe (T.Text, B.ByteString)
utf32SplitBytes dec bytes = split where
    split = maybeDecode (dec toDecode, extra)
    len = B.length bytes
    lenExtra = mod len 4

    lenToDecode = len - lenExtra
    (toDecode, extra) = if lenExtra == 0
        then (bytes, B.empty)
        else B.splitAt lenToDecode bytes

ascii :: Codec
ascii = Codec name enc dec where
    name = T.pack "ASCII"
    enc text = (bytes, extra) where
        (safe, unsafe) = T.span (\c -> ord c <= 0x7F) text
        bytes = B8.pack (T.unpack safe)
        extra = if T.null unsafe
            then Nothing
            else Just (EncodeException ascii (T.head unsafe), unsafe)

    dec bytes = (text, extra) where
        (safe, unsafe) = B.span (<= 0x7F) bytes
        text = T.pack (B8.unpack safe)
        extra = if B.null unsafe
            then Right B.empty
            else Left (DecodeException ascii (B.head unsafe), unsafe)

iso8859_1 :: Codec
iso8859_1 = Codec name enc dec where
    name = T.pack "ISO-8859-1"
    enc text = (bytes, extra) where
        (safe, unsafe) = T.span (\c -> ord c <= 0xFF) text
        bytes = B8.pack (T.unpack safe)
        extra = if T.null unsafe
            then Nothing
            else Just (EncodeException iso8859_1 (T.head unsafe), unsafe)

    dec bytes = (T.pack (B8.unpack bytes), Right B.empty)

tryEvaluate :: a -> Either TextException a
tryEvaluate = unsafePerformIO . Exc.try . Exc.evaluate

maybeDecode:: (a, b) -> Maybe (a, b)
maybeDecode (a, b) = case tryEvaluate a of
    Left _ -> Nothing
    Right _ -> Just (a, b)
