{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Copyright: 2011 Michael Snoyman, 2010-2011 John Millikin
-- License: MIT
--
-- Handle streams of text.
--
-- Parts of this code were taken from enumerator and adapted for conduits.
module Data.Conduit.Text
    (

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
    , lines

    ) where

import qualified Prelude
import           Prelude hiding (head, drop, takeWhile, lines, zip, zip3, zipWith, zipWith3)

import           Control.Arrow (first)
import qualified Control.Exception as Exc
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

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Control.Monad (unless)
import Control.Applicative ((<$>))

-- | A specific character encoding.
--
-- Since 0.3.0
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

-- | Emit each line separately
--
-- Since 0.4.1
lines :: Monad m => Conduit T.Text m T.Text
lines =
    loop id
  where
    loop front = do
        mbs <- await
        case mbs of
            Nothing ->
                let final = front T.empty
                 in unless (T.null final) $ yield final
            Just bs -> go front bs

    go sofar more =
        case T.uncons second of
            Just (_, second') -> yield (sofar first') >> go id second'
            Nothing ->
                let rest = sofar more
                 in loop $ T.append rest
      where
        (first', second) = T.break (== '\n') more

-- | Convert text into bytes, using the provided codec. If the codec is
-- not capable of representing an input character, an exception will be thrown.
--
-- Since 0.3.0
encode :: MonadThrow m => Codec -> Conduit T.Text m B.ByteString
encode codec = CL.mapM $ \t -> do
    let (bs, mexc) = codecEncode codec t
    maybe (return bs) (monadThrow . fst) mexc


-- | Convert bytes into text, using the provided codec. If the codec is
-- not capable of decoding an input byte sequence, an exception will be thrown.
--
-- Since 0.3.0
decode :: MonadThrow m => Codec -> Conduit B.ByteString m T.Text
decode codec =
    loop id
  where
    loop front = do
        mbs <- await
        case front <$> mbs of
            Nothing ->
                case B.uncons $ front B.empty of
                    Nothing -> return ()
                    Just (w, _) -> lift $ monadThrow $ DecodeException codec w
            Just bs -> go bs

    go bs =
        case extra of
            Left (exc, _) -> lift $ monadThrow exc
            Right bs' -> yield text >> loop (B.append bs')
      where
        (text, extra) = codecDecode codec bs

-- |
-- Since 0.3.0
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

-- |
-- Since 0.3.0
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

-- |
-- Since 0.3.0
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

-- |
-- Since 0.3.0
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

-- |
-- Since 0.3.0
utf32_le :: Codec
utf32_le = Codec name enc dec where
    name = T.pack "UTF-32-LE"
    enc text = (TE.encodeUtf32LE text, Nothing)
    dec bs = case utf32SplitBytes TE.decodeUtf32LE bs of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf32LE bs

-- |
-- Since 0.3.0
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

-- |
-- Since 0.3.0
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

-- |
-- Since 0.3.0
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
