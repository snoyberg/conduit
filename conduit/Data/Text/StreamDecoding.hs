{-# LANGUAGE BangPatterns, CPP, Rank2Types #-}

-- |
-- Module      : Data.Text.Lazy.Encoding.Fusion
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fusible 'Stream'-oriented functions for converting between lazy
-- 'Text' and several common encodings.

-- | Provides a stream-based approach to decoding Unicode data. Each function
-- below works the same way: you give it a chunk of data, and it gives back a
-- @DecodeResult@. If the parse was a success, then you get a chunk of @Text@
-- (possibly empty) and a continuation parsing function. If the parse was a
-- failure, you get a chunk of successfully decoded @Text@ (possibly empty) and
-- the unconsumed bytes.
--
-- In order to indicate end of stream, you pass an empty @ByteString@ to the
-- decode function. This call may result in a failure, if there were unused
-- bytes left over from a previous step which formed part of a code sequence.
module Data.Text.StreamDecoding
    (
    -- * Streaming
    --  streamASCII
      streamUtf8
    , streamUtf16LE
    , streamUtf16BE
    , streamUtf32LE
    , streamUtf32BE

    -- * Unstreaming
    --, unstream

    , DecodeResult (..)
    ) where

import qualified Data.Text.Array as A
import Control.Monad.ST
import Control.Exception (throw)
import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Text.Internal (textP)
import Data.Text.Encoding.Error
import Data.Text.Internal.Unsafe.Char (unsafeChr, unsafeChr8, unsafeChr32)
import Data.Text.Internal.Unsafe.Shift (shiftL)
import Data.Word (Word8, Word16, Word32)
import qualified Data.Text.Internal.Encoding.Utf8 as U8
import qualified Data.Text.Internal.Encoding.Utf16 as U16
import qualified Data.Text.Internal.Encoding.Utf32 as U32
#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text.Internal.Unsafe.Char (ord, unsafeWrite)

data S = S0
       | S1 {-# UNPACK #-} !Word8
       | S2 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
       | S3 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
       | S4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    deriving Show

data DecodeResult
    = DecodeResultSuccess !Text !(B.ByteString -> DecodeResult)
    | DecodeResultFailure !Text !B.ByteString

data Status s = Status
    !Int -- ^ position in ByteString
    !Int -- ^ position in Array
    !Int -- ^ array size
    !(A.MArray s)
    !B.ByteString
    !S

toBS :: S -> B.ByteString
toBS S0 = B.empty
toBS (S1 a) = B.pack [a]
toBS (S2 a b) = B.pack [a, b]
toBS (S3 a b c) = B.pack [a, b, c]
toBS (S4 a b c d) = B.pack [a, b, c, d]

getText :: Int -> A.MArray s -> ST s Text
getText j marr = do
    arr <- A.unsafeFreeze marr
    return $! textP arr 0 j

addChar :: Status s
        -> (Status s -> ST s b)
        -> Int -- ^ delta of i
        -> Char
        -> ST s b
addChar (Status i j size marr ps _) next deltai c
    | tooSmall >= size = do
        let size' = (size + 1) `shiftL` 1
        marr' <- A.new size'
        A.copyM marr' 0 marr 0 size
        addChar (Status i j size' marr' ps S0) next deltai c
    | otherwise = do
        d <- unsafeWrite marr j c
        next (Status (i + deltai) (j + d) size marr ps S0)
  where
    tooSmall
        | ord c < 0x10000 = j + 1
        | otherwise       = j

handleNull :: (B.ByteString -> DecodeResult)
           -> (forall s. Status s -> ST s DecodeResult)
           -> S
           -> B.ByteString
           -> DecodeResult
handleNull stream f s bs
    | B.null bs =
        case s of
            S0 -> DecodeResultSuccess T.empty stream
            _  -> DecodeResultFailure T.empty $ toBS s
    | otherwise = runST $ do
        let initLen = B.length bs
        marr <- A.new initLen
        f $! Status 0 0 initLen marr bs s

consume :: Status s
        -> (Status s -> ST s DecodeResult)
        -> (S -> B.ByteString -> DecodeResult)
        -> ST s DecodeResult
consume (Status i j size marr ps s) next streamStart
    | i >= B.length ps = do
        t <- getText j marr
        return $! DecodeResultSuccess t (streamStart s)
    | otherwise =
  case s of
    S0         -> next' (S1 x)
    S1 a       -> next' (S2 a x)
    S2 a b     -> next' (S3 a b x)
    S3 a b c   -> next' (S4 a b c x)
    S4 a b c d -> do
        t <- getText j marr
        let ps' = B.pack [a, b, c, d] `B.append` ps
        return $! DecodeResultFailure t ps'
    where
        x = B.unsafeIndex ps i
        next' s' = next (Status (i + 1) j size marr ps s')

-- | /O(n)/ Convert a lazy 'ByteString' into a 'Stream Char', using
-- UTF-8 encoding.
streamUtf8 :: B.ByteString -> DecodeResult
streamUtf8 = streamUtf8Start S0

streamUtf8Start :: S -> B.ByteString -> DecodeResult
streamUtf8Start =
    handleNull streamUtf8 next
  where
    next st@(Status i _j _size _marr ps S0)
      | i < len && U8.validate1 a           = addChar' 1 (unsafeChr8 a)
      | i + 1 < len && U8.validate2 a b     = addChar' 2 (U8.chr2 a b)
      | i + 2 < len && U8.validate3 a b c   = addChar' 3 (U8.chr3 a b c)
      | i + 3 < len && U8.validate4 a b c d = addChar' 4 (U8.chr4 a b c d)
      where len = B.length ps
            a = B.unsafeIndex ps i
            b = B.unsafeIndex ps (i+1)
            c = B.unsafeIndex ps (i+2)
            d = B.unsafeIndex ps (i+3)
            addChar' = addChar st next
    next st@(Status _i _j _size _marr _ps s) =
      case s of
        S1 a       | U8.validate1 a       -> addChar' (unsafeChr8 a)
        S2 a b     | U8.validate2 a b     -> addChar' (U8.chr2 a b)
        S3 a b c   | U8.validate3 a b c   -> addChar' (U8.chr3 a b c)
        S4 a b c d | U8.validate4 a b c d -> addChar' (U8.chr4 a b c d)
        _ -> consume st next streamUtf8Start
       where addChar' = addChar st next 0
{-# INLINE [0] streamUtf8 #-}
{-# INLINE [0] streamUtf8Start #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using little
-- endian UTF-16 encoding.
streamUtf16LE :: B.ByteString -> DecodeResult
streamUtf16LE = streamUtf16LEStart S0

streamUtf16LEStart :: S -> B.ByteString -> DecodeResult
streamUtf16LEStart =
    handleNull streamUtf16LE next
  where
    next st@(Status i _j _size _marr ps S0)
      | i + 1 < len && U16.validate1 x1    = addChar st next 2 (unsafeChr x1)
      | i + 3 < len && U16.validate2 x1 x2 = addChar st next 4 (U16.chr2 x1 x2)
      where len = B.length ps
            x1   = c (idx  i)      (idx (i + 1))
            x2   = c (idx (i + 2)) (idx (i + 3))
            c w1 w2 = w1 + (w2 `shiftL` 8)
            idx = fromIntegral . B.unsafeIndex ps :: Int -> Word16
    next st@(Status _i _j _size _marr _bs s) =
      case s of
        S2 w1 w2       | U16.validate1 (c w1 w2)           ->
            addChar st next 0 (unsafeChr (c w1 w2))
        S4 w1 w2 w3 w4 | U16.validate2 (c w1 w2) (c w3 w4) ->
            addChar st next 0 (U16.chr2 (c w1 w2) (c w3 w4))
        _ -> consume st next streamUtf16LEStart
       where c :: Word8 -> Word8 -> Word16
             c w1 w2 = fromIntegral w1 + (fromIntegral w2 `shiftL` 8)
{-# INLINE [0] streamUtf16LE #-}
{-# INLINE [0] streamUtf16LEStart #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using big
-- endian UTF-16 encoding.
streamUtf16BE :: B.ByteString -> DecodeResult
streamUtf16BE = streamUtf16BEStart S0

streamUtf16BEStart :: S -> B.ByteString -> DecodeResult
streamUtf16BEStart =
    handleNull streamUtf16BE next
  where
    next st@(Status i _j _size _marr ps S0)
      | i + 1 < len && U16.validate1 x1    = addChar st next 2 (unsafeChr x1)
      | i + 3 < len && U16.validate2 x1 x2 = addChar st next 4 (U16.chr2 x1 x2)
      where len = B.length ps
            x1   = c (idx  i)      (idx (i + 1))
            x2   = c (idx (i + 2)) (idx (i + 3))
            c w1 w2 = (w1 `shiftL` 8) + w2
            idx = fromIntegral . B.unsafeIndex ps :: Int -> Word16
    next st@(Status _i _j _size _marr _ps s) =
      case s of
        S2 w1 w2       | U16.validate1 (c w1 w2)           ->
          addChar st next 0 (unsafeChr (c w1 w2))
        S4 w1 w2 w3 w4 | U16.validate2 (c w1 w2) (c w3 w4) ->
          addChar st next 0 (U16.chr2 (c w1 w2) (c w3 w4))
        _ -> consume st next streamUtf16BEStart
       where c :: Word8 -> Word8 -> Word16
             c w1 w2 = (fromIntegral w1 `shiftL` 8) + fromIntegral w2
{-# INLINE [0] streamUtf16BE #-}
{-# INLINE [0] streamUtf16BEStart #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using big
-- endian UTF-32 encoding.
streamUtf32BE :: B.ByteString -> DecodeResult
streamUtf32BE = streamUtf32BEStart S0

streamUtf32BEStart :: S -> B.ByteString -> DecodeResult
streamUtf32BEStart =
    handleNull streamUtf32BE next
  where
    next st@(Status i _j _size _marr ps S0)
      | i + 3 < len && U32.validate x =
          addChar st next 4 (unsafeChr32 x)
      where len = B.length ps
            x = shiftL x1 24 + shiftL x2 16 + shiftL x3 8 + x4
            x1    = idx i
            x2    = idx (i+1)
            x3    = idx (i+2)
            x4    = idx (i+3)
            idx = fromIntegral . B.unsafeIndex ps :: Int -> Word32
    next st@(Status _i _j _size _marr _ps s) =
      case s of
        S4 w1 w2 w3 w4 | U32.validate (c w1 w2 w3 w4) ->
          addChar st next 0 (unsafeChr32 (c w1 w2 w3 w4))
        _ -> consume st next streamUtf32BEStart
       where c :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
             c w1 w2 w3 w4 = shifted
              where
               shifted = shiftL x1 24 + shiftL x2 16 + shiftL x3 8 + x4
               x1 = fromIntegral w1
               x2 = fromIntegral w2
               x3 = fromIntegral w3
               x4 = fromIntegral w4
{-# INLINE [0] streamUtf32BE #-}
{-# INLINE [0] streamUtf32BEStart #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using little
-- endian UTF-32 encoding.
streamUtf32LE :: B.ByteString -> DecodeResult
streamUtf32LE = streamUtf32LEStart S0

streamUtf32LEStart :: S -> B.ByteString -> DecodeResult
streamUtf32LEStart =
    handleNull streamUtf32LE next
  where
    next st@(Status i _j _size _marr ps S0)
      | i + 3 < len && U32.validate x =
          addChar st next 4 (unsafeChr32 x)
      where len = B.length ps
            x = shiftL x4 24 + shiftL x3 16 + shiftL x2 8 + x1
            x1    = idx i
            x2    = idx (i+1)
            x3    = idx (i+2)
            x4    = idx (i+3)
            idx = fromIntegral . B.unsafeIndex ps :: Int -> Word32
    next st@(Status _i _j _size _marr _ps s) =
      case s of
        S4 w1 w2 w3 w4 | U32.validate (c w1 w2 w3 w4) ->
          addChar st next 0 (unsafeChr32 (c w1 w2 w3 w4))
        _ -> consume st next streamUtf32LEStart
       where c :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
             c w1 w2 w3 w4 = shifted
              where
               shifted = shiftL x4 24 + shiftL x3 16 + shiftL x2 8 + x1
               x1 = fromIntegral w1
               x2 = fromIntegral w2
               x3 = fromIntegral w3
               x4 = fromIntegral w4
{-# INLINE [0] streamUtf32LE #-}
{-# INLINE [0] streamUtf32LEStart #-}
