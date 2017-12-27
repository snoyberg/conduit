{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}
-- | /NOTE/ It is recommended to start using "Data.Conduit.Combinators" instead
-- of this module.
--
-- Copyright: 2011 Michael Snoyman, 2010-2011 John Millikin
-- License: MIT
--
-- Handle streams of text.
--
-- Parts of this code were taken from enumerator and adapted for conduits.
--
-- For many purposes, it's recommended to use the conduit-combinators library,
-- which provides a more complete set of functions.
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
    , linesBounded
    , TextException (..)
    , takeWhile
    , dropWhile
    , take
    , drop
    , foldLines
    , withLine
    , CC.decodeUtf8
    , CC.decodeUtf8Lenient
    , CC.encodeUtf8
    , detectUtf
    ) where

import           Prelude hiding (head, drop, takeWhile, lines, zip, zip3, zipWith, zipWith3, take, dropWhile)

import qualified Control.Exception as Exc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word8)
import           Data.Typeable (Typeable)

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadThrow, throwM)
import Control.Monad (unless)
import Data.Streaming.Text

-- | A specific character encoding.
--
-- Since 0.3.0
data Codec = Codec
    { _codecName :: T.Text
    , codecEncode
        :: T.Text
        -> (B.ByteString, Maybe (TextException, T.Text))
    , codecDecode
        :: B.ByteString
        -> (T.Text, Either
            (TextException, B.ByteString)
            B.ByteString)
    }
    | NewCodec T.Text (T.Text -> B.ByteString) (B.ByteString -> DecodeResult)

instance Show Codec where
    showsPrec d c =
        let (cnst, name) = case c of
                Codec t _ _    -> ("Codec ", t)
                NewCodec t _ _ -> ("NewCodec ", t)
        in showParen (d > 10) $ showString cnst . shows name



-- | Emit each line separately
--
-- Since 0.4.1
lines :: Monad m => ConduitT T.Text T.Text m ()
lines =
    awaitText id
  where
    awaitText front = await >>= maybe (finish front) (process front)

    finish front =
      let t = T.concat $ front []
       in unless (T.null t) (yield t)

    process front text =
      let (line, rest) = T.break (== '\n') text
      in  case T.uncons rest of
            Just (_, rest') -> do
              yield (T.concat $ front [line])
              process id rest'
            Nothing -> Exc.assert (line == text) $ awaitText $ front . (line:)



-- | Variant of the lines function with an integer parameter.
-- The text length of any emitted line
-- never exceeds the value of the parameter. Whenever
-- this is about to happen a LengthExceeded exception
-- is thrown. This function should be used instead
-- of the lines function whenever we are dealing with
-- user input (e.g. a file upload) because we can't be sure that
-- user input won't have extraordinarily large lines which would
-- require large amounts of memory if consumed.
linesBounded :: MonadThrow m => Int -> ConduitT T.Text T.Text m ()
linesBounded maxLineLen =
    awaitText 0 T.empty
  where
    awaitText len buf = await >>= maybe (finish buf) (process len buf)

    finish buf = unless (T.null buf) (yield buf)

    process len buf text =
      let (line, rest) = T.break (== '\n') text
          len' = len + T.length line
      in  if len' > maxLineLen
            then lift $ throwM (LengthExceeded maxLineLen)
            else case T.uncons rest of
                   Just (_, rest') ->
                     yield (buf `T.append` line) >> process 0 T.empty rest'
                   _ ->
                     awaitText len' $ buf `T.append` text



-- | Convert text into bytes, using the provided codec. If the codec is
-- not capable of representing an input character, an exception will be thrown.
--
-- Since 0.3.0
encode :: MonadThrow m => Codec -> ConduitT T.Text B.ByteString m ()
encode (NewCodec _ enc _) = CL.map enc
encode codec = CL.mapM $ \t -> do
    let (bs, mexc) = codecEncode codec t
    maybe (return bs) (throwM . fst) mexc

decodeNew
    :: Monad m
    => (Int -> B.ByteString -> T.Text -> B.ByteString -> ConduitT B.ByteString T.Text m ())
    -> t
    -> Int
    -> (B.ByteString -> DecodeResult)
    -> ConduitT B.ByteString T.Text m ()
decodeNew onFailure _name =
    loop
  where
    loop consumed dec =
        await >>= maybe finish go
      where
        finish =
            case dec B.empty of
                DecodeResultSuccess _ _ -> return ()
                DecodeResultFailure t rest -> onFailure consumed B.empty t rest
        {-# INLINE finish #-}

        go bs | B.null bs = loop consumed dec
        go bs =
            case dec bs of
                DecodeResultSuccess t dec' -> do
                    let consumed' = consumed + B.length bs
                        next = do
                            unless (T.null t) (yield t)
                            loop consumed' dec'
                     in consumed' `seq` next
                DecodeResultFailure t rest -> onFailure consumed bs t rest

-- | Convert bytes into text, using the provided codec. If the codec is
-- not capable of decoding an input byte sequence, an exception will be thrown.
--
-- Since 0.3.0
decode :: MonadThrow m => Codec -> ConduitT B.ByteString T.Text m ()
decode (NewCodec name _ start) =
    decodeNew onFailure name 0 start
  where
    onFailure consumed bs t rest = do
        unless (T.null t) (yield t)
        leftover rest -- rest will never be null, no need to check
        let consumed' = consumed + B.length bs - B.length rest
        throwM $ NewDecodeException name consumed' (B.take 4 rest)
    {-# INLINE onFailure #-}
decode codec =
    loop id
  where
    loop front = await >>= maybe (finish front) (go front)

    finish front =
        case B.uncons $ front B.empty of
            Nothing -> return ()
            Just (w, _) -> lift $ throwM $ DecodeException codec w

    go front bs' =
        case extra of
            Left (exc, _) -> lift $ throwM exc
            Right bs'' -> yield text >> loop (B.append bs'')
      where
        (text, extra) = codecDecode codec bs
        bs = front bs'

-- |
-- Since 0.3.0
data TextException = DecodeException Codec Word8
                   | EncodeException Codec Char
                   | LengthExceeded Int
                   | TextException Exc.SomeException
                   | NewDecodeException !T.Text !Int !B.ByteString
    deriving Typeable
instance Show TextException where
    show (DecodeException codec w) = concat
        [ "Error decoding legacy Data.Conduit.Text codec "
        , show codec
        , " when parsing byte: "
        , show w
        ]
    show (EncodeException codec c) = concat
        [ "Error encoding legacy Data.Conduit.Text codec "
        , show codec
        , " when parsing char: "
        , show c
        ]
    show (LengthExceeded i) = "Data.Conduit.Text.linesBounded: line too long: " ++ show i
    show (TextException se) = "Data.Conduit.Text.TextException: " ++ show se
    show (NewDecodeException codec consumed next) = concat
        [ "Data.Conduit.Text.decode: Error decoding stream of "
        , T.unpack codec
        , " bytes. Error encountered in stream at offset "
        , show consumed
        , ". Encountered at byte sequence "
        , show next
        ]
instance Exc.Exception TextException

-- |
-- Since 0.3.0
utf8 :: Codec
utf8 = NewCodec (T.pack "UTF-8") TE.encodeUtf8 Data.Streaming.Text.decodeUtf8

-- |
-- Since 0.3.0
utf16_le :: Codec
utf16_le = NewCodec (T.pack "UTF-16-LE") TE.encodeUtf16LE decodeUtf16LE

-- |
-- Since 0.3.0
utf16_be :: Codec
utf16_be = NewCodec (T.pack "UTF-16-BE") TE.encodeUtf16BE decodeUtf16BE

-- |
-- Since 0.3.0
utf32_le :: Codec
utf32_le = NewCodec (T.pack "UTF-32-LE") TE.encodeUtf32LE decodeUtf32LE

-- |
-- Since 0.3.0
utf32_be :: Codec
utf32_be = NewCodec (T.pack "UTF-32-BE") TE.encodeUtf32BE decodeUtf32BE

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

-- |
--
-- Since 1.0.8
takeWhile :: Monad m
          => (Char -> Bool)
          -> ConduitT T.Text T.Text m ()
takeWhile p =
    loop
  where
    loop = await >>= maybe (return ()) go
    go t =
        case T.span p t of
            (x, y)
                | T.null y -> yield x >> loop
                | otherwise -> yield x >> leftover y

-- |
--
-- Since 1.0.8
dropWhile :: Monad m
          => (Char -> Bool)
          -> ConduitT T.Text o m ()
dropWhile p =
    loop
  where
    loop = await >>= maybe (return ()) go
    go t
        | T.null x = loop
        | otherwise = leftover x
      where
        x = T.dropWhile p t

-- |
--
-- Since 1.0.8
take :: Monad m => Int -> ConduitT T.Text T.Text m ()
take =
    loop
  where
    loop i = await >>= maybe (return ()) (go i)
    go i t
        | diff == 0 = yield t
        | diff < 0 =
            let (x, y) = T.splitAt i t
             in yield x >> leftover y
        | otherwise = yield t >> loop diff
      where
        diff = i - T.length t

-- |
--
-- Since 1.0.8
drop :: Monad m => Int -> ConduitT T.Text o m ()
drop =
    loop
  where
    loop i = await >>= maybe (return ()) (go i)
    go i t
        | diff == 0 = return ()
        | diff < 0 = leftover $ T.drop i t
        | otherwise = loop diff
      where
        diff = i - T.length t

-- |
--
-- Since 1.0.8
foldLines :: Monad m
          => (a -> ConduitM T.Text o m a)
          -> a
          -> ConduitT T.Text o m a
foldLines f =
    start
  where
    start a = CL.peek >>= maybe (return a) (const $ loop $ f a)

    loop consumer = do
        a <- takeWhile (/= '\n') .| do
            a <- CL.map (T.filter (/= '\r')) .| consumer
            CL.sinkNull
            return a
        drop 1
        start a

-- |
--
-- Since 1.0.8
withLine :: Monad m
         => ConduitT T.Text Void m a
         -> ConduitT T.Text o m (Maybe a)
withLine consumer = toConsumer $ do
    mx <- CL.peek
    case mx of
        Nothing -> return Nothing
        Just _ -> do
            x <- takeWhile (/= '\n') .| do
                x <- CL.map (T.filter (/= '\r')) .| consumer
                CL.sinkNull
                return x
            drop 1
            return $ Just x

-- | Automatically determine which UTF variant is being used. This function
-- checks for BOMs, removing them as necessary. It defaults to assuming UTF-8.
--
-- Since 1.1.9
detectUtf :: MonadThrow m => ConduitT B.ByteString T.Text m ()
detectUtf =
    go id
  where
    go front = await >>= maybe (close front) (push front)

    push front bs'
        | B.length bs < 4 = go $ B.append bs
        | otherwise       = leftDecode bs
      where bs = front bs'

    close front = leftDecode $ front B.empty

    leftDecode bs = leftover bsOut >> decode codec
      where
        bsOut = B.append (B.drop toDrop x) y
        (x, y) = B.splitAt 4 bs
        (toDrop, codec) =
            case B.unpack x of
                [0x00, 0x00, 0xFE, 0xFF] -> (4, utf32_be)
                [0xFF, 0xFE, 0x00, 0x00] -> (4, utf32_le)
                0xFE : 0xFF: _           -> (2, utf16_be)
                0xFF : 0xFE: _           -> (2, utf16_le)
                0xEF : 0xBB: 0xBF : _    -> (3, utf8)
                _                        -> (0, utf8) -- Assuming UTF-8
{-# INLINE detectUtf #-}
