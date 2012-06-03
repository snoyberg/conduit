{-# LANGUAGE FlexibleContexts #-}
-- | Streaming compression and decompression using conduits.
--
-- Parts of this code were taken from zlib-enum and adapted for conduits.
module Data.Conduit.Zlib (
    -- * Conduits
    compress, decompress, gzip, ungzip,
    -- * Flushing
    compressFlush, decompressFlush,
    -- * Re-exported from zlib-bindings
    WindowBits (..), defaultWindowBits
) where

import Codec.Zlib
import Data.Conduit hiding (unsafeLiftIO)
import qualified Data.Conduit as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Exception (try)
import Control.Monad ((<=<), unless)
import Control.Monad.Trans.Class (lift)

-- | Gzip compression with default parameters.
gzip :: (MonadThrow m, MonadUnsafeIO m) => Conduit ByteString m ByteString
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
ungzip :: (MonadUnsafeIO m, MonadThrow m) => Conduit ByteString m ByteString
ungzip = decompress (WindowBits 31)

unsafeLiftIO :: (MonadUnsafeIO m, MonadThrow m) => IO a -> m a
unsafeLiftIO =
    either rethrow return <=< C.unsafeLiftIO . try
  where
    rethrow :: MonadThrow m => ZlibException -> m a
    rethrow = monadThrow

-- |
-- Decompress (inflate) a stream of 'ByteString's. For example:
--
-- >    sourceFile "test.z" $= decompress defaultWindowBits $$ sinkFile "test"

decompress
    :: (MonadUnsafeIO m, MonadThrow m)
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
decompress =
    mapOutput unChunk . mapInput Chunk unChunk' . decompressFlush
  where
    unChunk Flush = S.empty
    unChunk (Chunk bs) = bs

    unChunk' Flush = Nothing
    unChunk' (Chunk bs) = Just bs

-- | Same as 'decompress', but allows you to explicitly flush the stream.
decompressFlush
    :: (MonadUnsafeIO m, MonadThrow m)
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Pipe (Flush ByteString) (Flush ByteString) r m r
decompressFlush config =
    awaitE >>= either return start
  where
    start input = do
        inf <- lift $ unsafeLiftIO $ initInflate config
        push inf input

    continue inf = awaitE >>= either (close inf) (push inf)

    goPopper popper = do
        mbs <- lift $ unsafeLiftIO popper
        case mbs of
            Nothing -> return ()
            Just bs -> yield (Chunk bs) >> goPopper popper

    push inf (Chunk x) = do
        popper <- lift $ unsafeLiftIO $ feedInflate inf x
        goPopper popper
        continue inf

    push inf Flush = do
        chunk <- lift $ unsafeLiftIO $ flushInflate inf
        unless (S.null chunk) $ yield $ Chunk chunk
        yield Flush
        continue inf

    close inf ret = do
        chunk <- lift $ unsafeLiftIO $ finishInflate inf
        unless (S.null chunk) $ yield $ Chunk chunk
        return ret

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: (MonadUnsafeIO m, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
compress level =
    mapOutput unChunk . mapInput Chunk unChunk' . compressFlush level
  where
    unChunk Flush = S.empty
    unChunk (Chunk bs) = bs

    unChunk' Flush = Nothing
    unChunk' (Chunk bs) = Just bs

-- | Same as 'compress', but allows you to explicitly flush the stream.
compressFlush
    :: (MonadUnsafeIO m, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Pipe (Flush ByteString) (Flush ByteString) r m r
compressFlush level config =
    awaitE >>= either return start
  where
    start input = do
        def <- lift $ unsafeLiftIO $ initDeflate level config
        push def input

    continue def = awaitE >>= either (close def) (push def)

    goPopper popper = do
        mbs <- lift $ unsafeLiftIO popper
        case mbs of
            Nothing -> return ()
            Just bs -> yield (Chunk bs) >> goPopper popper

    push def (Chunk x) = do
        popper <- lift $ unsafeLiftIO $ feedDeflate def x
        goPopper popper
        continue def

    push def Flush = do
        mchunk <- lift $ unsafeLiftIO $ flushDeflate def
        maybe (return ()) (yield . Chunk) mchunk
        yield Flush
        continue def

    close def ret = do
        mchunk <- lift $ unsafeLiftIO $ finishDeflate def
        maybe (return ()) (yield . Chunk) mchunk
        return ret
