{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Conduit hiding (unsafeLiftIO, Source, Sink, Conduit)
import Data.Conduit.Class (MonadStream, StreamMonad)
import qualified Data.Conduit as C (unsafeLiftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Exception (try)
import Control.Monad ((<=<), unless, liftM)

-- | Gzip compression with default parameters.
gzip :: (MonadThrow m, MonadUnsafeIO m) => MonadConduit ByteString m ByteString
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
ungzip :: (MonadUnsafeIO m, MonadThrow m) => MonadConduit ByteString m ByteString
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
    -> MonadConduit ByteString m ByteString
decompress =
    helperDecompress (liftM (fmap Chunk) await) yield'
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs

-- | Same as 'decompress', but allows you to explicitly flush the stream.
decompressFlush
    :: (MonadUnsafeIO m, MonadThrow m)
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> MonadConduit (Flush ByteString) m (Flush ByteString)
decompressFlush = helperDecompress await yield

helperDecompress :: (MonadUnsafeIO (StreamMonad m), MonadThrow (StreamMonad m), MonadStream m)
                 => m (Maybe (Flush ByteString))
                 -> (Flush ByteString -> m ())
                 -> WindowBits
                 -> m ()
helperDecompress await' yield' config =
    await' >>= maybe (return ()) start
  where
    start input = do
        inf <- liftStreamMonad $ unsafeLiftIO $ initInflate config
        push inf input

    continue inf = await' >>= maybe (close inf) (push inf)

    goPopper popper = do
        mbs <- liftStreamMonad $ unsafeLiftIO popper
        case mbs of
            Nothing -> return ()
            Just bs -> yield' (Chunk bs) >> goPopper popper

    push inf (Chunk x) = do
        popper <- liftStreamMonad $ unsafeLiftIO $ feedInflate inf x
        goPopper popper
        continue inf

    push inf Flush = do
        chunk <- liftStreamMonad $ unsafeLiftIO $ flushInflate inf
        unless (S.null chunk) $ yield' $ Chunk chunk
        yield' Flush
        continue inf

    close inf = do
        chunk <- liftStreamMonad $ unsafeLiftIO $ finishInflate inf
        unless (S.null chunk) $ yield' $ Chunk chunk

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: (MonadUnsafeIO m, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> MonadConduit ByteString m ByteString
compress =
    helperCompress (liftM (fmap Chunk) await) yield'
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs

-- | Same as 'compress', but allows you to explicitly flush the stream.
compressFlush
    :: (MonadUnsafeIO m, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> MonadConduit (Flush ByteString) m (Flush ByteString)
compressFlush = helperCompress await yield

helperCompress :: (MonadUnsafeIO (StreamMonad m), MonadThrow (StreamMonad m), MonadStream m)
               => m (Maybe (Flush ByteString))
               -> (Flush ByteString -> m ())
               -> Int
               -> WindowBits
               -> m ()
helperCompress await' yield' level config =
    await' >>= maybe (return ()) start
  where
    start input = do
        def <- liftStreamMonad $ unsafeLiftIO $ initDeflate level config
        push def input

    continue def = await' >>= maybe (close def) (push def)

    goPopper popper = do
        mbs <- liftStreamMonad $ unsafeLiftIO popper
        case mbs of
            Nothing -> return ()
            Just bs -> yield' (Chunk bs) >> goPopper popper

    push def (Chunk x) = do
        popper <- liftStreamMonad $ unsafeLiftIO $ feedDeflate def x
        goPopper popper
        continue def

    push def Flush = do
        mchunk <- liftStreamMonad $ unsafeLiftIO $ flushDeflate def
        maybe (return ()) (yield' . Chunk) mchunk
        yield' Flush
        continue def

    close def = do
        mchunk <- liftStreamMonad $ unsafeLiftIO $ finishDeflate def
        case mchunk of
            Nothing -> return ()
            Just chunk -> yield' (Chunk chunk) >> close def
