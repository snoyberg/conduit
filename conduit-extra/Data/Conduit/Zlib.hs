{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Streaming.Zlib
import Data.Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Monad (unless, liftM)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Primitive (PrimMonad, unsafePrimToPrim)
import Control.Monad.Base (MonadBase, liftBase)

-- | Gzip compression with default parameters.
gzip :: (MonadThrow m, MonadBase base m, PrimMonad base) => Conduit ByteString m ByteString
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
ungzip :: (MonadBase base m, PrimMonad base, MonadThrow m) => Conduit ByteString m ByteString
ungzip = decompress (WindowBits 31)

unsafeLiftIO :: (MonadBase base m, PrimMonad base, MonadThrow m) => IO a -> m a
unsafeLiftIO = liftBase . unsafePrimToPrim

-- |
-- Decompress (inflate) a stream of 'ByteString's. For example:
--
-- >    sourceFile "test.z" $= decompress defaultWindowBits $$ sinkFile "test"

decompress
    :: (MonadBase base m, PrimMonad base, MonadThrow m)
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
decompress =
    helperDecompress (liftM (fmap Chunk) await) yield'
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs

-- | Same as 'decompress', but allows you to explicitly flush the stream.
decompressFlush
    :: (MonadBase base m, PrimMonad base, MonadThrow m)
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit (Flush ByteString) m (Flush ByteString)
decompressFlush = helperDecompress await yield

helperDecompress :: (Monad (t m), MonadBase base m, PrimMonad base, MonadThrow m, MonadTrans t)
                 => t m (Maybe (Flush ByteString))
                 -> (Flush ByteString -> t m ())
                 -> WindowBits
                 -> t m ()
helperDecompress await' yield' config =
    await' >>= maybe (return ()) start
  where
    start input = do
        inf <- lift $ unsafeLiftIO $ initInflate config
        push inf input

    continue inf = await' >>= maybe (close inf) (push inf)

    goPopper popper = do
        mbs <- lift $ unsafeLiftIO popper
        case mbs of
            PRDone -> return ()
            PRNext bs -> yield' (Chunk bs) >> goPopper popper
            PRError e -> lift $ monadThrow e

    push inf (Chunk x) = do
        popper <- lift $ unsafeLiftIO $ feedInflate inf x
        goPopper popper
        continue inf

    push inf Flush = do
        chunk <- lift $ unsafeLiftIO $ flushInflate inf
        unless (S.null chunk) $ yield' $ Chunk chunk
        yield' Flush
        continue inf

    close inf = do
        chunk <- lift $ unsafeLiftIO $ finishInflate inf
        unless (S.null chunk) $ yield' $ Chunk chunk

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: (MonadBase base m, PrimMonad base, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
compress =
    helperCompress (liftM (fmap Chunk) await) yield'
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs

-- | Same as 'compress', but allows you to explicitly flush the stream.
compressFlush
    :: (MonadBase base m, PrimMonad base, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit (Flush ByteString) m (Flush ByteString)
compressFlush = helperCompress await yield

helperCompress :: (Monad (t m), MonadBase base m, PrimMonad base, MonadThrow m, MonadTrans t)
               => t m (Maybe (Flush ByteString))
               -> (Flush ByteString -> t m ())
               -> Int
               -> WindowBits
               -> t m ()
helperCompress await' yield' level config =
    await' >>= maybe (return ()) start
  where
    start input = do
        def <- lift $ unsafeLiftIO $ initDeflate level config
        push def input

    continue def = await' >>= maybe (close def) (push def)

    goPopper popper = do
        mbs <- lift $ unsafeLiftIO popper
        case mbs of
            PRDone -> return ()
            PRNext bs -> yield' (Chunk bs) >> goPopper popper
            PRError e -> lift $ monadThrow e

    push def (Chunk x) = do
        popper <- lift $ unsafeLiftIO $ feedDeflate def x
        goPopper popper
        continue def

    push def Flush = do
        mchunk <- lift $ unsafeLiftIO $ flushDeflate def
        case mchunk of
            PRDone -> return ()
            PRNext x -> yield' $ Chunk x
            PRError e -> lift $ monadThrow e
        yield' Flush
        continue def

    close def = do
        mchunk <- lift $ unsafeLiftIO $ finishDeflate def
        case mchunk of
            PRDone -> return ()
            PRNext chunk -> yield' (Chunk chunk) >> close def
            PRError e -> lift $ monadThrow e
