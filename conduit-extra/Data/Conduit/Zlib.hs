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
    -- * Decompression combinators
    multiple,
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
import Control.Monad.Trans.Resource (MonadThrow, throwM)
import Data.Function (fix)

-- | Gzip compression with default parameters.
gzip :: (MonadThrow m, PrimMonad m) => ConduitT ByteString ByteString m ()
gzip = compress (-1) (WindowBits 31)

-- | Gzip decompression with default parameters.
ungzip :: (PrimMonad m, MonadThrow m) => ConduitT ByteString ByteString m ()
ungzip = decompress (WindowBits 31)

unsafeLiftIO :: (PrimMonad m, MonadThrow m) => IO a -> m a
unsafeLiftIO = unsafePrimToPrim

-- |
-- Decompress (inflate) a stream of 'ByteString's. For example:
--
-- >    sourceFile "test.z" $= decompress defaultWindowBits $$ sinkFile "test"

decompress
    :: (PrimMonad m, MonadThrow m)
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> ConduitT ByteString ByteString m ()
decompress =
    helperDecompress (liftM (fmap Chunk) await) yield' leftover
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs

-- | Same as 'decompress', but allows you to explicitly flush the stream.
decompressFlush
    :: (PrimMonad m, MonadThrow m)
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> ConduitT (Flush ByteString) (Flush ByteString) m ()
decompressFlush = helperDecompress await yield (leftover . Chunk)

helperDecompress :: (Monad (t m), PrimMonad m, MonadThrow m, MonadTrans t)
                 => t m (Maybe (Flush ByteString))
                 -> (Flush ByteString -> t m ())
                 -> (ByteString -> t m ())
                 -> WindowBits
                 -> t m ()
helperDecompress await' yield' leftover' config = do
    -- Initialize the stateful inflater, which will be used below
    -- This inflater is never exposed outside of this function
    inf <- lift $ unsafeLiftIO $ initInflate config

    -- Some helper functions used by the main feeder loop below

    let -- Flush any remaining inflated bytes downstream
        flush = do
            chunk <- lift $ unsafeLiftIO $ flushInflate inf
            unless (S.null chunk) $ yield' $ Chunk chunk

        -- Get any input which is unused by the inflater
        getUnused = lift $ unsafeLiftIO $ getUnusedInflate inf

        -- If there is any unused data, return it as leftovers to the stream
        unused = do
            rem' <- getUnused
            unless (S.null rem') $ leftover' rem'

    -- Main loop: feed data from upstream into the inflater
    fix $ \feeder -> do
        mnext <- await'
        case mnext of
            -- No more data is available from upstream
            Nothing -> do
                -- Flush any remaining uncompressed data
                flush
                -- Return the rest of the unconsumed data as leftovers
                unused
            -- Another chunk of compressed data arrived
            Just (Chunk x) -> do
                -- Feed the compressed data into the inflater, returning a
                -- "popper" which will return chunks of decompressed data
                popper <- lift $ unsafeLiftIO $ feedInflate inf x

                -- Loop over the popper grabbing decompressed chunks and
                -- yielding them downstream
                fix $ \pop -> do
                    mbs <- lift $ unsafeLiftIO popper
                    case mbs of
                        -- No more data from this popper
                        PRDone -> do
                            rem' <- getUnused
                            if S.null rem'
                                -- No data was unused by the inflater, so let's
                                -- fill it up again and get more data out of it
                                then feeder
                                -- In this case, there is some unconsumed data,
                                -- meaning the compressed stream is complete.
                                -- At this point, we need to stop feeding,
                                -- return the unconsumed data as leftovers, and
                                -- flush any remaining content (which should be
                                -- nothing)
                                else do
                                    flush
                                    leftover' rem'
                        -- Another chunk available, yield it downstream and
                        -- loop again
                        PRNext bs -> do
                            yield' (Chunk bs)
                            pop
                        -- An error occurred inside zlib, throw it
                        PRError e -> lift $ throwM e
            -- We've been asked to flush the stream
            Just Flush -> do
                -- Get any uncompressed data waiting for us
                flush
                -- Put a Flush in the stream
                yield' Flush
                -- Feed in more data
                feeder

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: (PrimMonad m, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> ConduitT ByteString ByteString m ()
compress =
    helperCompress (liftM (fmap Chunk) await) yield'
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs

-- | Same as 'compress', but allows you to explicitly flush the stream.
compressFlush
    :: (PrimMonad m, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> ConduitT (Flush ByteString) (Flush ByteString) m ()
compressFlush = helperCompress await yield

helperCompress :: (Monad (t m), PrimMonad m, MonadThrow m, MonadTrans t)
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
            PRError e -> lift $ throwM e

    push def (Chunk x) = do
        popper <- lift $ unsafeLiftIO $ feedDeflate def x
        goPopper popper
        continue def

    push def Flush = do
        mchunk <- lift $ unsafeLiftIO $ flushDeflate def
        case mchunk of
            PRDone -> return ()
            PRNext x -> yield' $ Chunk x
            PRError e -> lift $ throwM e
        yield' Flush
        continue def

    close def = do
        mchunk <- lift $ unsafeLiftIO $ finishDeflate def
        case mchunk of
            PRDone -> return ()
            PRNext chunk -> yield' (Chunk chunk) >> close def
            PRError e -> lift $ throwM e

-- | The standard 'decompress' and 'ungzip' functions will only decompress a
-- single compressed entity from the stream. This combinator will exhaust the
-- stream completely of all individual compressed entities. This is useful for
-- cases where you have a concatenated archive, e.g. @cat file1.gz file2.gz >
-- combined.gz@.
--
-- Usage:
--
-- > sourceFile "combined.gz" $$ multiple ungzip =$ consume
--
-- This combinator will not fail on an empty stream. If you want to ensure that
-- at least one compressed entity in the stream exists, consider a usage such
-- as:
--
-- > sourceFile "combined.gz" $$ (ungzip >> multiple ungzip) =$ consume
--
-- @since 1.1.10
multiple :: Monad m
         => ConduitT ByteString a m ()
         -> ConduitT ByteString a m ()
multiple inner =
    loop
  where
    loop = do
        mbs <- await
        case mbs of
            Nothing -> return ()
            Just bs
                | S.null bs -> loop
                | otherwise -> do
                    leftover bs
                    inner
                    loop
