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
import Data.Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as S

-- | Gzip compression with default parameters.
gzip :: MonadUnsafeIO m => Conduit ByteString m ByteString
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
ungzip :: MonadUnsafeIO m => Conduit ByteString m ByteString
ungzip = decompress (WindowBits 31)

-- |
-- Decompress (inflate) a stream of 'ByteString's. For example:
--
-- >    sourceFile "test.z" $= decompress defaultWindowBits $$ sinkFile "test"

decompress
    :: MonadUnsafeIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
decompress config = Conduit
    { conduitPush = \input -> do
        inf <- unsafeLiftIO $ initInflate config
        push inf input
    , conduitClose = return []
    }
  where
    mkCon inf = Conduit (push inf) (close inf)
    push inf x = do
        chunks <- unsafeLiftIO $ withInflateInput inf x callback
        return $ Producing (mkCon inf) chunks
    close inf = do
        chunk <- unsafeLiftIO $ finishInflate inf
        return $ if S.null chunk then [] else [chunk]

-- | Same as 'decompress', but allows you to explicitly flush the stream.
decompressFlush
    :: MonadUnsafeIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit (Flush ByteString) m (Flush ByteString)
decompressFlush config = Conduit
    { conduitPush = \input -> do
        inf <- unsafeLiftIO $ initInflate config
        push inf input
    , conduitClose = return []
    }
  where
    mkCon inf = Conduit (push inf) (close inf)
    push inf (Chunk x) = do
        chunks <- unsafeLiftIO $ withInflateInput inf x callback
        return $ Producing (mkCon inf) $ map Chunk chunks
    push inf Flush = do
        chunk <- unsafeLiftIO $ flushInflate inf
        let chunk' = if S.null chunk then id else (Chunk chunk:)
        return $ Producing (mkCon inf) $ chunk' [Flush]
    close inf = do
        chunk <- unsafeLiftIO $ finishInflate inf
        return $ if S.null chunk then [] else [Chunk chunk]

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: MonadUnsafeIO m
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
compress level config = Conduit
    { conduitPush = \input -> do
        def <- unsafeLiftIO $ initDeflate level config
        push def input
    , conduitClose = return []
    }
  where
    push def x = do
        chunks <- unsafeLiftIO $ withDeflateInput def x callback
        return $ Producing (Conduit (push def) (close def)) chunks
    close def = do
        chunks <- unsafeLiftIO $ finishDeflate def callback
        return chunks

-- | Same as 'compress', but allows you to explicitly flush the stream.
compressFlush
    :: MonadUnsafeIO m
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit (Flush ByteString) m (Flush ByteString)
compressFlush level config = Conduit
    { conduitPush = \input -> do
        def <- unsafeLiftIO $ initDeflate level config
        push def input
    , conduitClose = return []
    }
  where
    mkCon def = Conduit (push def) (close def)
    push def (Chunk x) = do
        chunks <- unsafeLiftIO $ withDeflateInput def x callback
        return $ Producing (mkCon def) $ map Chunk chunks
    push def Flush = do
        chunks <- unsafeLiftIO $ flushDeflate def callback
        return $ Producing (mkCon def) $ map Chunk chunks ++ [Flush]
    close def = do
        chunks <- unsafeLiftIO $ finishDeflate def callback
        return $ map Chunk chunks

callback :: Monad m => m (Maybe a) -> m [a]
callback pop = go id where
    go front = do
       x <- pop
       case x of
           Nothing -> return $ front []
           Just y -> go (front . (:) y)
