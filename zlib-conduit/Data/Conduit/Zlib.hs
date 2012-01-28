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
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class

-- | Gzip compression with default parameters.
gzip :: ResourceUnsafeIO m => Conduit ByteString m ByteString
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
ungzip :: ResourceUnsafeIO m => Conduit ByteString m ByteString
ungzip = decompress (WindowBits 31)

-- |
-- Decompress (inflate) a stream of 'ByteString's. For example:
--
-- >    sourceFile "test.z" $= decompress defaultWindowBits $$ sinkFile "test"

decompress
    :: ResourceUnsafeIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
decompress config = Conduit
    { conduitPush = \input -> do
        inf <- lift $ unsafeFromIO $ initInflate config
        push inf input
    , conduitClose = return []
    }
  where
    mkCon inf = Conduit (push inf) (close inf)
    push inf x = do
        chunks <- lift $ unsafeFromIO $ withInflateInput inf x callback
        return $ Producing (mkCon inf) chunks
    close inf = do
        chunk <- lift $ unsafeFromIO $ finishInflate inf
        return $ if S.null chunk then [] else [chunk]

-- | Same as 'decompress', but allows you to explicitly flush the stream.
decompressFlush
    :: ResourceUnsafeIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit (Flush ByteString) m (Flush ByteString)
decompressFlush config = Conduit
    { conduitPush = \input -> do
        inf <- lift $ unsafeFromIO $ initInflate config
        push inf input
    , conduitClose = return []
    }
  where
    mkCon inf = Conduit (push inf) (close inf)
    push inf (Chunk x) = do
        chunks <- lift $ unsafeFromIO $ withInflateInput inf x callback
        return $ Producing (mkCon inf) $ map Chunk chunks
    push inf Flush = do
        chunk <- lift $ unsafeFromIO $ flushInflate inf
        let chunk' = if S.null chunk then id else (Chunk chunk:)
        return $ Producing (mkCon inf) $ chunk' [Flush]
    close inf = do
        chunk <- lift $ unsafeFromIO $ finishInflate inf
        return $ if S.null chunk then [] else [Chunk chunk]

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: ResourceUnsafeIO m
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
compress level config = Conduit
    { conduitPush = \input -> do
        def <- lift $ unsafeFromIO $ initDeflate level config
        push def input
    , conduitClose = return []
    }
  where
    push def x = do
        chunks <- lift $ unsafeFromIO $ withDeflateInput def x callback
        return $ Producing (Conduit (push def) (close def)) chunks
    close def = do
        chunks <- lift $ unsafeFromIO $ finishDeflate def callback
        return chunks

-- | Same as 'compress', but allows you to explicitly flush the stream.
compressFlush
    :: ResourceUnsafeIO m
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit (Flush ByteString) m (Flush ByteString)
compressFlush level config = Conduit
    { conduitPush = \input -> do
        def <- lift $ unsafeFromIO $ initDeflate level config
        push def input
    , conduitClose = return []
    }
  where
    mkCon def = Conduit (push def) (close def)
    push def (Chunk x) = do
        chunks <- lift $ unsafeFromIO $ withDeflateInput def x callback
        return $ Producing (mkCon def) $ map Chunk chunks
    push def Flush = do
        chunks <- lift $ unsafeFromIO $ flushDeflate def callback
        return $ Producing (mkCon def) $ map Chunk chunks ++ [Flush]
    close def = do
        chunks <- lift $ unsafeFromIO $ finishDeflate def callback
        return $ map Chunk chunks

callback :: Monad m => m (Maybe a) -> m [a]
callback pop = go id where
    go front = do
       x <- pop
       case x of
           Nothing -> return $ front []
           Just y -> go (front . (:) y)
