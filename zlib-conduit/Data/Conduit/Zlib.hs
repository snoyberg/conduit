{-# LANGUAGE FlexibleContexts #-}
-- | Streaming compression and decompression using conduits.
--
-- Parts of this code were taken from zlib-enum and adapted for conduits.
module Data.Conduit.Zlib (
    -- * Conduits
    compress, decompress, gzip, ungzip,
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
--
-- Note: if a null chunk is encountered in the stream, it will be taken as an
-- instruction to flush the buffer.

decompress
    :: ResourceUnsafeIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
decompress config = Conduit $ do
    inf <- lift $ unsafeFromIO $ initInflate config
    return $ PreparedConduit (push inf) (close inf)
  where
    push inf x = do
        let action = if S.null x
                        then fmap return $ flushInflate inf
                        else withInflateInput inf x callback
        chunks <- lift $ unsafeFromIO action
        return $ Producing chunks
    close inf = do
        chunk <- lift $ unsafeFromIO $ finishInflate inf
        return $ if S.null chunk then [] else [chunk]

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).
--
-- Note: if a null chunk is encountered in the stream, it will be taken as an
-- instruction to flush the buffer.

compress
    :: ResourceUnsafeIO m
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
compress level config = Conduit $ do
    def <- lift $ unsafeFromIO $ initDeflate level config
    return $ PreparedConduit (push def) (close def)
  where
    push def x = do
        let action = if S.null x
                        then flushDeflate
                        else flip withDeflateInput x
        chunks <- lift $ unsafeFromIO $ action def callback
        return $ Producing chunks
    close def = do
        chunks <- lift $ unsafeFromIO $ finishDeflate def callback
        return chunks

callback :: Monad m => m (Maybe a) -> m [a]
callback pop = go id where
    go front = do
       x <- pop
       case x of
           Nothing -> return $ front []
           Just y -> go (front . (:) y)
