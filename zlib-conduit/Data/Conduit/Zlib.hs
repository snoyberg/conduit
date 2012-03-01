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
    push inf x = do
        popper <- unsafeLiftIO $ feedInflate inf x
        goPopper (push inf) (close inf) id [] popper

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
    push inf (Chunk x) = do
        popper <- unsafeLiftIO $ feedInflate inf x
        goPopper (push inf) (close inf) Chunk [] popper
    push inf Flush = do
        chunk <- unsafeLiftIO $ flushInflate inf
        let chunk' = if S.null chunk then id else (Chunk chunk:)
        return $ Producing (push inf) (close inf) $ chunk' [Flush]

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
        popper <- unsafeLiftIO $ feedDeflate def x
        goPopper (push def) (close def) id [] popper

    close def = do
        chunks <- unsafeLiftIO $ slurp $ finishDeflate def
        return chunks

goPopper :: MonadUnsafeIO m
         => ConduitPush input m output
         -> ConduitClose m output
         -> (S.ByteString -> output)
         -> [output]
         -> IO (Maybe S.ByteString)
         -> m (ConduitResult input m output)
goPopper push close wrap final popper = do
    mbs <- unsafeLiftIO popper
    return $ case mbs of
        Nothing -> Producing push close final
        Just bs -> HaveMore (goPopper push close wrap final popper) (return ()) [wrap bs]

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
    push def (Chunk x) = do
        popper <- unsafeLiftIO $ feedDeflate def x
        goPopper (push def) (close def) Chunk [] popper
    push def Flush = goPopper (push def) (close def) Chunk [Flush] $ flushDeflate def

    close def = do
        mchunk <- unsafeLiftIO $ finishDeflate def
        return $ maybe [] (return . Chunk) mchunk

slurp :: Monad m => m (Maybe a) -> m [a]
slurp pop = go id where
    go front = do
       x <- pop
       case x of
           Nothing -> return $ front []
           Just y -> go (front . (:) y)
