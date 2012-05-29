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
import Control.Monad ((<=<))
import Data.Void (Void)

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
decompress config = NeedInput
    (\input -> PipeM (do
        inf <- unsafeLiftIO $ initInflate config
        push inf input) (return ()))
    (Done ())
  where
    push' inf x = PipeM (push inf x) (return ())

    push inf x = do
        popper <- unsafeLiftIO $ feedInflate inf x
        goPopper (push' inf) (close inf) id [] popper

    close inf = flip PipeM (return ()) $ do
        chunk <- unsafeLiftIO $ finishInflate inf
        return $
            if S.null chunk
                then Done ()
                else HaveOutput (Done ()) (return ()) chunk

-- | Same as 'decompress', but allows you to explicitly flush the stream.
decompressFlush
    :: (MonadUnsafeIO m, MonadThrow m)
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit (Flush ByteString) m (Flush ByteString)
decompressFlush config = NeedInput
    (\input -> flip PipeM (return ()) $ do
        inf <- unsafeLiftIO $ initInflate config
        push inf input)
    (Done ())
  where
    push' inf x = PipeM (push inf x) (return ())

    push inf (Chunk x) = do
        popper <- unsafeLiftIO $ feedInflate inf x
        goPopper (push' inf) (close inf) Chunk [] popper
    push inf Flush = do
        chunk <- unsafeLiftIO $ flushInflate inf
        let next = HaveOutput
                (NeedInput (push' inf) (close inf))
                (return ())
                Flush
        return $
            if S.null chunk
                then next
                else HaveOutput next (return ()) (Chunk chunk)

    close inf = flip PipeM (return ()) $ do
        chunk <- unsafeLiftIO $ finishInflate inf
        return $
            if S.null chunk
                then Done ()
                else HaveOutput (Done ()) (return ()) $ Chunk chunk

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: (MonadUnsafeIO m, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
compress level config = NeedInput
    (\input -> flip PipeM (return ()) $ do
        def <- unsafeLiftIO $ initDeflate level config
        push def input) (Done ())
  where
    push' def input = PipeM (push def input) (return ())
    push def x = do
        popper <- unsafeLiftIO $ feedDeflate def x
        goPopper (push' def) (close def) id [] popper

    close def = slurp $ unsafeLiftIO $ finishDeflate def

-- | Same as 'compress', but allows you to explicitly flush the stream.
compressFlush
    :: (MonadUnsafeIO m, MonadThrow m)
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit (Flush ByteString) m (Flush ByteString)
compressFlush level config = NeedInput
    (\input -> flip PipeM (return ()) $ do
        def <- unsafeLiftIO $ initDeflate level config
        push def input) (Done ())
  where
    push' def input = PipeM (push def input) (return ())

    push def (Chunk x) = do
        popper <- unsafeLiftIO $ feedDeflate def x
        goPopper (push' def) (close def) Chunk [] popper
    push def Flush = goPopper (push' def) (close def) Chunk [Flush] $ flushDeflate def

    close def = flip PipeM (return ()) $ do
        mchunk <- unsafeLiftIO $ finishDeflate def
        return $ case mchunk of
            Nothing -> Done ()
            Just chunk -> HaveOutput (Done ()) (return ()) (Chunk chunk)

goPopper :: (MonadUnsafeIO m, MonadThrow m)
         => (input -> Conduit input m output)
         -> Conduit Void m output
         -> (S.ByteString -> output)
         -> [output]
         -> Popper
         -> m (Conduit input m output)
goPopper push close wrap final popper = do
    mbs <- unsafeLiftIO popper
    return $ case mbs of
        Nothing ->
            let go [] = NeedInput push close
                go (x:xs) = HaveOutput (go xs) (return ()) x
             in go final
        Just bs -> HaveOutput (PipeM (goPopper push close wrap final popper) (return ())) (return ()) (wrap bs)

slurp :: Monad m => m (Maybe a) -> Pipe i a m ()
slurp pop = flip PipeM (return ()) $ do
    x <- pop
    return $ case x of
        Nothing -> Done ()
        Just y -> HaveOutput (slurp pop) (return ()) y
