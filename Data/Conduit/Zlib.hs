{-# LANGUAGE FlexibleContexts #-}
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
-- >    run $ enumFile "test.z" $$ decompress defaultWindowBits $$ printChunks True

decompress
    :: ResourceUnsafeIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
decompress config = Conduit $ do
    inf <- lift $ unsafeFromIO $ initInflate config
    return $ PreparedConduit (push id inf) (close id inf)
  where
    push front _ [] = return $ ConduitResult Processing $ front []
    push front inf (x:xs) = do
        chunks <- lift $ unsafeFromIO $ withInflateInput inf x callback
        push (front . (chunks ++)) inf xs
    close front inf = do
        chunk <- lift $ unsafeFromIO $ finishInflate inf
        return $ ConduitResult [] $ front $ if S.null chunk then [] else [chunk]

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: ResourceUnsafeIO m
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Conduit ByteString m ByteString
compress level config = Conduit $ do
    def <- lift $ unsafeFromIO $ initDeflate level config
    return $ PreparedConduit (push id def) (close id def)
  where
    push front _ [] = return $ ConduitResult Processing $ front []
    push front def (x:xs) = do
        chunks <- lift $ unsafeFromIO $ withDeflateInput def x callback
        push (front . (chunks ++)) def xs
    close front def = do
        chunks <- lift $ unsafeFromIO $ finishDeflate def callback
        return $ ConduitResult [] $ front chunks

callback :: Monad m => m (Maybe a) -> m [a]
callback pop = go id where
    go front = do
       x <- pop
       case x of
           Nothing -> return $ front []
           Just y -> go (front . (:) y)
