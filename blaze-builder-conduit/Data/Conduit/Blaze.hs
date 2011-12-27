{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-- | Convert a stream of blaze-builder @Builder@s into a stream of @ByteString@s.
--
-- Adapted from blaze-builder-enumerator, written by myself and Simon Meier.
--
-- Note that the functions here can work in any monad built on top of @IO@ or
-- @ST@.
module Data.Conduit.Blaze
    (

  -- * Buffers
    Buffer

  -- ** Status information
  , freeSize
  , sliceSize
  , bufferSize

  -- ** Creation and modification
  , allocBuffer
  , reuseBuffer
  , nextSlice

  -- ** Conversion to bytestings
  , unsafeFreezeBuffer
  , unsafeFreezeNonEmptyBuffer

  -- * Buffer allocation strategies
  , BufferAllocStrategy
  , allNewBuffersStrategy
  , reuseBufferStrategy

  -- * Enumeratees from builders to bytestrings
  , builderToByteString
  , unsafeBuilderToByteString
  , builderToByteStringWith

    ) where

import Data.Conduit hiding (SinkResult (Done))
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class

import qualified Data.ByteString                   as S

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Blaze.ByteString.Builder.Internal.Buffer

-- | Incrementally execute builders and pass on the filled chunks as
-- bytestrings.
builderToByteString :: ResourceUnsafeIO m => Conduit Builder m S.ByteString
builderToByteString =
  builderToByteStringWith (allNewBuffersStrategy defaultBufferSize)

-- | Incrementally execute builders on the given buffer and pass on the filled
-- chunks as bytestrings. Note that, if the given buffer is too small for the
-- execution of a build step, a larger one will be allocated.
--
-- WARNING: This conduit yields bytestrings that are NOT
-- referentially transparent. Their content will be overwritten as soon
-- as control is returned from the inner sink!
unsafeBuilderToByteString :: ResourceUnsafeIO m
                          => IO Buffer  -- action yielding the inital buffer.
                          -> Conduit Builder m S.ByteString
unsafeBuilderToByteString = builderToByteStringWith . reuseBufferStrategy


-- | A conduit that incrementally executes builders and passes on the
-- filled chunks as bytestrings to an inner sink.
--
-- INV: All bytestrings passed to the inner sink are non-empty.
builderToByteStringWith :: ResourceUnsafeIO m
                        => BufferAllocStrategy
                        -> Conduit Builder m S.ByteString
builderToByteStringWith (ioBuf0, nextBuf) = conduitState
    ioBuf0
    push
    close
  where
    finalStep !(BufRange pf _) = return $ Done pf ()

    close ioBuf = lift $ unsafeFromIO $ do
        buf <- ioBuf
        return $ maybe [] return $ unsafeFreezeNonEmptyBuffer buf

    push ioBuf x = lift $ unsafeFromIO $ do
        (ioBuf', front) <- go (unBuilder x (buildStep finalStep)) ioBuf id
        return (ioBuf', Producing $ front [])

    go bStep ioBuf front = do
        !buf   <- ioBuf
        signal <- (execBuildStep bStep buf)
        case signal of
            Done op' _ -> return (return $ updateEndOfSlice buf op', front)
            BufferFull minSize op' bStep' -> do
                let buf' = updateEndOfSlice buf op'
                    {-# INLINE cont #-}
                    cont front' = do
                        -- sequencing the computation of the next buffer
                        -- construction here ensures that the reference to the
                        -- foreign pointer `fp` is lost as soon as possible.
                        ioBuf' <- nextBuf minSize buf'
                        go bStep' ioBuf' front'
                case unsafeFreezeNonEmptyBuffer buf' of
                    Nothing -> cont front
                    Just bs -> cont (front . (bs:))
            InsertByteString op' bs bStep' -> do
                let buf' = updateEndOfSlice buf op'
                    bsk  = maybe id (:) $ unsafeFreezeNonEmptyBuffer buf'
                    front' = front . bsk . (bs:)
                ioBuf' <- nextBuf 1 buf'
                go bStep' ioBuf' front'
