-- | Convert a stream of blaze-builder @Builder@s into a stream of @ByteString@s.
--
-- Adapted from blaze-builder-enumerator, written by myself and Simon Meier.
--
-- Note that the functions here can work in any monad built on top of @IO@ or
-- @ST@.
--
-- Since 1.1.7.0, the functions here call their counterparts in
-- "Data.Conduit.ByteString.Builder", which work with both
-- 'Data.ByteString.Builder.Builder' and blaze-builder 0.3's
-- 'Blaze.ByteString.Builder.Builder'.
module Data.Conduit.Blaze
    (

  -- * Conduits from builders to bytestrings
    builderToByteString
  , unsafeBuilderToByteString
  , builderToByteStringWith

  -- ** Flush
  , builderToByteStringFlush
  , builderToByteStringWithFlush

  -- * Buffers
  , B.Buffer

  -- ** Status information
  , B.freeSize
  , B.sliceSize
  , B.bufferSize

  -- ** Creation and modification
  , B.allocBuffer
  , B.reuseBuffer
  , B.nextSlice

  -- ** Conversion to bytestings
  , B.unsafeFreezeBuffer
  , B.unsafeFreezeNonEmptyBuffer

  -- * Buffer allocation strategies
  , B.BufferAllocStrategy
  , B.allNewBuffersStrategy
  , B.reuseBufferStrategy
    ) where

import Data.Conduit

import qualified Data.ByteString                   as S

import Data.ByteString.Builder (Builder)
import Control.Monad.Primitive (PrimMonad)
import Data.Streaming.Blaze

import qualified Data.Conduit.ByteString.Builder as B

-- | Incrementally execute builders and pass on the filled chunks as
-- bytestrings.
builderToByteString :: PrimMonad m => Conduit Builder m S.ByteString
builderToByteString = B.builderToByteString
{-# INLINE builderToByteString #-}

-- |
--
-- Since 0.0.2
builderToByteStringFlush :: PrimMonad m => Conduit (Flush Builder) m (Flush S.ByteString)
builderToByteStringFlush = B.builderToByteStringFlush
{-# INLINE builderToByteStringFlush #-}

-- | Incrementally execute builders on the given buffer and pass on the filled
-- chunks as bytestrings. Note that, if the given buffer is too small for the
-- execution of a build step, a larger one will be allocated.
--
-- WARNING: This conduit yields bytestrings that are NOT
-- referentially transparent. Their content will be overwritten as soon
-- as control is returned from the inner sink!
unsafeBuilderToByteString :: PrimMonad m
                          => IO Buffer  -- action yielding the inital buffer.
                          -> Conduit Builder m S.ByteString
unsafeBuilderToByteString = B.unsafeBuilderToByteString
{-# INLINE unsafeBuilderToByteString #-}


-- | A conduit that incrementally executes builders and passes on the
-- filled chunks as bytestrings to an inner sink.
--
-- INV: All bytestrings passed to the inner sink are non-empty.
builderToByteStringWith :: PrimMonad m
                        => BufferAllocStrategy
                        -> Conduit Builder m S.ByteString
builderToByteStringWith = B.builderToByteStringWith
{-# INLINE builderToByteStringWith #-}

-- |
--
-- Since 0.0.2
builderToByteStringWithFlush
    :: PrimMonad m
    => BufferAllocStrategy
    -> Conduit (Flush Builder) m (Flush S.ByteString)
builderToByteStringWithFlush = B.builderToByteStringWithFlush
{-# INLINE builderToByteStringWithFlush #-}
