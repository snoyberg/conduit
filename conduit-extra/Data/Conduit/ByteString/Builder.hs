{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
-- | Convert a stream of blaze-builder @Builder@s into a stream of @ByteString@s.
--
-- Works with both blaze-builder < 0.4's @Builder@s and
-- 'Data.ByteString.Builder.Builder'.
--
-- Adapted from blaze-builder-enumerator, written by myself and Simon Meier.
--
-- Note that the functions here can work in any monad built on top of @IO@ or
-- @ST@.
--
-- Since 1.1.7.0
--
module Data.Conduit.ByteString.Builder
    (

  -- * Conduits from builders to bytestrings
    CC.builderToByteString
  , CC.unsafeBuilderToByteString
  , CC.builderToByteStringWith

  -- ** Flush
  , CC.builderToByteStringFlush
  , CC.builderToByteStringWithFlush

  -- * Buffer allocation strategies
  , CC.BufferAllocStrategy
  , CC.allNewBuffersStrategy
  , CC.reuseBufferStrategy
    ) where

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
