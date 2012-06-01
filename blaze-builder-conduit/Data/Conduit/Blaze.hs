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

  -- * Conduits from builders to bytestrings
  , builderToByteString
  , unsafeBuilderToByteString
  , builderToByteStringWith

  -- ** Flush
  , builderToByteStringFlush
  , builderToByteStringWithFlush
    ) where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString                   as S

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Blaze.ByteString.Builder.Internal.Buffer

-- | Incrementally execute builders and pass on the filled chunks as
-- bytestrings.
builderToByteString :: MonadUnsafeIO m => Conduit Builder m S.ByteString
builderToByteString =
  builderToByteStringWith (allNewBuffersStrategy defaultBufferSize)

-- |
--
-- Since 0.0.2
builderToByteStringFlush :: MonadUnsafeIO m => Conduit (Flush Builder) m (Flush S.ByteString)
builderToByteStringFlush =
  builderToByteStringWithFlush (allNewBuffersStrategy defaultBufferSize)

-- | Incrementally execute builders on the given buffer and pass on the filled
-- chunks as bytestrings. Note that, if the given buffer is too small for the
-- execution of a build step, a larger one will be allocated.
--
-- WARNING: This conduit yields bytestrings that are NOT
-- referentially transparent. Their content will be overwritten as soon
-- as control is returned from the inner sink!
unsafeBuilderToByteString :: MonadUnsafeIO m
                          => IO Buffer  -- action yielding the inital buffer.
                          -> Conduit Builder m S.ByteString
unsafeBuilderToByteString = builderToByteStringWith . reuseBufferStrategy


-- | A conduit that incrementally executes builders and passes on the
-- filled chunks as bytestrings to an inner sink.
--
-- INV: All bytestrings passed to the inner sink are non-empty.
builderToByteStringWith :: MonadUnsafeIO m
                        => BufferAllocStrategy
                        -> Conduit Builder m S.ByteString
builderToByteStringWith strategy =
    CL.map Chunk =$= builderToByteStringWithFlush strategy =$= unchunk
  where
    unchunk = toPipe $ forever $ await >>= \x -> case x of { Chunk y -> yield y ; Flush -> return () }

-- |
--
-- Since 0.0.2
builderToByteStringWithFlush
    :: MonadUnsafeIO m
    => BufferAllocStrategy
    -> Conduit (Flush Builder) m (Flush S.ByteString)
builderToByteStringWithFlush (ioBufInit, nextBuf) =
    loop ioBufInit
  where
    loop ioBuf = do
        mfb <- tryAwait
        case mfb of
            Nothing -> close ioBuf
            Just Flush -> push ioBuf flush $ tryYield Flush . loop
            Just (Chunk builder) -> push ioBuf builder loop

    close ioBuf = do
        buf <- lift $ unsafeLiftIO $ ioBuf
        case unsafeFreezeNonEmptyBuffer buf of
            Nothing -> return ()
            Just bs -> tryYield (Chunk bs) $ return ()

    push ioBuf0 x continue = do
        go (unBuilder x (buildStep finalStep)) ioBuf0
      where
        finalStep !(BufRange pf _) = return $ Done pf ()

        go bStep ioBuf = do
            !buf   <- lift $ unsafeLiftIO $ ioBuf
            signal <- lift $ unsafeLiftIO $ execBuildStep bStep buf
            case signal of
                Done op' _ -> continue $ return $ updateEndOfSlice buf op'
                BufferFull minSize op' bStep' -> do
                    let buf' = updateEndOfSlice buf op'
                        {-# INLINE cont #-}
                        cont = do
                            -- sequencing the computation of the next buffer
                            -- construction here ensures that the reference to the
                            -- foreign pointer `fp` is lost as soon as possible.
                            ioBuf' <- lift $ unsafeLiftIO $ nextBuf minSize buf'
                            go bStep' ioBuf'
                    case unsafeFreezeNonEmptyBuffer buf' of
                        Nothing -> cont
                        Just bs -> tryYield (Chunk bs) cont
                InsertByteString op' bs bStep' -> do
                    let buf' = updateEndOfSlice buf op'
                    let y1 =
                            case unsafeFreezeNonEmptyBuffer buf' of
                                Nothing -> id
                                Just bs' -> tryYield (Chunk bs')
                        y2 = if S.null bs then id else tryYield (Chunk bs)
                    y1 $ y2 $ lift (unsafeLiftIO $ nextBuf 1 buf') >>= go bStep'
