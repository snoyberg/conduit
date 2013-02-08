{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Convert a stream of blaze-builder @Builder@s into a stream of @ByteString@s.
--
-- Adapted from blaze-builder-enumerator, written by myself and Simon Meier.
--
-- Note that the functions here can work in any monad built on top of @IO@ or
-- @ST@.
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
  , Buffer

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
    ) where

import Data.Conduit hiding (Source, Conduit, Sink)
import Data.Conduit.Class (StreamMonad, MonadStream)
import Control.Monad (unless, liftM)

import qualified Data.ByteString                   as S

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Blaze.ByteString.Builder.Internal.Buffer

-- | Incrementally execute builders and pass on the filled chunks as
-- bytestrings.
builderToByteString :: MonadUnsafeIO m => MonadConduit Builder m S.ByteString
builderToByteString =
  builderToByteStringWith (allNewBuffersStrategy defaultBufferSize)

-- |
--
-- Since 0.0.2
builderToByteStringFlush :: MonadUnsafeIO m => MonadConduit (Flush Builder) m (Flush S.ByteString)
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
                          -> MonadConduit Builder m S.ByteString
unsafeBuilderToByteString = builderToByteStringWith . reuseBufferStrategy


-- | A conduit that incrementally executes builders and passes on the
-- filled chunks as bytestrings to an inner sink.
--
-- INV: All bytestrings passed to the inner sink are non-empty.
builderToByteStringWith :: MonadUnsafeIO m
                        => BufferAllocStrategy
                        -> MonadConduit Builder m S.ByteString
builderToByteStringWith =
    helper (liftM (fmap Chunk) await) yield'
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs

-- |
--
-- Since 0.0.2
builderToByteStringWithFlush
    :: MonadUnsafeIO m
    => BufferAllocStrategy
    -> MonadConduit (Flush Builder) m (Flush S.ByteString)
builderToByteStringWithFlush = helper await yield

helper :: (MonadUnsafeIO (StreamMonad m), MonadStream m)
       => m (Maybe (Flush Builder))
       -> (Flush S.ByteString -> m ())
       -> BufferAllocStrategy
       -> m ()
helper await' yield' (ioBufInit, nextBuf) =
    loop ioBufInit
  where
    loop ioBuf = do
        await' >>= maybe (close ioBuf) (cont' ioBuf)

    cont' ioBuf Flush = push ioBuf flush $ \ioBuf' -> yield' Flush >> loop ioBuf'
    cont' ioBuf (Chunk builder) = push ioBuf builder loop

    close ioBuf = do
        buf <- liftStreamMonad $ unsafeLiftIO $ ioBuf
        maybe (return ()) (yield' . Chunk) (unsafeFreezeNonEmptyBuffer buf)

    push ioBuf0 x continue = do
        go (unBuilder x (buildStep finalStep)) ioBuf0
      where
        finalStep !(BufRange pf _) = return $ Done pf ()

        go bStep ioBuf = do
            !buf   <- liftStreamMonad $ unsafeLiftIO $ ioBuf
            signal <- liftStreamMonad $ unsafeLiftIO $ execBuildStep bStep buf
            case signal of
                Done op' _ -> continue $ return $ updateEndOfSlice buf op'
                BufferFull minSize op' bStep' -> do
                    let buf' = updateEndOfSlice buf op'
                        {-# INLINE cont #-}
                        cont = do
                            -- sequencing the computation of the next buffer
                            -- construction here ensures that the reference to the
                            -- foreign pointer `fp` is lost as soon as possible.
                            ioBuf' <- liftStreamMonad $ unsafeLiftIO $ nextBuf minSize buf'
                            go bStep' ioBuf'
                    case unsafeFreezeNonEmptyBuffer buf' of
                        Nothing -> return ()
                        Just bs -> yield' (Chunk bs)
                    cont
                InsertByteString op' bs bStep' -> do
                    let buf' = updateEndOfSlice buf op'
                    case unsafeFreezeNonEmptyBuffer buf' of
                        Nothing -> return ()
                        Just bs' -> yield' $ Chunk bs'
                    unless (S.null bs) $ yield' $ Chunk bs
                    liftStreamMonad (unsafeLiftIO $ nextBuf 1 buf') >>= go bStep'
