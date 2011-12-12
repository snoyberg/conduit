{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
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

import Data.Conduit

import qualified Data.ByteString                   as S
import           Data.Monoid

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Blaze.ByteString.Builder.Internal.Buffer

------------------------------------------------------------------------------
-- |
-- Module       : Blaze.ByteString.Builder.Enumerator
-- Copyright    : (c) 2010 Simon Meier
-- License      : BSD3
--
-- Maintainer   : Simon Meier <iridcode@gmail.com>
-- Stability    : Experimental
-- Portability  : Tested on GHC only
--
-- Infrastructure and enumeratees for the incremental execution of builders and
-- passing on of the filled chunks as bytestrings to an inner iteratee.
--
-- Note that the @Buffer@ code is likely to move/change in order to
-- reconciliate it with the rest of the blaze-builder library.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Enumeratees for converting builders incrementally to bytestrings
------------------------------------------------------------------------------

-- Simple default instances
---------------------------

-- | Incrementally execute builders and pass on the filled chunks as
-- bytestrings.
builderToByteString :: MonadBase IO m => ConduitM Builder m S.ByteString
builderToByteString = 
  builderToByteStringWith (allNewBuffersStrategy defaultBufferSize)

-- | Incrementally execute builders on the given buffer and pass on the filled
-- chunks as bytestrings. Note that, if the given buffer is too small for the
-- execution of a build step, a larger one will be allocated.
--
-- WARNING: This enumeratee yields bytestrings that are NOT
-- referentially transparent. Their content will be overwritten as soon
-- as control is returned from the inner iteratee!
unsafeBuilderToByteString :: MonadBase IO m
                          => IO Buffer  -- action yielding the inital buffer.
                          -> ConduitM Builder m S.ByteString
unsafeBuilderToByteString = builderToByteStringWith . reuseBufferStrategy


-- | An enumeratee that incrementally executes builders and passes on the
-- filled chunks as bytestrings to an inner iteratee.
--
-- INV: All bytestrings passed to the inner iteratee are non-empty.

--
-- based on the enumeratee code by Michael Snoyman <michael@snoyman.com>
--
builderToByteStringWith :: MonadBase IO m
                        => BufferAllocStrategy
                        -> ConduitM Builder m S.ByteString
builderToByteStringWith (ioBuf0, nextBuf) = conduitMState
    ioBuf0
    push
    close
  where
    finalStep !(BufRange pf _) = return $ Done pf ()

    close ioBuf xs = liftBase $ do
        (ioBuf', front) <- go (unBuilder (mconcat xs) (buildStep finalStep)) ioBuf id
        buf <- ioBuf'
        return $ ConduitCloseResult [] $ front $ maybe [] return $ unsafeFreezeNonEmptyBuffer buf

    push ioBuf xs = liftBase $ do
        (ioBuf', front) <- go (unBuilder (mconcat xs) (buildStep finalStep)) ioBuf id
        return (ioBuf', ConduitResult StreamOpen [] $ front [])

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

{- Old testing code:
 
main :: IO ()
main = main1 >> main2 >> main3

main1 :: IO ()
main1 = do
    builder <- fromLazyByteString `fmap` L.readFile "test-input"
    withBinaryFile "test-output1" WriteMode $ \h -> run_ (go h builder)
  where
    go h builder = enumList 1 [builder]
      $$ joinI $ blaze
      $$ iterHandle h

main2 :: IO ()
main2 =
    withBinaryFile "test-output2" WriteMode $ \h -> run_ (go h)
  where
    go h = enumFile "test-input"
      $$ joinI $ E.map fromByteString
      $$ joinI $ blaze
      $$ iterHandle h

main3 :: IO ()
main3 =
    withBinaryFile "test-output3" WriteMode $ \h -> run_ (go h)
  where
    go h = enumList 1 (map S.singleton $ concat $ replicate 1000 [65..90])
      $$ joinI $ E.map (mconcat . map fromWord8 . S.unpack)
      $$ joinI $ blaze
      $$ iterHandle h

-}
