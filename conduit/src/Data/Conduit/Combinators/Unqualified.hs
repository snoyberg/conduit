{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Conduit.Combinators.Unqualified
    ( -- ** Producers
      -- *** Pure
      CC.yieldMany
    , unfoldC
    , enumFromToC
    , iterateC
    , repeatC
    , replicateC
    , CC.sourceLazy

      -- *** Monadic
    , repeatMC
    , repeatWhileMC
    , replicateMC

      -- *** I\/O
    , CC.sourceFile
    , CC.sourceFileBS
    , CC.sourceHandle
    , CC.sourceHandleUnsafe
    , CC.sourceIOHandle
    , stdinC
    , CC.withSourceFile

      -- *** Filesystem
    , CC.sourceDirectory
    , CC.sourceDirectoryDeep

      -- ** Consumers
      -- *** Pure
    , dropC
    , dropCE
    , dropWhileC
    , dropWhileCE
    , foldC
    , foldCE
    , foldlC
    , foldlCE
    , foldMapC
    , foldMapCE
    , allC
    , allCE
    , anyC
    , anyCE
    , andC
    , andCE
    , orC
    , orCE
    , asumC
    , elemC
    , elemCE
    , notElemC
    , notElemCE
    , CC.sinkLazy
    , CC.sinkList
    , CC.sinkVector
    , CC.sinkVectorN
    , CC.sinkLazyBuilder
    , CC.sinkNull
    , CC.awaitNonNull
    , headC
    , headDefC
    , headCE
    , peekC
    , peekCE
    , lastC
    , lastDefC
    , lastCE
    , lengthC
    , lengthCE
    , lengthIfC
    , lengthIfCE
    , maximumC
    , maximumCE
    , minimumC
    , minimumCE
    , nullC
    , nullCE
    , sumC
    , sumCE
    , productC
    , productCE
    , findC

      -- *** Monadic
    , mapM_C
    , mapM_CE
    , foldMC
    , foldMCE
    , foldMapMC
    , foldMapMCE

      -- *** I\/O
    , CC.sinkFile
    , CC.sinkFileCautious
    , CC.sinkTempFile
    , CC.sinkSystemTempFile
    , CC.sinkFileBS
    , CC.sinkHandle
    , CC.sinkIOHandle
    , printC
    , stdoutC
    , stderrC
    , CC.withSinkFile
    , CC.withSinkFileBuilder
    , CC.withSinkFileCautious
    , CC.sinkHandleBuilder
    , CC.sinkHandleFlush

      -- ** Transformers
      -- *** Pure
    , mapC
    , mapCE
    , omapCE
    , concatMapC
    , concatMapCE
    , takeC
    , takeCE
    , takeWhileC
    , takeWhileCE
    , takeExactlyC
    , takeExactlyCE
    , concatC
    , filterC
    , filterCE
    , mapWhileC
    , conduitVector
    , scanlC
    , mapAccumWhileC
    , concatMapAccumC
    , intersperseC
    , slidingWindowC
    , chunksOfCE
    , chunksOfExactlyCE

      -- *** Monadic
    , mapMC
    , mapMCE
    , omapMCE
    , concatMapMC
    , filterMC
    , filterMCE
    , iterMC
    , scanlMC
    , mapAccumWhileMC
    , concatMapAccumMC

      -- *** Textual
    , encodeUtf8C
    , decodeUtf8C
    , decodeUtf8LenientC
    , lineC
    , lineAsciiC
    , unlinesC
    , unlinesAsciiC
    , linesUnboundedC
    , linesUnboundedAsciiC

      -- ** Builders
    , CC.builderToByteString
    , CC.unsafeBuilderToByteString
    , CC.builderToByteStringWith
    , CC.builderToByteStringFlush
    , CC.builderToByteStringWithFlush
    , CC.BufferAllocStrategy
    , CC.allNewBuffersStrategy
    , CC.reuseBufferStrategy

      -- ** Special
    , vectorBuilderC
    , CC.mapAccumS
    , CC.peekForever
    , CC.peekForeverE
    ) where

-- BEGIN IMPORTS

import qualified Data.Conduit.Combinators as CC
-- BEGIN IMPORTS

import qualified Data.Traversable
import           Control.Applicative         (Alternative)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Data.Conduit
import           Data.Monoid                 (Monoid (..))
import           Data.MonoTraversable
import qualified Data.Sequences              as Seq
import qualified Data.Vector.Generic         as V
import           Prelude                     (Bool (..), Eq (..), Int,
                                              Maybe (..), Monad (..), Num (..),
                                              Ord (..), Functor (..), Either (..),
                                              Enum, Show, Char)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.Sequences as DTE


-- END IMPORTS

-- | Generate a producer from a seed value.
--
-- Since 1.0.0
unfoldC :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> ConduitT i a m ()
unfoldC = CC.unfold
{-# INLINE unfoldC #-}

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Since 1.0.0
enumFromToC :: (Monad m, Enum a, Ord a) => a -> a -> ConduitT i a m ()
enumFromToC = CC.enumFromTo
{-# INLINE enumFromToC #-}

-- | Produces an infinite stream of repeated applications of f to x.
--
-- Since 1.0.0
iterateC :: Monad m => (a -> a) -> a -> ConduitT i a m ()
iterateC = CC.iterate
{-# INLINE iterateC #-}

-- | Produce an infinite stream consisting entirely of the given value.
--
-- Since 1.0.0
repeatC :: Monad m => a -> ConduitT i a m ()
repeatC = CC.repeat
{-# INLINE repeatC #-}

-- | Produce a finite stream consisting of n copies of the given value.
--
-- Since 1.0.0
replicateC :: Monad m
          => Int
          -> a
          -> ConduitT i a m ()
replicateC = CC.replicate
{-# INLINE replicateC #-}

-- | Repeatedly run the given action and yield all values it produces.
--
-- Since 1.0.0
repeatMC :: Monad m
        => m a
        -> ConduitT i a m ()
repeatMC = CC.repeatM
{-# INLINE repeatMC #-}

-- | Repeatedly run the given action and yield all values it produces, until
-- the provided predicate returns @False@.
--
-- Since 1.0.0
repeatWhileMC :: Monad m
             => m a
             -> (a -> Bool)
             -> ConduitT i a m ()
repeatWhileMC = CC.repeatWhileM
{-# INLINE repeatWhileMC #-}

-- | Perform the given action n times, yielding each result.
--
-- Since 1.0.0
replicateMC :: Monad m
           => Int
           -> m a
           -> ConduitT i a m ()
replicateMC = CC.replicateM
{-# INLINE replicateMC #-}

-- | @sourceHandle@ applied to @stdin@.
--
-- Since 1.0.0
stdinC :: MonadIO m => ConduitT i ByteString m ()
stdinC = CC.stdin
{-# INLINE stdinC #-}

-- | Ignore a certain number of values in the stream.
--
-- Since 1.0.0
dropC :: Monad m
     => Int
     -> ConduitT a o m ()
dropC = CC.drop
{-# INLINE dropC #-}

-- | Drop a certain number of elements from a chunked stream.
--
-- Since 1.0.0
dropCE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> ConduitT seq o m ()
dropCE = CC.dropE
{-# INLINE dropCE #-}

-- | Drop all values which match the given predicate.
--
-- Since 1.0.0
dropWhileC :: Monad m
          => (a -> Bool)
          -> ConduitT a o m ()
dropWhileC = CC.dropWhile
{-# INLINE dropWhileC #-}

-- | Drop all elements in the chunked stream which match the given predicate.
--
-- Since 1.0.0
dropWhileCE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> ConduitT seq o m ()
dropWhileCE = CC.dropWhileE
{-# INLINE dropWhileCE #-}

-- | Monoidally combine all values in the stream.
--
-- Since 1.0.0
foldC :: (Monad m, Monoid a)
     => ConduitT a o m a
foldC = CC.fold
{-# INLINE foldC #-}

-- | Monoidally combine all elements in the chunked stream.
--
-- Since 1.0.0
foldCE :: (Monad m, MonoFoldable mono, Monoid (Element mono))
      => ConduitT mono o m (Element mono)
foldCE = CC.foldE
{-# INLINE foldCE #-}

-- | A strict left fold.
--
-- Since 1.0.0
foldlC :: Monad m => (a -> b -> a) -> a -> ConduitT b o m a
foldlC = CC.foldl
{-# INLINE foldlC #-}

-- | A strict left fold on a chunked stream.
--
-- Since 1.0.0
foldlCE :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> a)
       -> a
       -> ConduitT mono o m a
foldlCE = CC.foldlE
{-# INLINE foldlCE #-}

-- | Apply the provided mapping function and monoidal combine all values.
--
-- Since 1.0.0
foldMapC :: (Monad m, Monoid b)
        => (a -> b)
        -> ConduitT a o m b
foldMapC = CC.foldMap
{-# INLINE foldMapC #-}

-- | Apply the provided mapping function and monoidal combine all elements of the chunked stream.
--
-- Since 1.0.0
foldMapCE :: (Monad m, MonoFoldable mono, Monoid w)
         => (Element mono -> w)
         -> ConduitT mono o m w
foldMapCE = CC.foldMapE
{-# INLINE foldMapCE #-}

-- | Check that all values in the stream return True.
--
-- Subject to shortcut logic: at the first False, consumption of the stream
-- will stop.
--
-- Since 1.0.0
allC :: Monad m
    => (a -> Bool)
    -> ConduitT a o m Bool
allC = CC.all
{-# INLINE allC #-}

-- | Check that all elements in the chunked stream return True.
--
-- Subject to shortcut logic: at the first False, consumption of the stream
-- will stop.
--
-- Since 1.0.0
allCE :: (Monad m, MonoFoldable mono)
     => (Element mono -> Bool)
     -> ConduitT mono o m Bool
allCE = CC.allE
{-# INLINE allCE #-}

-- | Check that at least one value in the stream returns True.
--
-- Subject to shortcut logic: at the first True, consumption of the stream
-- will stop.
--
-- Since 1.0.0
anyC :: Monad m
    => (a -> Bool)
    -> ConduitT a o m Bool
anyC = CC.any
{-# INLINE anyC #-}

-- | Check that at least one element in the chunked stream returns True.
--
-- Subject to shortcut logic: at the first True, consumption of the stream
-- will stop.
--
-- Since 1.0.0
anyCE :: (Monad m, MonoFoldable mono)
     => (Element mono -> Bool)
     -> ConduitT mono o m Bool
anyCE = CC.anyE
{-# INLINE anyCE #-}

-- | Are all values in the stream True?
--
-- Consumption stops once the first False is encountered.
--
-- Since 1.0.0
andC :: Monad m => ConduitT Bool o m Bool
andC = CC.and
{-# INLINE andC #-}

-- | Are all elements in the chunked stream True?
--
-- Consumption stops once the first False is encountered.
--
-- Since 1.0.0
andCE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
     => ConduitT mono o m Bool
andCE = CC.andE
{-# INLINE andCE #-}

-- | Are any values in the stream True?
--
-- Consumption stops once the first True is encountered.
--
-- Since 1.0.0
orC :: Monad m => ConduitT Bool o m Bool
orC = CC.or
{-# INLINE orC #-}

-- | Are any elements in the chunked stream True?
--
-- Consumption stops once the first True is encountered.
--
-- Since 1.0.0
orCE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
    => ConduitT mono o m Bool
orCE = CC.orE
{-# INLINE orCE #-}

-- | 'Alternative'ly combine all values in the stream.
--
-- Since 1.1.1
asumC :: (Monad m, Alternative f) => ConduitT (f a) o m (f a)
asumC = CC.asum

-- | Are any values in the stream equal to the given value?
--
-- Stops consuming as soon as a match is found.
--
-- Since 1.0.0
elemC :: (Monad m, Eq a) => a -> ConduitT a o m Bool
elemC = CC.elem
{-# INLINE elemC #-}

-- | Are any elements in the chunked stream equal to the given element?
--
-- Stops consuming as soon as a match is found.
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
elemCE :: (Monad m, Seq.IsSequence seq, Eq (Element seq))
#else
elemCE :: (Monad m, Seq.EqSequence seq)
#endif
      => Element seq
      -> ConduitT seq o m Bool
elemCE = CC.elemE
{-# INLINE elemCE #-}

-- | Are no values in the stream equal to the given value?
--
-- Stops consuming as soon as a match is found.
--
-- Since 1.0.0
notElemC :: (Monad m, Eq a) => a -> ConduitT a o m Bool
notElemC = CC.notElem
{-# INLINE notElemC #-}

-- | Are no elements in the chunked stream equal to the given element?
--
-- Stops consuming as soon as a match is found.
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
notElemCE :: (Monad m, Seq.IsSequence seq, Eq (Element seq))
#else
notElemCE :: (Monad m, Seq.EqSequence seq)
#endif
         => Element seq
         -> ConduitT seq o m Bool
notElemCE = CC.notElemE
{-# INLINE notElemCE #-}

-- | Take a single value from the stream, if available.
--
-- Since 1.0.5
headC :: Monad m => ConduitT a o m (Maybe a)
headC = CC.head

-- | Same as 'headC', but returns a default value if none are available from the stream.
--
-- Since 1.0.5
headDefC :: Monad m => a -> ConduitT a o m a
headDefC = CC.headDef

-- | Get the next element in the chunked stream.
--
-- Since 1.0.0
headCE :: (Monad m, Seq.IsSequence seq) => ConduitT seq o m (Maybe (Element seq))
headCE = CC.headE
{-# INLINE headCE #-}

-- | View the next value in the stream without consuming it.
--
-- Since 1.0.0
peekC :: Monad m => ConduitT a o m (Maybe a)
peekC = CC.peek
{-# INLINE peekC #-}

-- | View the next element in the chunked stream without consuming it.
--
-- Since 1.0.0
peekCE :: (Monad m, MonoFoldable mono) => ConduitT mono o m (Maybe (Element mono))
peekCE = CC.peekE
{-# INLINE peekCE #-}

-- | Retrieve the last value in the stream, if present.
--
-- Since 1.0.0
lastC :: Monad m => ConduitT a o m (Maybe a)
lastC = CC.last
{-# INLINE lastC #-}

-- | Same as 'lastC', but returns a default value if none are available from the stream.
--
-- Since 1.0.5
lastDefC :: Monad m => a -> ConduitT a o m a
lastDefC = CC.lastDef

-- | Retrieve the last element in the chunked stream, if present.
--
-- Since 1.0.0
lastCE :: (Monad m, Seq.IsSequence seq) => ConduitT seq o m (Maybe (Element seq))
lastCE = CC.lastE
{-# INLINE lastCE #-}

-- | Count how many values are in the stream.
--
-- Since 1.0.0
lengthC :: (Monad m, Num len) => ConduitT a o m len
lengthC = CC.length
{-# INLINE lengthC #-}

-- | Count how many elements are in the chunked stream.
--
-- Since 1.0.0
lengthCE :: (Monad m, Num len, MonoFoldable mono) => ConduitT mono o m len
lengthCE = CC.lengthE
{-# INLINE lengthCE #-}

-- | Count how many values in the stream pass the given predicate.
--
-- Since 1.0.0
lengthIfC :: (Monad m, Num len) => (a -> Bool) -> ConduitT a o m len
lengthIfC = CC.lengthIf
{-# INLINE lengthIfC #-}

-- | Count how many elements in the chunked stream pass the given predicate.
--
-- Since 1.0.0
lengthIfCE :: (Monad m, Num len, MonoFoldable mono)
          => (Element mono -> Bool) -> ConduitT mono o m len
lengthIfCE = CC.lengthIfE
{-# INLINE lengthIfCE #-}

-- | Get the largest value in the stream, if present.
--
-- Since 1.0.0
maximumC :: (Monad m, Ord a) => ConduitT a o m (Maybe a)
maximumC = CC.maximum
{-# INLINE maximumC #-}

-- | Get the largest element in the chunked stream, if present.
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
maximumCE :: (Monad m, Seq.IsSequence seq, Ord (Element seq)) => ConduitT seq o m (Maybe (Element seq))
#else
maximumCE :: (Monad m, Seq.OrdSequence seq) => ConduitT seq o m (Maybe (Element seq))
#endif
maximumCE = CC.maximumE
{-# INLINE maximumCE #-}

-- | Get the smallest value in the stream, if present.
--
-- Since 1.0.0
minimumC :: (Monad m, Ord a) => ConduitT a o m (Maybe a)
minimumC = CC.minimum
{-# INLINE minimumC #-}

-- | Get the smallest element in the chunked stream, if present.
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
minimumCE :: (Monad m, Seq.IsSequence seq, Ord (Element seq)) => ConduitT seq o m (Maybe (Element seq))
#else
minimumCE :: (Monad m, Seq.OrdSequence seq) => ConduitT seq o m (Maybe (Element seq))
#endif
minimumCE = CC.minimumE
{-# INLINE minimumCE #-}

-- | True if there are no values in the stream.
--
-- This function does not modify the stream.
--
-- Since 1.0.0
nullC :: Monad m => ConduitT a o m Bool
nullC = CC.null
{-# INLINE nullC #-}

-- | True if there are no elements in the chunked stream.
--
-- This function may remove empty leading chunks from the stream, but otherwise
-- will not modify it.
--
-- Since 1.0.0
nullCE :: (Monad m, MonoFoldable mono)
      => ConduitT mono o m Bool
nullCE = CC.nullE
{-# INLINE nullCE #-}

-- | Get the sum of all values in the stream.
--
-- Since 1.0.0
sumC :: (Monad m, Num a) => ConduitT a o m a
sumC = CC.sum
{-# INLINE sumC #-}

-- | Get the sum of all elements in the chunked stream.
--
-- Since 1.0.0
sumCE :: (Monad m, MonoFoldable mono, Num (Element mono)) => ConduitT mono o m (Element mono)
sumCE = CC.sumE
{-# INLINE sumCE #-}

-- | Get the product of all values in the stream.
--
-- Since 1.0.0
productC :: (Monad m, Num a) => ConduitT a o m a
productC = CC.product
{-# INLINE productC #-}

-- | Get the product of all elements in the chunked stream.
--
-- Since 1.0.0
productCE :: (Monad m, MonoFoldable mono, Num (Element mono)) => ConduitT mono o m (Element mono)
productCE = CC.productE
{-# INLINE productCE #-}

-- | Find the first matching value.
--
-- Since 1.0.0
findC :: Monad m => (a -> Bool) -> ConduitT a o m (Maybe a)
findC = CC.find
{-# INLINE findC #-}

-- | Apply the action to all values in the stream.
--
-- Since 1.0.0
mapM_C :: Monad m => (a -> m ()) -> ConduitT a o m ()
mapM_C = CC.mapM_
{-# INLINE mapM_C #-}

-- | Apply the action to all elements in the chunked stream.
--
-- Since 1.0.0
mapM_CE :: (Monad m, MonoFoldable mono) => (Element mono -> m ()) -> ConduitT mono o m ()
mapM_CE = CC.mapM_E
{-# INLINE mapM_CE #-}

-- | A monadic strict left fold.
--
-- Since 1.0.0
foldMC :: Monad m => (a -> b -> m a) -> a -> ConduitT b o m a
foldMC = CC.foldM
{-# INLINE foldMC #-}

-- | A monadic strict left fold on a chunked stream.
--
-- Since 1.0.0
foldMCE :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> m a)
       -> a
       -> ConduitT mono o m a
foldMCE = CC.foldME
{-# INLINE foldMCE #-}

-- | Apply the provided monadic mapping function and monoidal combine all values.
--
-- Since 1.0.0
foldMapMC :: (Monad m, Monoid w) => (a -> m w) -> ConduitT a o m w
foldMapMC = CC.foldMapM
{-# INLINE foldMapMC #-}

-- | Apply the provided monadic mapping function and monoidal combine all
-- elements in the chunked stream.
--
-- Since 1.0.0
foldMapMCE :: (Monad m, MonoFoldable mono, Monoid w)
          => (Element mono -> m w)
          -> ConduitT mono o m w
foldMapMCE = CC.foldMapME
{-# INLINE foldMapMCE #-}

-- | Print all incoming values to stdout.
--
-- Since 1.0.0
printC :: (Show a, MonadIO m) => ConduitT a o m ()
printC = CC.print
{-# INLINE printC #-}

-- | @sinkHandle@ applied to @stdout@.
--
-- Since 1.0.0
stdoutC :: MonadIO m => ConduitT ByteString o m ()
stdoutC = CC.stdout
{-# INLINE stdoutC #-}

-- | @sinkHandle@ applied to @stderr@.
--
-- Since 1.0.0
stderrC :: MonadIO m => ConduitT ByteString o m ()
stderrC = CC.stderr
{-# INLINE stderrC #-}

-- | Apply a transformation to all values in a stream.
--
-- Since 1.0.0
mapC :: Monad m => (a -> b) -> ConduitT a b m ()
mapC = CC.map
{-# INLINE mapC #-}

-- | Apply a transformation to all elements in a chunked stream.
--
-- Since 1.0.0
mapCE :: (Monad m, Functor f) => (a -> b) -> ConduitT (f a) (f b) m ()
mapCE = CC.mapE
{-# INLINE mapCE #-}

-- | Apply a monomorphic transformation to all elements in a chunked stream.
--
-- Unlike @mapE@, this will work on types like @ByteString@ and @Text@ which
-- are @MonoFunctor@ but not @Functor@.
--
-- Since 1.0.0
omapCE :: (Monad m, MonoFunctor mono) => (Element mono -> Element mono) -> ConduitT mono mono m ()
omapCE = CC.omapE
{-# INLINE omapCE #-}

-- | Apply the function to each value in the stream, resulting in a foldable
-- value (e.g., a list). Then yield each of the individual values in that
-- foldable value separately.
--
-- Generalizes concatMap, mapMaybe, and mapFoldable.
--
-- Since 1.0.0
concatMapC :: (Monad m, MonoFoldable mono)
          => (a -> mono)
          -> ConduitT a (Element mono) m ()
concatMapC = CC.concatMap
{-# INLINE concatMapC #-}

-- | Apply the function to each element in the chunked stream, resulting in a
-- foldable value (e.g., a list). Then yield each of the individual values in
-- that foldable value separately.
--
-- Generalizes concatMap, mapMaybe, and mapFoldable.
--
-- Since 1.0.0
concatMapCE :: (Monad m, MonoFoldable mono, Monoid w)
           => (Element mono -> w)
           -> ConduitT mono w m ()
concatMapCE = CC.concatMapE
{-# INLINE concatMapCE #-}

-- | Stream up to n number of values downstream.
--
-- Note that, if downstream terminates early, not all values will be consumed.
-- If you want to force /exactly/ the given number of values to be consumed,
-- see 'takeExactly'.
--
-- Since 1.0.0
takeC :: Monad m => Int -> ConduitT a a m ()
takeC = CC.take
{-# INLINE takeC #-}

-- | Stream up to n number of elements downstream in a chunked stream.
--
-- Note that, if downstream terminates early, not all values will be consumed.
-- If you want to force /exactly/ the given number of values to be consumed,
-- see 'takeExactlyE'.
--
-- Since 1.0.0
takeCE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> ConduitT seq seq m ()
takeCE = CC.takeE
{-# INLINE takeCE #-}

-- | Stream all values downstream that match the given predicate.
--
-- Same caveats regarding downstream termination apply as with 'take'.
--
-- Since 1.0.0
takeWhileC :: Monad m
          => (a -> Bool)
          -> ConduitT a a m ()
takeWhileC = CC.takeWhile
{-# INLINE takeWhileC #-}

-- | Stream all elements downstream that match the given predicate in a chunked stream.
--
-- Same caveats regarding downstream termination apply as with 'takeE'.
--
-- Since 1.0.0
takeWhileCE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> ConduitT seq seq m ()
takeWhileCE = CC.takeWhileE
{-# INLINE takeWhileCE #-}

-- | Consume precisely the given number of values and feed them downstream.
--
-- This function is in contrast to 'take', which will only consume up to the
-- given number of values, and will terminate early if downstream terminates
-- early. This function will discard any additional values in the stream if
-- they are unconsumed.
--
-- Note that this function takes a downstream @ConduitT@ as a parameter, as
-- opposed to working with normal fusion. For more information, see
-- <http://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit>, the section
-- titled \"pipes and conduit: isolate\".
--
-- Since 1.0.0
takeExactlyC :: Monad m
            => Int
            -> ConduitT a b m r
            -> ConduitT a b m r
takeExactlyC = CC.takeExactly
{-# INLINE takeExactlyC #-}

-- | Same as 'takeExactly', but for chunked streams.
--
-- Since 1.0.0
takeExactlyCE :: (Monad m, Seq.IsSequence a)
             => Seq.Index a
             -> ConduitT a b m r
             -> ConduitT a b m r
takeExactlyCE = CC.takeExactlyE
{-# INLINE takeExactlyCE #-}

-- | Flatten out a stream by yielding the values contained in an incoming
-- @MonoFoldable@ as individually yielded values.
--
-- Since 1.0.0
concatC :: (Monad m, MonoFoldable mono)
       => ConduitT mono (Element mono) m ()
concatC = CC.concat
{-# INLINE concatC #-}

-- | Keep only values in the stream passing a given predicate.
--
-- Since 1.0.0
filterC :: Monad m => (a -> Bool) -> ConduitT a a m ()
filterC = CC.filter
{-# INLINE filterC #-}

-- | Keep only elements in the chunked stream passing a given predicate.
--
-- Since 1.0.0
filterCE :: (Seq.IsSequence seq, Monad m) => (Element seq -> Bool) -> ConduitT seq seq m ()
filterCE = CC.filterE
{-# INLINE filterCE #-}

-- | Map values as long as the result is @Just@.
--
-- Since 1.0.0
mapWhileC :: Monad m => (a -> Maybe b) -> ConduitT a b m ()
mapWhileC = CC.mapWhile
{-# INLINE mapWhileC #-}

-- | Break up a stream of values into vectors of size n. The final vector may
-- be smaller than n if the total number of values is not a strict multiple of
-- n. No empty vectors will be yielded.
--
-- Since 1.0.0
conduitVector :: (V.Vector v a, PrimMonad m)
              => Int -- ^ maximum allowed size
              -> ConduitT a (v a) m ()
conduitVector = CC.conduitVector
{-# INLINE conduitVector #-}

-- | Analog of 'Prelude.scanl' for lists.
--
-- Since 1.0.6
scanlC :: Monad m => (a -> b -> a) -> a -> ConduitT b a m ()
scanlC = CC.scanl
{-# INLINE scanlC #-}

-- | 'mapWhileC' with a break condition dependent on a strict accumulator.
-- Equivalently, 'CL.mapAccum' as long as the result is @Right@. Instead of
-- producing a leftover, the breaking input determines the resulting
-- accumulator via @Left@.
mapAccumWhileC :: Monad m =>
    (a -> s -> Either s (s, b)) -> s -> ConduitT a b m s
mapAccumWhileC = CC.mapAccumWhile
{-# INLINE mapAccumWhileC #-}

-- | 'concatMap' with an accumulator.
--
-- Since 1.0.0
concatMapAccumC :: Monad m => (a -> accum -> (accum, [b])) -> accum -> ConduitT a b m ()
concatMapAccumC = CC.concatMapAccum
{-# INLINE concatMapAccumC #-}

-- | Insert the given value between each two values in the stream.
--
-- Since 1.0.0
intersperseC :: Monad m => a -> ConduitT a a m ()
intersperseC = CC.intersperse
{-# INLINE intersperseC #-}

-- | Sliding window of values
-- 1,2,3,4,5 with window size 2 gives
-- [1,2],[2,3],[3,4],[4,5]
--
-- Best used with structures that support O(1) snoc.
--
-- Since 1.0.0
slidingWindowC :: (Monad m, Seq.IsSequence seq, Element seq ~ a) => Int -> ConduitT a seq m ()
slidingWindowC = CC.slidingWindow
{-# INLINE slidingWindowC #-}


-- | Split input into chunk of size 'chunkSize'
--
-- The last element may be smaller than the 'chunkSize' (see also
-- 'chunksOfExactlyE' which will not yield this last element)
--
-- @since 1.1.2
chunksOfCE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> ConduitT seq seq m ()
chunksOfCE = CC.chunksOfE
{-# INLINE chunksOfCE #-}

-- | Split input into chunk of size 'chunkSize'
--
-- If the input does not split into chunks exactly, the remainder will be
-- leftover (see also 'chunksOfE')
--
-- @since 1.1.2
chunksOfExactlyCE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> ConduitT seq seq m ()
chunksOfExactlyCE = CC.chunksOfExactlyE
{-# INLINE chunksOfExactlyCE #-}

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Since 1.0.0
mapMC :: Monad m => (a -> m b) -> ConduitT a b m ()
mapMC = CC.mapM
{-# INLINE mapMC #-}

-- | Apply a monadic transformation to all elements in a chunked stream.
--
-- Since 1.0.0
mapMCE :: (Monad m, Data.Traversable.Traversable f) => (a -> m b) -> ConduitT (f a) (f b) m ()
mapMCE = CC.mapME
{-# INLINE mapMCE #-}

-- | Apply a monadic monomorphic transformation to all elements in a chunked stream.
--
-- Unlike @mapME@, this will work on types like @ByteString@ and @Text@ which
-- are @MonoFunctor@ but not @Functor@.
--
-- Since 1.0.0
omapMCE :: (Monad m, MonoTraversable mono)
       => (Element mono -> m (Element mono))
       -> ConduitT mono mono m ()
omapMCE = CC.omapME
{-# INLINE omapMCE #-}

-- | Apply the monadic function to each value in the stream, resulting in a
-- foldable value (e.g., a list). Then yield each of the individual values in
-- that foldable value separately.
--
-- Generalizes concatMapM, mapMaybeM, and mapFoldableM.
--
-- Since 1.0.0
concatMapMC :: (Monad m, MonoFoldable mono)
           => (a -> m mono)
           -> ConduitT a (Element mono) m ()
concatMapMC = CC.concatMapM
{-# INLINE concatMapMC #-}

-- | Keep only values in the stream passing a given monadic predicate.
--
-- Since 1.0.0
filterMC :: Monad m
        => (a -> m Bool)
        -> ConduitT a a m ()
filterMC = CC.filterM
{-# INLINE filterMC #-}

-- | Keep only elements in the chunked stream passing a given monadic predicate.
--
-- Since 1.0.0
filterMCE :: (Monad m, Seq.IsSequence seq) => (Element seq -> m Bool) -> ConduitT seq seq m ()
filterMCE = CC.filterME
{-# INLINE filterMCE #-}

-- | Apply a monadic action on all values in a stream.
--
-- This @Conduit@ can be used to perform a monadic side-effect for every
-- value, whilst passing the value through the @Conduit@ as-is.
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
--
-- Since 1.0.0
iterMC :: Monad m => (a -> m ()) -> ConduitT a a m ()
iterMC = CC.iterM
{-# INLINE iterMC #-}

-- | Analog of 'Prelude.scanl' for lists, monadic.
--
-- Since 1.0.6
scanlMC :: Monad m => (a -> b -> m a) -> a -> ConduitT b a m ()
scanlMC = CC.scanlM
{-# INLINE scanlMC #-}

-- | Monadic `mapAccumWhileC`.
mapAccumWhileMC :: Monad m => (a -> s -> m (Either s (s, b))) -> s -> ConduitT a b m s
mapAccumWhileMC = CC.mapAccumWhileM
{-# INLINE mapAccumWhileMC #-}

-- | 'concatMapM' with an accumulator.
--
-- Since 1.0.0
concatMapAccumMC :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> ConduitT a b m ()
concatMapAccumMC = CC.concatMapAccumM
{-# INLINE concatMapAccumMC #-}

-- | Encode a stream of text as UTF8.
--
-- Since 1.0.0
encodeUtf8C :: (Monad m, DTE.Utf8 text binary) => ConduitT text binary m ()
encodeUtf8C = CC.encodeUtf8
{-# INLINE encodeUtf8C #-}

-- | Decode a stream of binary data as UTF8.
--
-- Since 1.0.0
decodeUtf8C :: MonadThrow m => ConduitT ByteString Text m ()
decodeUtf8C = CC.decodeUtf8
{-# INLINE decodeUtf8C #-}

-- | Decode a stream of binary data as UTF8, replacing any invalid bytes with
-- the Unicode replacement character.
--
-- Since 1.0.0
decodeUtf8LenientC :: Monad m => ConduitT ByteString Text m ()
decodeUtf8LenientC = CC.decodeUtf8Lenient
{-# INLINE decodeUtf8LenientC #-}

-- | Stream in the entirety of a single line.
--
-- Like @takeExactly@, this will consume the entirety of the line regardless of
-- the behavior of the inner Conduit.
--
-- Since 1.0.0
lineC :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
     => ConduitT seq o m r
     -> ConduitT seq o m r
lineC = CC.line
{-# INLINE lineC #-}

-- | Same as 'line', but operates on ASCII/binary data.
--
-- Since 1.0.0
lineAsciiC :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8)
          => ConduitT seq o m r
          -> ConduitT seq o m r
lineAsciiC = CC.lineAscii
{-# INLINE lineAsciiC #-}

-- | Insert a newline character after each incoming chunk of data.
--
-- Since 1.0.0
unlinesC :: (Monad m, Seq.IsSequence seq, Element seq ~ Char) => ConduitT seq seq m ()
unlinesC = CC.unlines
{-# INLINE unlinesC #-}

-- | Same as 'unlines', but operates on ASCII/binary data.
--
-- Since 1.0.0
unlinesAsciiC :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8) => ConduitT seq seq m ()
unlinesAsciiC = CC.unlinesAscii
{-# INLINE unlinesAsciiC #-}

-- | Convert a stream of arbitrarily-chunked textual data into a stream of data
-- where each chunk represents a single line. Note that, if you have
-- unknown/untrusted input, this function is /unsafe/, since it would allow an
-- attacker to form lines of massive length and exhaust memory.
--
-- Since 1.0.0
linesUnboundedC :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
               => ConduitT seq seq m ()
linesUnboundedC = CC.linesUnbounded
{-# INLINE linesUnboundedC #-}

-- | Same as 'linesUnbounded', but for ASCII/binary data.
--
-- Since 1.0.0
linesUnboundedAsciiC :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8)
                    => ConduitT seq seq m ()
linesUnboundedAsciiC = CC.linesUnboundedAscii
{-# INLINE linesUnboundedAsciiC #-}

-- | Generally speaking, yielding values from inside a Conduit requires
-- some allocation for constructors. This can introduce an overhead,
-- similar to the overhead needed to represent a list of values instead of
-- a vector. This overhead is even more severe when talking about unboxed
-- values.
--
-- This combinator allows you to overcome this overhead, and efficiently
-- fill up vectors. It takes two parameters. The first is the size of each
-- mutable vector to be allocated. The second is a function. The function
-- takes an argument which will yield the next value into a mutable
-- vector.
--
-- Under the surface, this function uses a number of tricks to get high
-- performance. For more information on both usage and implementation,
-- please see:
-- <https://www.fpcomplete.com/user/snoyberg/library-documentation/vectorbuilder>
--
-- Since 1.0.0
vectorBuilderC :: (PrimMonad m, V.Vector v e, PrimMonad n, PrimState m ~ PrimState n)
              => Int -- ^ size
              -> ((e -> n ()) -> ConduitT i Void m r)
              -> ConduitT i (v e) m r
vectorBuilderC = CC.vectorBuilder
{-# INLINE vectorBuilderC #-}
