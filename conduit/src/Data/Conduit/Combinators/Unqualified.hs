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

import qualified Data.NonNull as NonNull
import qualified Data.Traversable
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.Trans.Resource (MonadResource, MonadThrow)
import           Data.Conduit
import           Data.Monoid                 (Monoid (..))
import           Data.MonoTraversable
import qualified Data.Sequences              as Seq
import qualified Data.Vector.Generic         as V
import           Prelude                     (Bool (..), Eq (..), Int,
                                              Maybe (..), Monad (..), Num (..),
                                              Ord (..), Functor (..), Either (..),
                                              Enum, Show, Char, FilePath)
import Data.Word (Word8)
import qualified System.IO                   as SIO
import Data.ByteString (ByteString)
import Data.Text (Text)

#if MIN_VERSION_mono_traversable(1,0,0)
import qualified Data.Sequences as DTE
import           Data.Sequences (LazySequence (..))
#else
import           Data.Sequences.Lazy
import qualified Data.Textual.Encoding as DTE
#endif


-- END IMPORTS

-- | Yield each of the values contained by the given @MonoFoldable@.
--
-- This will work on many data structures, including lists, @ByteString@s, and @Vector@s.
--
-- Since 1.0.0
yieldMany :: (Monad m, MonoFoldable mono)
          => mono
          -> Producer m (Element mono)
yieldMany = CC.yieldMany
{-# INLINE yieldMany #-}

-- | Generate a producer from a seed value.
--
-- Since 1.0.0
unfoldC :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> Producer m a
unfoldC = CC.unfold
{-# INLINE unfoldC #-}

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Since 1.0.0
enumFromToC :: (Monad m, Enum a, Ord a) => a -> a -> Producer m a
enumFromToC = CC.enumFromTo
{-# INLINE enumFromToC #-}

-- | Produces an infinite stream of repeated applications of f to x.
--
-- Since 1.0.0
iterateC :: Monad m => (a -> a) -> a -> Producer m a
iterateC = CC.iterate
{-# INLINE iterateC #-}

-- | Produce an infinite stream consisting entirely of the given value.
--
-- Since 1.0.0
repeatC :: Monad m => a -> Producer m a
repeatC = CC.repeat
{-# INLINE repeatC #-}

-- | Produce a finite stream consisting of n copies of the given value.
--
-- Since 1.0.0
replicateC :: Monad m
          => Int
          -> a
          -> Producer m a
replicateC = CC.replicate
{-# INLINE replicateC #-}

-- | Generate a producer by yielding each of the strict chunks in a @LazySequence@.
--
-- For more information, see 'toChunks'.
--
-- Since 1.0.0
sourceLazy :: (Monad m, LazySequence lazy strict)
           => lazy
           -> Producer m strict
sourceLazy = CC.sourceLazy
{-# INLINE sourceLazy #-}

-- | Repeatedly run the given action and yield all values it produces.
--
-- Since 1.0.0
repeatMC :: Monad m
        => m a
        -> Producer m a
repeatMC = CC.repeatM
{-# INLINE repeatMC #-}

-- | Repeatedly run the given action and yield all values it produces, until
-- the provided predicate returns @False@.
--
-- Since 1.0.0
repeatWhileMC :: Monad m
             => m a
             -> (a -> Bool)
             -> Producer m a
repeatWhileMC = CC.repeatWhileM
{-# INLINE repeatWhileMC #-}

-- | Perform the given action n times, yielding each result.
--
-- Since 1.0.0
replicateMC :: Monad m
           => Int
           -> m a
           -> Producer m a
replicateMC = CC.replicateM
{-# INLINE replicateMC #-}

-- | @sourceHandle@ applied to @stdin@.
--
-- Since 1.0.0
stdinC :: MonadIO m => Producer m ByteString
stdinC = CC.stdin
{-# INLINE stdinC #-}

-- | Stream the contents of the given directory, without traversing deeply.
--
-- This function will return /all/ of the contents of the directory, whether
-- they be files, directories, etc.
--
-- Note that the generated filepaths will be the complete path, not just the
-- filename. In other words, if you have a directory @foo@ containing files
-- @bar@ and @baz@, and you use @sourceDirectory@ on @foo@, the results will be
-- @foo/bar@ and @foo/baz@.
--
-- Since 1.0.0
sourceDirectory :: MonadResource m => FilePath -> Producer m FilePath
sourceDirectory = CC.sourceDirectory
{-# INLINE sourceDirectory #-}

-- | Deeply stream the contents of the given directory.
--
-- This works the same as @sourceDirectory@, but will not return directories at
-- all. This function also takes an extra parameter to indicate whether
-- symlinks will be followed.
--
-- Since 1.0.0
sourceDirectoryDeep :: MonadResource m
                    => Bool -- ^ Follow directory symlinks
                    -> FilePath -- ^ Root directory
                    -> Producer m FilePath
sourceDirectoryDeep = CC.sourceDirectoryDeep
{-# INLINE sourceDirectoryDeep #-}

-- | Ignore a certain number of values in the stream.
--
-- Since 1.0.0
dropC :: Monad m
     => Int
     -> Consumer a m ()
dropC = CC.drop
{-# INLINE dropC #-}

-- | Drop a certain number of elements from a chunked stream.
--
-- Since 1.0.0
dropCE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> Consumer seq m ()
dropCE = CC.dropE
{-# INLINE dropCE #-}

-- | Drop all values which match the given predicate.
--
-- Since 1.0.0
dropWhileC :: Monad m
          => (a -> Bool)
          -> Consumer a m ()
dropWhileC = CC.dropWhile
{-# INLINE dropWhileC #-}

-- | Drop all elements in the chunked stream which match the given predicate.
--
-- Since 1.0.0
dropWhileCE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> Consumer seq m ()
dropWhileCE = CC.dropWhileE
{-# INLINE dropWhileCE #-}

-- | Monoidally combine all values in the stream.
--
-- Since 1.0.0
foldC :: (Monad m, Monoid a)
     => Consumer a m a
foldC = CC.fold
{-# INLINE foldC #-}

-- | Monoidally combine all elements in the chunked stream.
--
-- Since 1.0.0
foldCE :: (Monad m, MonoFoldable mono, Monoid (Element mono))
      => Consumer mono m (Element mono)
foldCE = CC.foldE
{-# INLINE foldCE #-}

-- | A strict left fold.
--
-- Since 1.0.0
foldlC :: Monad m => (a -> b -> a) -> a -> Consumer b m a
foldlC = CC.foldl
{-# INLINE foldlC #-}

-- | A strict left fold on a chunked stream.
--
-- Since 1.0.0
foldlCE :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> a)
       -> a
       -> Consumer mono m a
foldlCE = CC.foldlE
{-# INLINE foldlCE #-}

-- | Apply the provided mapping function and monoidal combine all values.
--
-- Since 1.0.0
foldMapC :: (Monad m, Monoid b)
        => (a -> b)
        -> Consumer a m b
foldMapC = CC.foldMap
{-# INLINE foldMapC #-}

-- | Apply the provided mapping function and monoidal combine all elements of the chunked stream.
--
-- Since 1.0.0
foldMapCE :: (Monad m, MonoFoldable mono, Monoid w)
         => (Element mono -> w)
         -> Consumer mono m w
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
    -> Consumer a m Bool
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
     -> Consumer mono m Bool
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
    -> Consumer a m Bool
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
     -> Consumer mono m Bool
anyCE = CC.anyE
{-# INLINE anyCE #-}

-- | Are all values in the stream True?
--
-- Consumption stops once the first False is encountered.
--
-- Since 1.0.0
andC :: Monad m => Consumer Bool m Bool
andC = CC.and
{-# INLINE andC #-}

-- | Are all elements in the chunked stream True?
--
-- Consumption stops once the first False is encountered.
--
-- Since 1.0.0
andCE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
     => Consumer mono m Bool
andCE = CC.andE
{-# INLINE andCE #-}

-- | Are any values in the stream True?
--
-- Consumption stops once the first True is encountered.
--
-- Since 1.0.0
orC :: Monad m => Consumer Bool m Bool
orC = CC.or
{-# INLINE orC #-}

-- | Are any elements in the chunked stream True?
--
-- Consumption stops once the first True is encountered.
--
-- Since 1.0.0
orCE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
    => Consumer mono m Bool
orCE = CC.orE
{-# INLINE orCE #-}

-- | 'Alternative'ly combine all values in the stream.
--
-- Since 1.1.1
asumC = CC.asum

-- | Are any values in the stream equal to the given value?
--
-- Stops consuming as soon as a match is found.
--
-- Since 1.0.0
elemC :: (Monad m, Eq a) => a -> Consumer a m Bool
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
      -> Consumer seq m Bool
elemCE = CC.elemE
{-# INLINE elemCE #-}

-- | Are no values in the stream equal to the given value?
--
-- Stops consuming as soon as a match is found.
--
-- Since 1.0.0
notElemC :: (Monad m, Eq a) => a -> Consumer a m Bool
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
         -> Consumer seq m Bool
notElemCE = CC.notElemE
{-# INLINE notElemCE #-}

-- | Consume all incoming strict chunks into a lazy sequence.
-- Note that the entirety of the sequence will be resident at memory.
--
-- This can be used to consume a stream of strict ByteStrings into a lazy
-- ByteString, for example.
--
-- Since 1.0.0
sinkLazy :: (Monad m, LazySequence lazy strict)
         => Consumer strict m lazy
sinkLazy = CC.sinkLazy
{-# INLINE sinkLazy #-}

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory.
--
-- Since 1.0.0
sinkList :: Monad m => Consumer a m [a]
sinkList = CC.sinkList
{-# INLINE sinkList #-}

-- | Sink incoming values into a vector, growing the vector as necessary to fit
-- more elements.
--
-- Note that using this function is more memory efficient than @sinkList@ and
-- then converting to a @Vector@, as it avoids intermediate list constructors.
--
-- Since 1.0.0
sinkVector :: (V.Vector v a, PrimMonad m)
           => Consumer a m (v a)
sinkVector = CC.sinkVector
{-# INLINE sinkVector #-}

-- | Sink incoming values into a vector, up until size @maxSize@.  Subsequent
-- values will be left in the stream. If there are less than @maxSize@ values
-- present, returns a @Vector@ of smaller size.
--
-- Note that using this function is more memory efficient than @sinkList@ and
-- then converting to a @Vector@, as it avoids intermediate list constructors.
--
-- Since 1.0.0
sinkVectorN :: (V.Vector v a, PrimMonad m)
            => Int -- ^ maximum allowed size
            -> Consumer a m (v a)
sinkVectorN = CC.sinkVectorN
{-# INLINE sinkVectorN #-}

-- | Consume and discard all remaining values in the stream.
--
-- Since 1.0.0
sinkNull :: Monad m => Consumer a m ()
sinkNull = CC.sinkNull
{-# INLINE sinkNull #-}

-- | Same as @await@, but discards any leading 'onull' values.
--
-- Since 1.0.0
awaitNonNull :: (Monad m, MonoFoldable a) => Consumer a m (Maybe (NonNull.NonNull a))
awaitNonNull = CC.awaitNonNull
{-# INLINE awaitNonNull #-}

-- | Take a single value from the stream, if available.
--
-- Since 1.0.5
headC :: Monad m => Consumer a m (Maybe a)
headC = CC.head

-- | Same as 'headC', but returns a default value if none are available from the stream.
--
-- Since 1.0.5
headDefC :: Monad m => a -> Consumer a m a
headDefC = CC.headDef

-- | Get the next element in the chunked stream.
--
-- Since 1.0.0
headCE :: (Monad m, Seq.IsSequence seq) => Consumer seq m (Maybe (Element seq))
headCE = CC.headE
{-# INLINE headCE #-}

-- | View the next value in the stream without consuming it.
--
-- Since 1.0.0
peekC :: Monad m => Consumer a m (Maybe a)
peekC = CC.peek
{-# INLINE peekC #-}

-- | View the next element in the chunked stream without consuming it.
--
-- Since 1.0.0
peekCE :: (Monad m, MonoFoldable mono) => Consumer mono m (Maybe (Element mono))
peekCE = CC.peekE
{-# INLINE peekCE #-}

-- | Retrieve the last value in the stream, if present.
--
-- Since 1.0.0
lastC :: Monad m => Consumer a m (Maybe a)
lastC = CC.last
{-# INLINE lastC #-}

-- | Same as 'lastC', but returns a default value if none are available from the stream.
--
-- Since 1.0.5
lastDefC :: Monad m => a -> Consumer a m a
lastDefC = CC.lastDef

-- | Retrieve the last element in the chunked stream, if present.
--
-- Since 1.0.0
lastCE :: (Monad m, Seq.IsSequence seq) => Consumer seq m (Maybe (Element seq))
lastCE = CC.lastE
{-# INLINE lastCE #-}

-- | Count how many values are in the stream.
--
-- Since 1.0.0
lengthC :: (Monad m, Num len) => Consumer a m len
lengthC = CC.length
{-# INLINE lengthC #-}

-- | Count how many elements are in the chunked stream.
--
-- Since 1.0.0
lengthCE :: (Monad m, Num len, MonoFoldable mono) => Consumer mono m len
lengthCE = CC.lengthE
{-# INLINE lengthCE #-}

-- | Count how many values in the stream pass the given predicate.
--
-- Since 1.0.0
lengthIfC :: (Monad m, Num len) => (a -> Bool) -> Consumer a m len
lengthIfC = CC.lengthIf
{-# INLINE lengthIfC #-}

-- | Count how many elements in the chunked stream pass the given predicate.
--
-- Since 1.0.0
lengthIfCE :: (Monad m, Num len, MonoFoldable mono)
          => (Element mono -> Bool) -> Consumer mono m len
lengthIfCE = CC.lengthIfE
{-# INLINE lengthIfCE #-}

-- | Get the largest value in the stream, if present.
--
-- Since 1.0.0
maximumC :: (Monad m, Ord a) => Consumer a m (Maybe a)
maximumC = CC.maximum
{-# INLINE maximumC #-}

-- | Get the largest element in the chunked stream, if present.
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
maximumCE :: (Monad m, Seq.IsSequence seq, Ord (Element seq)) => Consumer seq m (Maybe (Element seq))
#else
maximumCE :: (Monad m, Seq.OrdSequence seq) => Consumer seq m (Maybe (Element seq))
#endif
maximumCE = CC.maximumE
{-# INLINE maximumCE #-}

-- | Get the smallest value in the stream, if present.
--
-- Since 1.0.0
minimumC :: (Monad m, Ord a) => Consumer a m (Maybe a)
minimumC = CC.minimum
{-# INLINE minimumC #-}

-- | Get the smallest element in the chunked stream, if present.
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
minimumCE :: (Monad m, Seq.IsSequence seq, Ord (Element seq)) => Consumer seq m (Maybe (Element seq))
#else
minimumCE :: (Monad m, Seq.OrdSequence seq) => Consumer seq m (Maybe (Element seq))
#endif
minimumCE = CC.minimumE
{-# INLINE minimumCE #-}

-- | True if there are no values in the stream.
--
-- This function does not modify the stream.
--
-- Since 1.0.0
nullC :: Monad m => Consumer a m Bool
nullC = CC.null
{-# INLINE nullC #-}

-- | True if there are no elements in the chunked stream.
--
-- This function may remove empty leading chunks from the stream, but otherwise
-- will not modify it.
--
-- Since 1.0.0
nullCE :: (Monad m, MonoFoldable mono)
      => Consumer mono m Bool
nullCE = CC.nullE
{-# INLINE nullCE #-}

-- | Get the sum of all values in the stream.
--
-- Since 1.0.0
sumC :: (Monad m, Num a) => Consumer a m a
sumC = CC.sum
{-# INLINE sumC #-}

-- | Get the sum of all elements in the chunked stream.
--
-- Since 1.0.0
sumCE :: (Monad m, MonoFoldable mono, Num (Element mono)) => Consumer mono m (Element mono)
sumCE = CC.sumE
{-# INLINE sumCE #-}

-- | Get the product of all values in the stream.
--
-- Since 1.0.0
productC :: (Monad m, Num a) => Consumer a m a
productC = CC.product
{-# INLINE productC #-}

-- | Get the product of all elements in the chunked stream.
--
-- Since 1.0.0
productCE :: (Monad m, MonoFoldable mono, Num (Element mono)) => Consumer mono m (Element mono)
productCE = CC.productE
{-# INLINE productCE #-}

-- | Find the first matching value.
--
-- Since 1.0.0
findC :: Monad m => (a -> Bool) -> Consumer a m (Maybe a)
findC = CC.find
{-# INLINE findC #-}

-- | Apply the action to all values in the stream.
--
-- Since 1.0.0
mapM_C :: Monad m => (a -> m ()) -> Consumer a m ()
mapM_C = CC.mapM_
{-# INLINE mapM_C #-}

-- | Apply the action to all elements in the chunked stream.
--
-- Since 1.0.0
mapM_CE :: (Monad m, MonoFoldable mono) => (Element mono -> m ()) -> Consumer mono m ()
mapM_CE = CC.mapM_E
{-# INLINE mapM_CE #-}

-- | A monadic strict left fold.
--
-- Since 1.0.0
foldMC :: Monad m => (a -> b -> m a) -> a -> Consumer b m a
foldMC = CC.foldM
{-# INLINE foldMC #-}

-- | A monadic strict left fold on a chunked stream.
--
-- Since 1.0.0
foldMCE :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> m a)
       -> a
       -> Consumer mono m a
foldMCE = CC.foldME
{-# INLINE foldMCE #-}

-- | Apply the provided monadic mapping function and monoidal combine all values.
--
-- Since 1.0.0
foldMapMC :: (Monad m, Monoid w) => (a -> m w) -> Consumer a m w
foldMapMC = CC.foldMapM
{-# INLINE foldMapMC #-}

-- | Apply the provided monadic mapping function and monoidal combine all
-- elements in the chunked stream.
--
-- Since 1.0.0
foldMapMCE :: (Monad m, MonoFoldable mono, Monoid w)
          => (Element mono -> m w)
          -> Consumer mono m w
foldMapMCE = CC.foldMapME
{-# INLINE foldMapMCE #-}

-- | Print all incoming values to stdout.
--
-- Since 1.0.0
printC :: (Show a, MonadIO m) => Consumer a m ()
printC = CC.print
{-# INLINE printC #-}

-- | @sinkHandle@ applied to @stdout@.
--
-- Since 1.0.0
stdoutC :: MonadIO m => Consumer ByteString m ()
stdoutC = CC.stdout
{-# INLINE stdoutC #-}

-- | @sinkHandle@ applied to @stderr@.
--
-- Since 1.0.0
stderrC :: MonadIO m => Consumer ByteString m ()
stderrC = CC.stderr
{-# INLINE stderrC #-}

-- | Apply a transformation to all values in a stream.
--
-- Since 1.0.0
mapC :: Monad m => (a -> b) -> Conduit a m b
mapC = CC.map
{-# INLINE mapC #-}

-- | Apply a transformation to all elements in a chunked stream.
--
-- Since 1.0.0
mapCE :: (Monad m, Functor f) => (a -> b) -> Conduit (f a) m (f b)
mapCE = CC.mapE
{-# INLINE mapCE #-}

-- | Apply a monomorphic transformation to all elements in a chunked stream.
--
-- Unlike @mapE@, this will work on types like @ByteString@ and @Text@ which
-- are @MonoFunctor@ but not @Functor@.
--
-- Since 1.0.0
omapCE :: (Monad m, MonoFunctor mono) => (Element mono -> Element mono) -> Conduit mono m mono
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
          -> Conduit a m (Element mono)
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
           -> Conduit mono m w
concatMapCE = CC.concatMapE
{-# INLINE concatMapCE #-}

-- | Stream up to n number of values downstream.
--
-- Note that, if downstream terminates early, not all values will be consumed.
-- If you want to force /exactly/ the given number of values to be consumed,
-- see 'takeExactly'.
--
-- Since 1.0.0
takeC :: Monad m => Int -> Conduit a m a
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
      -> Conduit seq m seq
takeCE = CC.takeE
{-# INLINE takeCE #-}

-- | Stream all values downstream that match the given predicate.
--
-- Same caveats regarding downstream termination apply as with 'take'.
--
-- Since 1.0.0
takeWhileC :: Monad m
          => (a -> Bool)
          -> Conduit a m a
takeWhileC = CC.takeWhile
{-# INLINE takeWhileC #-}

-- | Stream all elements downstream that match the given predicate in a chunked stream.
--
-- Same caveats regarding downstream termination apply as with 'takeE'.
--
-- Since 1.0.0
takeWhileCE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> Conduit seq m seq
takeWhileCE = CC.takeWhileE
{-# INLINE takeWhileCE #-}

-- | Consume precisely the given number of values and feed them downstream.
--
-- This function is in contrast to 'take', which will only consume up to the
-- given number of values, and will terminate early if downstream terminates
-- early. This function will discard any additional values in the stream if
-- they are unconsumed.
--
-- Note that this function takes a downstream @ConduitM@ as a parameter, as
-- opposed to working with normal fusion. For more information, see
-- <http://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit>, the section
-- titled \"pipes and conduit: isolate\".
--
-- Since 1.0.0
takeExactlyC :: Monad m
            => Int
            -> ConduitM a b m r
            -> ConduitM a b m r
takeExactlyC = CC.takeExactly
{-# INLINE takeExactlyC #-}

-- | Same as 'takeExactly', but for chunked streams.
--
-- Since 1.0.0
takeExactlyCE :: (Monad m, Seq.IsSequence a)
             => Seq.Index a
             -> ConduitM a b m r
             -> ConduitM a b m r
takeExactlyCE = CC.takeExactlyE
{-# INLINE takeExactlyCE #-}

-- | Flatten out a stream by yielding the values contained in an incoming
-- @MonoFoldable@ as individually yielded values.
--
-- Since 1.0.0
concatC :: (Monad m, MonoFoldable mono)
       => Conduit mono m (Element mono)
concatC = CC.concat
{-# INLINE concatC #-}

-- | Keep only values in the stream passing a given predicate.
--
-- Since 1.0.0
filterC :: Monad m => (a -> Bool) -> Conduit a m a
filterC = CC.filter
{-# INLINE filterC #-}

-- | Keep only elements in the chunked stream passing a given predicate.
--
-- Since 1.0.0
filterCE :: (Seq.IsSequence seq, Monad m) => (Element seq -> Bool) -> Conduit seq m seq
filterCE = CC.filterE
{-# INLINE filterCE #-}

-- | Map values as long as the result is @Just@.
--
-- Since 1.0.0
mapWhileC :: Monad m => (a -> Maybe b) -> Conduit a m b
mapWhileC = CC.mapWhile
{-# INLINE mapWhileC #-}

-- | Break up a stream of values into vectors of size n. The final vector may
-- be smaller than n if the total number of values is not a strict multiple of
-- n. No empty vectors will be yielded.
--
-- Since 1.0.0
conduitVector :: (V.Vector v a, PrimMonad m)
              => Int -- ^ maximum allowed size
              -> Conduit a m (v a)
conduitVector = CC.conduitVector
{-# INLINE conduitVector #-}

-- | Analog of 'Prelude.scanl' for lists.
--
-- Since 1.0.6
scanlC :: Monad m => (a -> b -> a) -> a -> Conduit b m a
scanlC = CC.scanl
{-# INLINE scanlC #-}

-- | 'mapWhileC' with a break condition dependent on a strict accumulator.
-- Equivalently, 'CL.mapAccum' as long as the result is @Right@. Instead of
-- producing a leftover, the breaking input determines the resulting
-- accumulator via @Left@.
mapAccumWhileC :: Monad m =>
    (a -> s -> Either s (s, b)) -> s -> ConduitM a b m s
mapAccumWhileC = CC.mapAccumWhile
{-# INLINE mapAccumWhileC #-}

-- | 'concatMap' with an accumulator.
--
-- Since 1.0.0
concatMapAccumC :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccumC = CC.concatMapAccum
{-# INLINE concatMapAccumC #-}

-- | Insert the given value between each two values in the stream.
--
-- Since 1.0.0
intersperseC :: Monad m => a -> Conduit a m a
intersperseC = CC.intersperse
{-# INLINE intersperseC #-}

-- | Sliding window of values
-- 1,2,3,4,5 with window size 2 gives
-- [1,2],[2,3],[3,4],[4,5]
--
-- Best used with structures that support O(1) snoc.
--
-- Since 1.0.0
slidingWindowC :: (Monad m, Seq.IsSequence seq, Element seq ~ a) => Int -> Conduit a m seq
slidingWindowC = CC.slidingWindow
{-# INLINE slidingWindowC #-}


-- | Split input into chunk of size 'chunkSize'
--
-- The last element may be smaller than the 'chunkSize' (see also
-- 'chunksOfExactlyE' which will not yield this last element)
--
-- @since 1.1.2
chunksOfCE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> Conduit seq m seq
chunksOfCE = CC.chunksOfE
{-# INLINE chunksOfCE #-}

-- | Split input into chunk of size 'chunkSize'
--
-- If the input does not split into chunks exactly, the remainder will be
-- leftover (see also 'chunksOfE')
--
-- @since 1.1.2
chunksOfExactlyCE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> Conduit seq m seq
chunksOfExactlyCE = CC.chunksOfExactlyE
{-# INLINE chunksOfExactlyCE #-}

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Since 1.0.0
mapMC :: Monad m => (a -> m b) -> Conduit a m b
mapMC = CC.mapM
{-# INLINE mapMC #-}

-- | Apply a monadic transformation to all elements in a chunked stream.
--
-- Since 1.0.0
mapMCE :: (Monad m, Data.Traversable.Traversable f) => (a -> m b) -> Conduit (f a) m (f b)
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
       -> Conduit mono m mono
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
           -> Conduit a m (Element mono)
concatMapMC = CC.concatMapM
{-# INLINE concatMapMC #-}

-- | Keep only values in the stream passing a given monadic predicate.
--
-- Since 1.0.0
filterMC :: Monad m
        => (a -> m Bool)
        -> Conduit a m a
filterMC = CC.filterM
{-# INLINE filterMC #-}

-- | Keep only elements in the chunked stream passing a given monadic predicate.
--
-- Since 1.0.0
filterMCE :: (Monad m, Seq.IsSequence seq) => (Element seq -> m Bool) -> Conduit seq m seq
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
iterMC :: Monad m => (a -> m ()) -> Conduit a m a
iterMC = CC.iterM
{-# INLINE iterMC #-}

-- | Analog of 'Prelude.scanl' for lists, monadic.
--
-- Since 1.0.6
scanlMC :: Monad m => (a -> b -> m a) -> a -> Conduit b m a
scanlMC = CC.scanlM
{-# INLINE scanlMC #-}

-- | Monadic `mapAccumWhileC`.
mapAccumWhileMC :: Monad m => (a -> s -> m (Either s (s, b))) -> s -> ConduitM a b m s
mapAccumWhileMC = CC.mapAccumWhileM
{-# INLINE mapAccumWhileMC #-}

-- | 'concatMapM' with an accumulator.
--
-- Since 1.0.0
concatMapAccumMC :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
concatMapAccumMC = CC.concatMapAccumM
{-# INLINE concatMapAccumMC #-}

-- | Encode a stream of text as UTF8.
--
-- Since 1.0.0
encodeUtf8C :: (Monad m, DTE.Utf8 text binary) => Conduit text m binary
encodeUtf8C = CC.encodeUtf8
{-# INLINE encodeUtf8C #-}

-- | Decode a stream of binary data as UTF8.
--
-- Since 1.0.0
decodeUtf8C :: MonadThrow m => Conduit ByteString m Text
decodeUtf8C = CC.decodeUtf8
{-# INLINE decodeUtf8C #-}

-- | Decode a stream of binary data as UTF8, replacing any invalid bytes with
-- the Unicode replacement character.
--
-- Since 1.0.0
decodeUtf8LenientC :: Monad m => Conduit ByteString m Text
decodeUtf8LenientC = CC.decodeUtf8Lenient
{-# INLINE decodeUtf8LenientC #-}

-- | Stream in the entirety of a single line.
--
-- Like @takeExactly@, this will consume the entirety of the line regardless of
-- the behavior of the inner Conduit.
--
-- Since 1.0.0
lineC :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
     => ConduitM seq o m r
     -> ConduitM seq o m r
lineC = CC.line
{-# INLINE lineC #-}

-- | Same as 'line', but operates on ASCII/binary data.
--
-- Since 1.0.0
lineAsciiC :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8)
          => ConduitM seq o m r
          -> ConduitM seq o m r
lineAsciiC = CC.lineAscii
{-# INLINE lineAsciiC #-}

-- | Insert a newline character after each incoming chunk of data.
--
-- Since 1.0.0
unlinesC :: (Monad m, Seq.IsSequence seq, Element seq ~ Char) => Conduit seq m seq
unlinesC = CC.unlines
{-# INLINE unlinesC #-}

-- | Same as 'unlines', but operates on ASCII/binary data.
--
-- Since 1.0.0
unlinesAsciiC :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8) => Conduit seq m seq
unlinesAsciiC = CC.unlinesAscii
{-# INLINE unlinesAsciiC #-}

-- | Convert a stream of arbitrarily-chunked textual data into a stream of data
-- where each chunk represents a single line. Note that, if you have
-- unknown/untrusted input, this function is /unsafe/, since it would allow an
-- attacker to form lines of massive length and exhaust memory.
--
-- Since 1.0.0
linesUnboundedC :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
               => Conduit seq m seq
linesUnboundedC = CC.linesUnbounded
{-# INLINE linesUnboundedC #-}

-- | Same as 'linesUnbounded', but for ASCII/binary data.
--
-- Since 1.0.0
linesUnboundedAsciiC :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8)
                    => Conduit seq m seq
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
              -> ((e -> n ()) -> Sink i m r)
              -> ConduitM i (v e) m r
vectorBuilderC = CC.vectorBuilder
{-# INLINE vectorBuilderC #-}
