{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
-- | Higher-level functions to interact with the elements of a stream. Most of
-- these are based on list functions.
--
-- For many purposes, it's recommended to use the conduit-combinators library,
-- which provides a more complete set of functions.
--
-- Note that these functions all deal with individual elements of a stream as a
-- sort of \"black box\", where there is no introspection of the contained
-- elements. Values such as @ByteString@ and @Text@ will likely need to be
-- treated specially to deal with their contents properly (@Word8@ and @Char@,
-- respectively). See the "Data.Conduit.Binary" and "Data.Conduit.Text"
-- modules.
module Data.Conduit.List
    ( -- * Sources
      sourceList
    , sourceNull
    , unfold
    , unfoldM
    , enumFromTo
    , iterate
    , replicate
    , replicateM
      -- * Sinks
      -- ** Pure
    , fold
    , foldMap
    , take
    , drop
    , head
    , peek
    , consume
    , sinkNull
      -- ** Monadic
    , foldMapM
    , foldM
    , mapM_
      -- * Conduits
      -- ** Pure
    , map
    , mapMaybe
    , mapFoldable
    , catMaybes
    , concat
    , concatMap
    , concatMapAccum
    , scanl
    , scan
    , mapAccum
    , groupBy
    , groupOn1
    , isolate
    , filter
      -- ** Monadic
    , mapM
    , iterM
    , scanlM
    , scanM
    , mapAccumM
    , mapMaybeM
    , mapFoldableM
    , concatMapM
    , concatMapAccumM
      -- * Misc
    , sequence
    ) where

import qualified Prelude
import Prelude
    ( ($), return, (==), (-), Int
    , (.), id, Maybe (..), Monad
    , Bool (..)
    , (>>)
    , (>>=)
    , seq
    , otherwise
    , Enum, Eq
    , maybe
    , (<=)
    )
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Foldable as F
import Data.Conduit
import Data.Conduit.Internal.Fusion
import Data.Conduit.Internal.List.Stream
import qualified Data.Conduit.Internal as CI
import Control.Monad (when, (<=<), liftM, void)
import Control.Monad.Trans.Class (lift)

-- Defines INLINE_RULE0, INLINE_RULE, STREAMING0, and STREAMING.
#include "fusion-macros.h"

-- | Generate a source from a seed value.
--
-- Subject to fusion
--
-- Since 0.4.2
unfold, unfoldC :: Monad m
                => (b -> Maybe (a, b))
                -> b
                -> Producer m a
unfoldC f =
    go
  where
    go seed =
        case f seed of
            Just (a, seed') -> yield a >> go seed'
            Nothing -> return ()
{-# INLINE unfoldC #-}
STREAMING(unfold, unfoldC, unfoldS, f x)

-- | A monadic unfold.
--
-- Subject to fusion
--
-- Since 1.1.2
unfoldM, unfoldMC :: Monad m
                  => (b -> m (Maybe (a, b)))
                  -> b
                  -> Producer m a
unfoldMC f =
    go
  where
    go seed = do
        mres <- lift $ f seed
        case mres of
            Just (a, seed') -> yield a >> go seed'
            Nothing -> return ()
STREAMING(unfoldM, unfoldMC, unfoldMS, f seed)

-- | Yield the values from the list.
--
-- Subject to fusion
sourceList, sourceListC :: Monad m => [a] -> Producer m a
sourceListC = Prelude.mapM_ yield
{-# INLINE sourceListC #-}
STREAMING(sourceList, sourceListC, sourceListS, xs)

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Subject to fusion
--
-- Since 0.4.2
enumFromTo, enumFromToC :: (Enum a, Prelude.Ord a, Monad m)
                        => a
                        -> a
                        -> Producer m a
enumFromToC x0 y =
    loop x0
  where
    loop x
        | x Prelude.> y = return ()
        | otherwise = yield x >> loop (Prelude.succ x)
{-# INLINE enumFromToC #-}
STREAMING(enumFromTo, enumFromToC, enumFromToS, x0 y)

-- | Produces an infinite stream of repeated applications of f to x.
--
-- Subject to fusion
--
iterate, iterateC :: Monad m => (a -> a) -> a -> Producer m a
iterateC f =
    go
  where
    go a = yield a >> go (f a)
{-# INLINE iterateC #-}
STREAMING(iterate, iterateC, iterateS, f a)

-- | Replicate a single value the given number of times.
--
-- Subject to fusion
--
-- Since 1.2.0
replicate, replicateC :: Monad m => Int -> a -> Producer m a
replicateC cnt0 a =
    loop cnt0
  where
    loop i
        | i <= 0 = return ()
        | otherwise = yield a >> loop (i - 1)
{-# INLINE replicateC #-}
STREAMING(replicate, replicateC, replicateS, cnt0 a)

-- | Replicate a monadic value the given number of times.
--
-- Subject to fusion
--
-- Since 1.2.0
replicateM, replicateMC :: Monad m => Int -> m a -> Producer m a
replicateMC cnt0 ma =
    loop cnt0
  where
    loop i
        | i <= 0 = return ()
        | otherwise = lift ma >>= yield >> loop (i - 1)
{-# INLINE replicateMC #-}
STREAMING(replicateM, replicateMC, replicateMS, cnt0 ma)

-- | A strict left fold.
--
-- Subject to fusion
--
-- Since 0.3.0
fold, foldC :: Monad m
            => (b -> a -> b)
            -> b
            -> Consumer a m b
foldC f =
    loop
  where
    loop !accum = await >>= maybe (return accum) (loop . f accum)
{-# INLINE foldC #-}
STREAMING(fold, foldC, foldS, f accum)

-- | A monadic strict left fold.
--
-- Subject to fusion
--
-- Since 0.3.0
foldM, foldMC :: Monad m
              => (b -> a -> m b)
              -> b
              -> Consumer a m b
foldMC f =
    loop
  where
    loop accum = do
        await >>= maybe (return accum) go
      where
        go a = do
            accum' <- lift $ f accum a
            accum' `seq` loop accum'
{-# INLINE foldMC #-}
STREAMING(foldM, foldMC, foldMS, f accum)

-----------------------------------------------------------------
-- These are for cases where- for whatever reason- stream fusion cannot be
-- applied.
connectFold :: Monad m => Source m a -> (b -> a -> b) -> b -> m b
connectFold (CI.ConduitM src0) f =
    go (src0 CI.Done)
  where
    go (CI.Done ()) b = return b
    go (CI.HaveOutput src _ a) b = go src Prelude.$! f b a
    go (CI.NeedInput _ c) b = go (c ()) b
    go (CI.Leftover src ()) b = go src b
    go (CI.PipeM msrc) b = do
        src <- msrc
        go src b
{-# INLINE connectFold #-}
{-# RULES "conduit: $$ fold" forall src f b. src $$ fold f b = connectFold src f b #-}

connectFoldM :: Monad m => Source m a -> (b -> a -> m b) -> b -> m b
connectFoldM (CI.ConduitM src0) f =
    go (src0 CI.Done)
  where
    go (CI.Done ()) b = return b
    go (CI.HaveOutput src _ a) b = do
        !b' <- f b a
        go src b'
    go (CI.NeedInput _ c) b = go (c ()) b
    go (CI.Leftover src ()) b = go src b
    go (CI.PipeM msrc) b = do
        src <- msrc
        go src b
{-# INLINE connectFoldM #-}
{-# RULES "conduit: $$ foldM" forall src f b. src $$ foldM f b = connectFoldM src f b #-}
-----------------------------------------------------------------

-- | A monoidal strict left fold.
--
-- Subject to fusion
--
-- Since 0.5.3
foldMap :: (Monad m, Monoid b)
        => (a -> b)
        -> Consumer a m b
INLINE_RULE(foldMap, f, let combiner accum = mappend accum . f in fold combiner mempty)

-- | A monoidal strict left fold in a Monad.
--
-- Since 1.0.8
foldMapM :: (Monad m, Monoid b)
        => (a -> m b)
        -> Consumer a m b
INLINE_RULE(foldMapM, f, let combiner accum = liftM (mappend accum) . f in foldM combiner mempty)

-- | Apply the action to all values in the stream.
--
-- Subject to fusion
--
-- Since 0.3.0
mapM_, mapM_C :: Monad m
              => (a -> m ())
              -> Consumer a m ()
mapM_C f = awaitForever $ lift . f
{-# INLINE mapM_C #-}
STREAMING(mapM_, mapM_C, mapM_S, f)

srcMapM_ :: Monad m => Source m a -> (a -> m ()) -> m ()
srcMapM_ (CI.ConduitM src) f =
    go (src CI.Done)
  where
    go (CI.Done ()) = return ()
    go (CI.PipeM mp) = mp >>= go
    go (CI.Leftover p ()) = go p
    go (CI.HaveOutput p _ o) = f o >> go p
    go (CI.NeedInput _ c) = go (c ())
{-# INLINE srcMapM_ #-}
{-# RULES "conduit: connect to mapM_" [2] forall f src. src $$ mapM_ f = srcMapM_ src f #-}

-- | Ignore a certain number of values in the stream. This function is
-- semantically equivalent to:
--
-- > drop i = take i >> return ()
--
-- However, @drop@ is more efficient as it does not need to hold values in
-- memory.
--
-- Subject to fusion
--
-- Since 0.3.0
drop, dropC :: Monad m
            => Int
            -> Consumer a m ()
dropC =
    loop
  where
    loop i | i <= 0 = return ()
    loop count = await >>= maybe (return ()) (\_ -> loop (count - 1))
{-# INLINE dropC #-}
STREAMING(drop, dropC, dropS, i)

-- | Take some values from the stream and return as a list. If you want to
-- instead create a conduit that pipes data to another sink, see 'isolate'.
-- This function is semantically equivalent to:
--
-- > take i = isolate i =$ consume
--
-- Subject to fusion
--
-- Since 0.3.0
take, takeC :: Monad m
            => Int
            -> Consumer a m [a]
takeC =
    loop id
  where
    loop front count | count <= 0 = return $ front []
    loop front count = await >>= maybe
        (return $ front [])
        (\x -> loop (front . (x:)) (count - 1))
{-# INLINE takeC #-}
STREAMING(take, takeC, takeS, i)

-- | Take a single value from the stream, if available.
--
-- Subject to fusion
--
-- Since 0.3.0
head, headC :: Monad m => Consumer a m (Maybe a)
headC = await
{-# INLINE headC #-}
STREAMING0(head, headC, headS)

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.3.0
peek :: Monad m => Consumer a m (Maybe a)
peek = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))

-- | Apply a transformation to all values in a stream.
--
-- Subject to fusion
--
-- Since 0.3.0
map, mapC :: Monad m => (a -> b) -> Conduit a m b
mapC f = awaitForever $ yield . f
{-# INLINE mapC #-}
STREAMING(map, mapC, mapS, f)

-- Since a Source never has any leftovers, fusion rules on it are safe.
{-
{-# RULES "conduit: source/map fusion =$=" forall f src. src =$= map f = mapFuseRight src f #-}

mapFuseRight :: Monad m => Source m a -> (a -> b) -> Source m b
mapFuseRight src f = CIC.mapOutput f src
{-# INLINE mapFuseRight #-}
-}

{-

It might be nice to include these rewrite rules, but they may have subtle
differences based on leftovers.

{-# RULES "conduit: map-to-mapOutput pipeL" forall f src. pipeL src (map f) = mapOutput f src #-}
{-# RULES "conduit: map-to-mapOutput $=" forall f src. src $= (map f) = mapOutput f src #-}
{-# RULES "conduit: map-to-mapOutput pipe" forall f src. pipe src (map f) = mapOutput f src #-}
{-# RULES "conduit: map-to-mapOutput >+>" forall f src. src >+> (map f) = mapOutput f src #-}

{-# RULES "conduit: map-to-mapInput pipeL" forall f sink. pipeL (map f) sink = mapInput f (Prelude.const Prelude.Nothing) sink #-}
{-# RULES "conduit: map-to-mapInput =$" forall f sink. map f =$ sink = mapInput f (Prelude.const Prelude.Nothing) sink #-}
{-# RULES "conduit: map-to-mapInput pipe" forall f sink. pipe (map f) sink = mapInput f (Prelude.const Prelude.Nothing) sink #-}
{-# RULES "conduit: map-to-mapInput >+>" forall f sink. map f >+> sink = mapInput f (Prelude.const Prelude.Nothing) sink #-}

{-# RULES "conduit: map-to-mapOutput =$=" forall f con. con =$= map f = mapOutput f con #-}
{-# RULES "conduit: map-to-mapInput =$=" forall f con. map f =$= con = mapInput f (Prelude.const Prelude.Nothing) con #-}

{-# INLINE [1] map #-}

-}

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Subject to fusion
--
-- Since 0.3.0
mapM, mapMC :: Monad m => (a -> m b) -> Conduit a m b
mapMC f = awaitForever $ \a -> lift (f a) >>= yield
{-# INLINE mapMC #-}
STREAMING(mapM, mapMC, mapMS, f)

-- | Apply a monadic action on all values in a stream.
--
-- This @Conduit@ can be used to perform a monadic side-effect for every
-- value, whilst passing the value through the @Conduit@ as-is.
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
--
-- Subject to fusion
--
-- Since 0.5.6
iterM, iterMC :: Monad m => (a -> m ()) -> Conduit a m a
iterMC f = awaitForever $ \a -> lift (f a) >> yield a
{-# INLINE iterMC #-}
STREAMING(iterM, iterMC, iterMS, f)

-- | Apply a transformation that may fail to all values in a stream, discarding
-- the failures.
--
-- Subject to fusion
--
-- Since 0.5.1
mapMaybe, mapMaybeC :: Monad m => (a -> Maybe b) -> Conduit a m b
mapMaybeC f = awaitForever $ maybe (return ()) yield . f
{-# INLINE mapMaybeC #-}
STREAMING(mapMaybe, mapMaybeC, mapMaybeS, f)

-- | Apply a monadic transformation that may fail to all values in a stream,
-- discarding the failures.
--
-- Subject to fusion
--
-- Since 0.5.1
mapMaybeM, mapMaybeMC :: Monad m => (a -> m (Maybe b)) -> Conduit a m b
mapMaybeMC f = awaitForever $ maybe (return ()) yield <=< lift . f
{-# INLINE mapMaybeMC #-}
STREAMING(mapMaybeM, mapMaybeMC, mapMaybeMS, f)

-- | Filter the @Just@ values from a stream, discarding the @Nothing@  values.
--
-- Subject to fusion
--
-- Since 0.5.1
catMaybes, catMaybesC :: Monad m => Conduit (Maybe a) m a
catMaybesC = awaitForever $ maybe (return ()) yield
{-# INLINE catMaybesC #-}
STREAMING0(catMaybes, catMaybesC, catMaybesS)

-- | Generalization of 'catMaybes'. It puts all values from
--   'F.Foldable' into stream.
--
-- Subject to fusion
--
-- Since 1.0.6
concat, concatC :: (Monad m, F.Foldable f) => Conduit (f a) m a
concatC = awaitForever $ F.mapM_ yield
{-# INLINE concatC #-}
STREAMING0(concat, concatC, concatS)

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMap, concatMapC :: Monad m => (a -> [b]) -> Conduit a m b
concatMapC f = awaitForever $ sourceList . f
{-# INLINE concatMapC #-}
STREAMING(concatMap, concatMapC, concatMapS, f)

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapM, concatMapMC :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapMC f = awaitForever $ sourceList <=< lift . f
{-# INLINE concatMapMC #-}
STREAMING(concatMapM, concatMapMC, concatMapMS, f)

-- | 'concatMap' with a strict accumulator.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapAccum, concatMapAccumC :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccumC f x0 = void (mapAccum f x0) =$= concat
{-# INLINE concatMapAccumC #-}
STREAMING(concatMapAccum, concatMapAccumC, concatMapAccumS, f x0)

-- | Deprecated synonym for @mapAccum@
--
-- Since 1.0.6
scanl :: Monad m => (a -> s -> (s, b)) -> s -> Conduit a m b
scanl f s = void $ mapAccum f s
{-# DEPRECATED scanl "Use mapAccum instead" #-}

-- | Deprecated synonym for @mapAccumM@
--
-- Since 1.0.6
scanlM :: Monad m => (a -> s -> m (s, b)) -> s -> Conduit a m b
scanlM f s = void $ mapAccumM f s
{-# DEPRECATED scanlM "Use mapAccumM instead" #-}

-- | Analog of @mapAccumL@ for lists. Note that in contrast to @mapAccumL@, the function argument
--   takes the accumulator as its second argument, not its first argument, and the accumulated value
--   is strict.
--
-- Subject to fusion
--
-- Since 1.1.1
mapAccum, mapAccumC :: Monad m => (a -> s -> (s, b)) -> s -> ConduitM a b m s
mapAccumC f =
    loop
  where
    loop !s = await >>= maybe (return s) go
      where
        go a = case f a s of
                 (s', b) -> yield b >> loop s'
STREAMING(mapAccum, mapAccumC, mapAccumS, f s)

-- | Monadic `mapAccum`.
--
-- Subject to fusion
--
-- Since 1.1.1
mapAccumM, mapAccumMC :: Monad m => (a -> s -> m (s, b)) -> s -> ConduitM a b m s
mapAccumMC f =
    loop
  where
    loop !s = await >>= maybe (return s) go
      where
        go a = do (s', b) <- lift $ f a s
                  yield b
                  loop s'
{-# INLINE mapAccumMC #-}
STREAMING(mapAccumM, mapAccumMC, mapAccumMS, f s)

-- | Analog of 'Prelude.scanl' for lists.
--
-- Subject to fusion
--
-- Since 1.1.1
scan :: Monad m => (a -> b -> b) -> b -> ConduitM a b m b
INLINE_RULE(scan, f, mapAccum (\a b -> let r = f a b in (r, r)))

-- | Monadic @scanl@.
--
-- Subject to fusion
--
-- Since 1.1.1
scanM :: Monad m => (a -> b -> m b) -> b -> ConduitM a b m b
INLINE_RULE(scanM, f, mapAccumM (\a b -> f a b >>= \r -> return (r, r)))

-- | 'concatMapM' with a strict accumulator.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapAccumM, concatMapAccumMC :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
concatMapAccumMC f x0 = void (mapAccumM f x0) =$= concat
{-# INLINE concatMapAccumMC #-}
STREAMING(concatMapAccumM, concatMapAccumMC, concatMapAccumMS, f x0)

-- | Generalization of 'mapMaybe' and 'concatMap'. It applies function
-- to all values in a stream and send values inside resulting
-- 'Foldable' downstream.
--
-- Subject to fusion
--
-- Since 1.0.6
mapFoldable, mapFoldableC :: (Monad m, F.Foldable f) => (a -> f b) -> Conduit a m b
mapFoldableC f = awaitForever $ F.mapM_ yield . f
{-# INLINE mapFoldableC #-}
STREAMING(mapFoldable, mapFoldableC, mapFoldableS, f)

-- | Monadic variant of 'mapFoldable'.
--
-- Subject to fusion
--
-- Since 1.0.6
mapFoldableM, mapFoldableMC :: (Monad m, F.Foldable f) => (a -> m (f b)) -> Conduit a m b
mapFoldableMC f = awaitForever $ F.mapM_ yield <=< lift . f
{-# INLINE mapFoldableMC #-}
STREAMING(mapFoldableM, mapFoldableMC, mapFoldableMS, f)

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
--
-- Subject to fusion
--
-- Since 0.3.0
consume, consumeC :: Monad m => Consumer a m [a]
consumeC =
    loop id
  where
    loop front = await >>= maybe (return $ front []) (\x -> loop $ front . (x:))
{-# INLINE consumeC #-}
STREAMING0(consume, consumeC, consumeS)

-- | Grouping input according to an equality function.
--
-- Subject to fusion
--
-- Since 0.3.0
groupBy, groupByC :: Monad m => (a -> a -> Bool) -> Conduit a m [a]
groupByC f =
    start
  where
    start = await >>= maybe (return ()) (loop id)

    loop rest x =
        await >>= maybe (yield (x : rest [])) go
      where
        go y
            | f x y     = loop (rest . (y:)) x
            | otherwise = yield (x : rest []) >> loop id y
STREAMING(groupBy, groupByC, groupByS, f)

-- | 'groupOn1' is similar to @groupBy id@
--
-- returns a pair, indicating there are always 1 or more items in the grouping.
-- This is designed to be converted into a NonEmpty structure
-- but it avoids a dependency on another package
--
-- > import Data.List.NonEmpty
-- >
-- > groupOn1 :: (Monad m, Eq b) => (a -> b) -> Conduit a m (NonEmpty a)
-- > groupOn1 f = CL.groupOn1 f =$= CL.map (uncurry (:|))
--
-- Subject to fusion
--
-- Since 1.1.7
groupOn1, groupOn1C :: (Monad m, Eq b)
                     => (a -> b)
                     -> Conduit a m (a, [a])
groupOn1C f =
    start
  where
    start = await >>= maybe (return ()) (loop id)

    loop rest x =
        await >>= maybe (yield (x, rest [])) go
      where
        go y
            | f x == f y = loop (rest . (y:)) x
            | otherwise  = yield (x, rest []) >> loop id y
STREAMING(groupOn1, groupOn1C, groupOn1S, f)

-- | Ensure that the inner sink consumes no more than the given number of
-- values. Note this this does /not/ ensure that the sink consumes all of those
-- values. To get the latter behavior, combine with 'sinkNull', e.g.:
--
-- > src $$ do
-- >     x <- isolate count =$ do
-- >         x <- someSink
-- >         sinkNull
-- >         return x
-- >     someOtherSink
-- >     ...
--
-- Subject to fusion
--
-- Since 0.3.0
isolate, isolateC :: Monad m => Int -> Conduit a m a
isolateC =
    loop
  where
    loop count | count <= 0 = return ()
    loop count = await >>= maybe (return ()) (\x -> yield x >> loop (count - 1))
STREAMING(isolate, isolateC, isolateS, count)

-- | Keep only values in the stream passing a given predicate.
--
-- Subject to fusion
--
-- Since 0.3.0
filter, filterC :: Monad m => (a -> Bool) -> Conduit a m a
filterC f = awaitForever $ \i -> when (f i) (yield i)
STREAMING(filter, filterC, filterS, f)

filterFuseRight :: Monad m => Source m a -> (a -> Bool) -> Source m a
filterFuseRight (CI.ConduitM src) f = CI.ConduitM $ \rest -> let
    go (CI.Done ()) = rest ()
    go (CI.PipeM mp) = CI.PipeM (liftM go mp)
    go (CI.Leftover p i) = CI.Leftover (go p) i
    go (CI.HaveOutput p c o)
        | f o = CI.HaveOutput (go p) c o
        | otherwise = go p
    go (CI.NeedInput p c) = CI.NeedInput (go . p) (go . c)
    in go (src CI.Done)
-- Intermediate finalizers are dropped, but this is acceptable: the next
-- yielded value would be demanded by downstream in any event, and that new
-- finalizer will always override the existing finalizer.
{-# RULES "conduit: source/filter fusion =$=" forall f src. src =$= filter f = filterFuseRight src f #-}
{-# INLINE filterFuseRight #-}

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
--
-- Subject to fusion
--
-- Since 0.3.0
sinkNull, sinkNullC :: Monad m => Consumer a m ()
sinkNullC = awaitForever $ \_ -> return ()
{-# INLINE sinkNullC #-}
STREAMING0(sinkNull, sinkNullC, sinkNullS)

srcSinkNull :: Monad m => Source m a -> m ()
srcSinkNull (CI.ConduitM src) =
    go (src CI.Done)
  where
    go (CI.Done ()) = return ()
    go (CI.PipeM mp) = mp >>= go
    go (CI.Leftover p ()) = go p
    go (CI.HaveOutput p _ _) = go p
    go (CI.NeedInput _ c) = go (c ())
{-# INLINE srcSinkNull #-}
{-# RULES "conduit: connect to sinkNull" forall src. src $$ sinkNull = srcSinkNull src #-}

-- | A source that outputs no values. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Subject to fusion
--
-- Since 0.3.0
sourceNull, sourceNullC :: Monad m => Producer m a
sourceNullC = return ()
{-# INLINE sourceNullC #-}
STREAMING0(sourceNull, sourceNullC, sourceNullS)

-- | Run a @Pipe@ repeatedly, and output its result value downstream. Stops
-- when no more input is available from upstream.
--
-- Since 0.5.0
sequence :: Monad m
         => Consumer i m o -- ^ @Pipe@ to run repeatedly
         -> Conduit i m o
sequence sink =
    self
  where
    self = awaitForever $ \i -> leftover i >> sink >>= yield
