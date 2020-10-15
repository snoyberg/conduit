{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
-- | /NOTE/ It is recommended to start using "Data.Conduit.Combinators" instead
-- of this module.
--
-- Higher-level functions to interact with the elements of a stream. Most of
-- these are based on list functions.
--
-- For many purposes, it's recommended to use the conduit-combinators library,
-- which provides a more complete set of functions.
--
-- Note that these functions all deal with individual elements of a stream as a
-- sort of \"black box\", where there is no introspection of the contained
-- elements. Values such as @ByteString@ and @Text@ will likely need to be
-- treated specially to deal with their contents properly (@Word8@ and @Char@,
-- respectively). See the @Data.Conduit.Binary@ and @Data.Conduit.Text@
-- modules in the @conduit-extra@ package.
module Data.Conduit.List
    ( -- * Sources
      sourceList
    , sourceNull
    , unfold
    , unfoldEither
    , unfoldM
    , unfoldEitherM
    , enumFromTo
    , iterate
    , replicate
    , replicateM
      -- * Sinks
      -- ** Pure
    , fold
    , foldMap
    , uncons
    , unconsEither
    , take
    , drop
    , head
    , peek
    , consume
    , sinkNull
      -- ** Monadic
    , foldMapM
    , foldM
    , unconsM
    , unconsEitherM
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
    , chunksOf
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
    , Either (..)
    , Bool (..)
    , (>>)
    , (>>=)
    , seq
    , otherwise
    , Enum, Eq
    , maybe
    , (<=)
    , (>)
    , error
    , (++)
    , show
    )
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Foldable as F
import Data.Conduit
import Data.Conduit.Internal.Conduit (unconsM, unconsEitherM)
import Data.Conduit.Internal.Fusion
import Data.Conduit.Internal.List.Stream
import qualified Data.Conduit.Internal as CI
import Data.Functor.Identity (Identity (runIdentity))
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
                -> ConduitT i a m ()
unfoldC f =
    go
  where
    go seed =
        case f seed of
            Just (a, seed') -> yield a >> go seed'
            Nothing -> return ()
{-# INLINE unfoldC #-}
STREAMING(unfold, unfoldC, unfoldS, f x)

-- | Generate a source from a seed value with a return value.
--
-- Subject to fusion
--
-- @since 1.2.11
unfoldEither, unfoldEitherC :: Monad m
                            => (b -> Either r (a, b))
                            -> b
                            -> ConduitT i a m r
unfoldEitherC f =
    go
  where
    go seed =
        case f seed of
            Right (a, seed') -> yield a >> go seed'
            Left r -> return r
{-# INLINE unfoldEitherC #-}
STREAMING(unfoldEither, unfoldEitherC, unfoldEitherS, f x)

-- | A monadic unfold.
--
-- Subject to fusion
--
-- Since 1.1.2
unfoldM, unfoldMC :: Monad m
                  => (b -> m (Maybe (a, b)))
                  -> b
                  -> ConduitT i a m ()
unfoldMC f =
    go
  where
    go seed = do
        mres <- lift $ f seed
        case mres of
            Just (a, seed') -> yield a >> go seed'
            Nothing -> return ()
STREAMING(unfoldM, unfoldMC, unfoldMS, f seed)

-- | A monadic unfoldEither.
--
-- Subject to fusion
--
-- @since 1.2.11
unfoldEitherM, unfoldEitherMC :: Monad m
                              => (b -> m (Either r (a, b)))
                              -> b
                              -> ConduitT i a m r
unfoldEitherMC f =
    go
  where
    go seed = do
        mres <- lift $ f seed
        case mres of
            Right (a, seed') -> yield a >> go seed'
            Left r -> return r
STREAMING(unfoldEitherM, unfoldEitherMC, unfoldEitherMS, f seed)

-- | Split a pure conduit into head and tail.
-- This is equivalent to @runIdentity . unconsM@.
--
-- Note that you have to 'sealConduitT' it first.
--
-- Since 1.3.3
uncons :: SealedConduitT () o Identity ()
       -> Maybe (o, SealedConduitT () o Identity ())
uncons = runIdentity . unconsM

-- | Split a pure conduit into head and tail or return its result if it is done.
-- This is equivalent to @runIdentity . unconsEitherM@.
--
-- Note that you have to 'sealConduitT' it first.
--
-- Since 1.3.3
unconsEither :: SealedConduitT () o Identity r
             -> Either r (o, SealedConduitT () o Identity r)
unconsEither = runIdentity . unconsEitherM

-- | Yield the values from the list.
--
-- Subject to fusion
sourceList, sourceListC :: Monad m => [a] -> ConduitT i a m ()
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
                        -> ConduitT i a m ()
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
iterate, iterateC :: Monad m => (a -> a) -> a -> ConduitT i a m ()
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
replicate, replicateC :: Monad m => Int -> a -> ConduitT i a m ()
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
replicateM, replicateMC :: Monad m => Int -> m a -> ConduitT i a m ()
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
            -> ConduitT a o m b
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
              -> ConduitT a o m b
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
connectFold :: Monad m => ConduitT () a m () -> (b -> a -> b) -> b -> m b
connectFold (CI.ConduitT src0) f =
    go (src0 CI.Done)
  where
    go (CI.Done ()) b = return b
    go (CI.HaveOutput src a) b = go src Prelude.$! f b a
    go (CI.NeedInput _ c) b = go (c ()) b
    go (CI.Leftover src ()) b = go src b
    go (CI.PipeM msrc) b = do
        src <- msrc
        go src b
{-# INLINE connectFold #-}
{-# RULES "conduit: $$ fold" forall src f b. runConduit (src .| fold f b) = connectFold src f b #-}

connectFoldM :: Monad m => ConduitT () a m () -> (b -> a -> m b) -> b -> m b
connectFoldM (CI.ConduitT src0) f =
    go (src0 CI.Done)
  where
    go (CI.Done ()) b = return b
    go (CI.HaveOutput src a) b = do
        !b' <- f b a
        go src b'
    go (CI.NeedInput _ c) b = go (c ()) b
    go (CI.Leftover src ()) b = go src b
    go (CI.PipeM msrc) b = do
        src <- msrc
        go src b
{-# INLINE connectFoldM #-}
{-# RULES "conduit: $$ foldM" forall src f b. runConduit (src .| foldM f b) = connectFoldM src f b #-}
-----------------------------------------------------------------

-- | A monoidal strict left fold.
--
-- Subject to fusion
--
-- Since 0.5.3
foldMap :: (Monad m, Monoid b)
        => (a -> b)
        -> ConduitT a o m b
INLINE_RULE(foldMap, f, let combiner accum = mappend accum . f in fold combiner mempty)

-- | A monoidal strict left fold in a Monad.
--
-- Since 1.0.8
foldMapM :: (Monad m, Monoid b)
        => (a -> m b)
        -> ConduitT a o m b
INLINE_RULE(foldMapM, f, let combiner accum = liftM (mappend accum) . f in foldM combiner mempty)

-- | Apply the action to all values in the stream.
--
-- Subject to fusion
--
-- Since 0.3.0
mapM_, mapM_C :: Monad m
              => (a -> m ())
              -> ConduitT a o m ()
mapM_C f = awaitForever $ lift . f
{-# INLINE mapM_C #-}
STREAMING(mapM_, mapM_C, mapM_S, f)

srcMapM_ :: Monad m => ConduitT () a m () -> (a -> m ()) -> m ()
srcMapM_ (CI.ConduitT src) f =
    go (src CI.Done)
  where
    go (CI.Done ()) = return ()
    go (CI.PipeM mp) = mp >>= go
    go (CI.Leftover p ()) = go p
    go (CI.HaveOutput p o) = f o >> go p
    go (CI.NeedInput _ c) = go (c ())
{-# INLINE srcMapM_ #-}
{-# RULES "conduit: connect to mapM_" [2] forall f src. runConduit (src .| mapM_ f) = srcMapM_ src f #-}

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
            -> ConduitT a o m ()
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
            -> ConduitT a o m [a]
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
head, headC :: Monad m => ConduitT a o m (Maybe a)
headC = await
{-# INLINE headC #-}
STREAMING0(head, headC, headS)

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.3.0
peek :: Monad m => ConduitT a o m (Maybe a)
peek = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))

-- | Apply a transformation to all values in a stream.
--
-- Subject to fusion
--
-- Since 0.3.0
map, mapC :: Monad m => (a -> b) -> ConduitT a b m ()
mapC f = awaitForever $ yield . f
{-# INLINE mapC #-}
STREAMING(map, mapC, mapS, f)

-- Since a Source never has any leftovers, fusion rules on it are safe.
{-
{-# RULES "conduit: source/map fusion .|" forall f src. src .| map f = mapFuseRight src f #-}

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

{-# RULES "conduit: map-to-mapOutput .|" forall f con. con .| map f = mapOutput f con #-}
{-# RULES "conduit: map-to-mapInput .|" forall f con. map f .| con = mapInput f (Prelude.const Prelude.Nothing) con #-}

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
mapM, mapMC :: Monad m => (a -> m b) -> ConduitT a b m ()
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
iterM, iterMC :: Monad m => (a -> m ()) -> ConduitT a a m ()
iterMC f = awaitForever $ \a -> lift (f a) >> yield a
{-# INLINE iterMC #-}
STREAMING(iterM, iterMC, iterMS, f)

-- | Apply a transformation that may fail to all values in a stream, discarding
-- the failures.
--
-- Subject to fusion
--
-- Since 0.5.1
mapMaybe, mapMaybeC :: Monad m => (a -> Maybe b) -> ConduitT a b m ()
mapMaybeC f = awaitForever $ maybe (return ()) yield . f
{-# INLINE mapMaybeC #-}
STREAMING(mapMaybe, mapMaybeC, mapMaybeS, f)

-- | Apply a monadic transformation that may fail to all values in a stream,
-- discarding the failures.
--
-- Subject to fusion
--
-- Since 0.5.1
mapMaybeM, mapMaybeMC :: Monad m => (a -> m (Maybe b)) -> ConduitT a b m ()
mapMaybeMC f = awaitForever $ maybe (return ()) yield <=< lift . f
{-# INLINE mapMaybeMC #-}
STREAMING(mapMaybeM, mapMaybeMC, mapMaybeMS, f)

-- | Filter the @Just@ values from a stream, discarding the @Nothing@  values.
--
-- Subject to fusion
--
-- Since 0.5.1
catMaybes, catMaybesC :: Monad m => ConduitT (Maybe a) a m ()
catMaybesC = awaitForever $ maybe (return ()) yield
{-# INLINE catMaybesC #-}
STREAMING0(catMaybes, catMaybesC, catMaybesS)

-- | Generalization of 'catMaybes'. It puts all values from
--   'F.Foldable' into stream.
--
-- Subject to fusion
--
-- Since 1.0.6
concat, concatC :: (Monad m, F.Foldable f) => ConduitT (f a) a m ()
concatC = awaitForever $ F.mapM_ yield
{-# INLINE concatC #-}
STREAMING0(concat, concatC, concatS)

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMap, concatMapC :: Monad m => (a -> [b]) -> ConduitT a b m ()
concatMapC f = awaitForever $ sourceList . f
{-# INLINE concatMapC #-}
STREAMING(concatMap, concatMapC, concatMapS, f)

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapM, concatMapMC :: Monad m => (a -> m [b]) -> ConduitT a b m ()
concatMapMC f = awaitForever $ sourceList <=< lift . f
{-# INLINE concatMapMC #-}
STREAMING(concatMapM, concatMapMC, concatMapMS, f)

-- | 'concatMap' with a strict accumulator.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapAccum, concatMapAccumC :: Monad m => (a -> accum -> (accum, [b])) -> accum -> ConduitT a b m ()
concatMapAccumC f x0 = void (mapAccum f x0) .| concat
{-# INLINE concatMapAccumC #-}
STREAMING(concatMapAccum, concatMapAccumC, concatMapAccumS, f x0)

-- | Deprecated synonym for @mapAccum@
--
-- Since 1.0.6
scanl :: Monad m => (a -> s -> (s, b)) -> s -> ConduitT a b m ()
scanl f s = void $ mapAccum f s
{-# DEPRECATED scanl "Use mapAccum instead" #-}

-- | Deprecated synonym for @mapAccumM@
--
-- Since 1.0.6
scanlM :: Monad m => (a -> s -> m (s, b)) -> s -> ConduitT a b m ()
scanlM f s = void $ mapAccumM f s
{-# DEPRECATED scanlM "Use mapAccumM instead" #-}

-- | Analog of @mapAccumL@ for lists. Note that in contrast to @mapAccumL@, the function argument
--   takes the accumulator as its second argument, not its first argument, and the accumulated value
--   is strict.
--
-- Subject to fusion
--
-- Since 1.1.1
mapAccum, mapAccumC :: Monad m => (a -> s -> (s, b)) -> s -> ConduitT a b m s
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
mapAccumM, mapAccumMC :: Monad m => (a -> s -> m (s, b)) -> s -> ConduitT a b m s
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
scan :: Monad m => (a -> b -> b) -> b -> ConduitT a b m b
INLINE_RULE(scan, f, mapAccum (\a b -> let r = f a b in (r, r)))

-- | Monadic @scanl@.
--
-- Subject to fusion
--
-- Since 1.1.1
scanM :: Monad m => (a -> b -> m b) -> b -> ConduitT a b m b
INLINE_RULE(scanM, f, mapAccumM (\a b -> f a b >>= \r -> return (r, r)))

-- | 'concatMapM' with a strict accumulator.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapAccumM, concatMapAccumMC :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> ConduitT a b m ()
concatMapAccumMC f x0 = void (mapAccumM f x0) .| concat
{-# INLINE concatMapAccumMC #-}
STREAMING(concatMapAccumM, concatMapAccumMC, concatMapAccumMS, f x0)

-- | Generalization of 'mapMaybe' and 'concatMap'. It applies function
-- to all values in a stream and send values inside resulting
-- 'Foldable' downstream.
--
-- Subject to fusion
--
-- Since 1.0.6
mapFoldable, mapFoldableC :: (Monad m, F.Foldable f) => (a -> f b) -> ConduitT a b m ()
mapFoldableC f = awaitForever $ F.mapM_ yield . f
{-# INLINE mapFoldableC #-}
STREAMING(mapFoldable, mapFoldableC, mapFoldableS, f)

-- | Monadic variant of 'mapFoldable'.
--
-- Subject to fusion
--
-- Since 1.0.6
mapFoldableM, mapFoldableMC :: (Monad m, F.Foldable f) => (a -> m (f b)) -> ConduitT a b m ()
mapFoldableMC f = awaitForever $ F.mapM_ yield <=< lift . f
{-# INLINE mapFoldableMC #-}
STREAMING(mapFoldableM, mapFoldableMC, mapFoldableMS, f)

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory.
--
-- Subject to fusion
--
-- Since 0.3.0
consume, consumeC :: Monad m => ConduitT a o m [a]
consumeC =
    loop id
  where
    loop front = await >>= maybe (return $ front []) (\x -> loop $ front . (x:))
{-# INLINE consumeC #-}
STREAMING0(consume, consumeC, consumeS)

-- | Group a stream into chunks of a given size. The last chunk may contain
-- fewer than n elements.
--
-- Subject to fusion
--
-- Since 1.2.9
chunksOf :: Monad m => Int -> ConduitT a [a] m ()
chunksOf n = if n > 0 then loop n id else error $ "chunksOf size must be positive (given " ++ show n ++ ")"
  where
    loop 0 rest = yield (rest []) >> loop n id
    loop count rest = await >>= \ma -> case ma of
      Nothing -> case rest [] of
        [] -> return ()
        nonempty -> yield nonempty
      Just a -> loop (count - 1) (rest . (a :))

-- | Grouping input according to an equality function.
--
-- Subject to fusion
--
-- Since 0.3.0
groupBy, groupByC :: Monad m => (a -> a -> Bool) -> ConduitT a [a] m ()
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
-- > groupOn1 f = CL.groupOn1 f .| CL.map (uncurry (:|))
--
-- Subject to fusion
--
-- Since 1.1.7
groupOn1, groupOn1C :: (Monad m, Eq b)
                     => (a -> b)
                     -> ConduitT a (a, [a]) m ()
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
isolate, isolateC :: Monad m => Int -> ConduitT a a m ()
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
filter, filterC :: Monad m => (a -> Bool) -> ConduitT a a m ()
filterC f = awaitForever $ \i -> when (f i) (yield i)
STREAMING(filter, filterC, filterS, f)

filterFuseRight
  :: Monad m
  => ConduitT i o m ()
  -> (o -> Bool)
  -> ConduitT i o m ()
filterFuseRight (CI.ConduitT src) f = CI.ConduitT $ \rest -> let
    go (CI.Done ()) = rest ()
    go (CI.PipeM mp) = CI.PipeM (liftM go mp)
    go (CI.Leftover p i) = CI.Leftover (go p) i
    go (CI.HaveOutput p o)
        | f o = CI.HaveOutput (go p) o
        | otherwise = go p
    go (CI.NeedInput p c) = CI.NeedInput (go . p) (go . c)
    in go (src CI.Done)
-- Intermediate finalizers are dropped, but this is acceptable: the next
-- yielded value would be demanded by downstream in any event, and that new
-- finalizer will always override the existing finalizer.
{-# RULES "conduit: source/filter fusion .|" forall f src. src .| filter f = filterFuseRight src f #-}
{-# INLINE filterFuseRight #-}

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
--
-- Subject to fusion
--
-- Since 0.3.0
sinkNull, sinkNullC :: Monad m => ConduitT i o m ()
sinkNullC = awaitForever $ \_ -> return ()
{-# INLINE sinkNullC #-}
STREAMING0(sinkNull, sinkNullC, sinkNullS)

srcSinkNull :: Monad m => ConduitT () o m () -> m ()
srcSinkNull (CI.ConduitT src) =
    go (src CI.Done)
  where
    go (CI.Done ()) = return ()
    go (CI.PipeM mp) = mp >>= go
    go (CI.Leftover p ()) = go p
    go (CI.HaveOutput p _) = go p
    go (CI.NeedInput _ c) = go (c ())
{-# INLINE srcSinkNull #-}
{-# RULES "conduit: connect to sinkNull" forall src. runConduit (src .| sinkNull) = srcSinkNull src #-}

-- | A source that outputs no values. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Subject to fusion
--
-- Since 0.3.0
sourceNull, sourceNullC :: Monad m => ConduitT i o m ()
sourceNullC = return ()
{-# INLINE sourceNullC #-}
STREAMING0(sourceNull, sourceNullC, sourceNullS)

-- | Run a @Pipe@ repeatedly, and output its result value downstream. Stops
-- when no more input is available from upstream.
--
-- Since 0.5.0
sequence :: Monad m
         => ConduitT i o m o -- ^ @Pipe@ to run repeatedly
         -> ConduitT i o m ()
sequence sink =
    self
  where
    self = awaitForever $ \i -> leftover i >> sink >>= yield
