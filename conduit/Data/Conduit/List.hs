{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Higher-level functions to interact with the elements of a stream. Most of
-- these are based on list functions.
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
#ifdef QUICKCHECK
    , props
#endif
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
import qualified Data.Conduit.Internal as CI
import Data.Conduit.Internal.Fusion
import Control.Monad (when, (<=<), liftM, void)
import Control.Monad.Trans.Class (lift)

#if QUICKCHECK
import Test.Hspec
import Test.QuickCheck
import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.List
import qualified Data.Foldable
import qualified Safe
import qualified Data.Maybe
#endif

-- TODO:
--
-- * Should these new streamified producers should follow the pattern
-- of enumFromTo?  Why does the RULE use streamSourcePure whereas the
-- inline definition use streamSource?  Does it intentionally use ($)?

-- * Is there a better way to do the rule for foldMap? The RULES is
-- just a direct inliner.
--
-- * Are INLINE [2] pragmas intended instead of INLINE [0] ? Phase 2
-- happens earlier, I think.
--
-- * Revisit the old RULES - maybe these should fire in a later stage
-- than fusion rules?
--
-- * Is the stream version of "drop" even useful?


-- | Generate a source from a seed value.
--
-- Subject to fusion
--
-- Since 0.4.2
unfold :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> Producer m a
unfold = unfoldC
{-# INLINE [2] unfold #-}
{-# RULES "conduit: unstream unfold" forall x y.
    unfold x y = unstream (streamConduit (unfoldC x y) (\_ -> unfoldS x y))
  #-}

unfoldC :: Monad m
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

unfoldS :: Monad m
        => (b -> Maybe (a, b))
        -> b
        -> Stream m a ()
unfoldS f s0 =
    Stream step (return s0)
  where
    step s = return $
        case f s of
            Nothing -> Stop ()
            Just (x, s') -> Emit s' x
{-# INLINE unfoldS #-}

-- | A monadic unfold.
--
-- Subject to fusion
--
-- Since 1.1.2
unfoldM :: Monad m
        => (b -> m (Maybe (a, b)))
        -> b
        -> Producer m a
unfoldM = unfoldMC
{-# INLINE [2] unfoldM #-}
{-# RULES "conduit: unstream unfoldM" forall x y.
    unfoldM x y = unstream (streamConduit (unfoldMC x y) (\_ -> unfoldMS x y))
  #-}

unfoldMC :: Monad m
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

unfoldMS :: Monad m
         => (b -> m (Maybe (a, b)))
         -> b
         -> Stream m a ()
unfoldMS f s0 =
    Stream step (return s0)
  where
    step s = do
        ms' <- f s
        return $ case ms' of
            Nothing -> Stop ()
            Just (x, s') -> Emit s' x
{-# INLINE unfoldMS #-}

-- | Yield the values from the list.
--
-- Subject to fusion
sourceList :: Monad m => [a] -> Producer m a
sourceList = sourceListC
{-# INLINE [0] sourceList #-}
{-# RULES "conduit: unstream sourceList" forall x.
    sourceList x = unstream (streamConduit (sourceListC x) (\_ -> sourceListS x))
  #-}

sourceListC :: Monad m => [a] -> Producer m a
sourceListC = Prelude.mapM_ yield
{-# INLINE sourceListC #-}

sourceListS :: Monad m => [a] -> Stream m a ()
sourceListS xs =
    Stream (return . step) (return xs)
  where
    step [] = Stop ()
    step (x:xs) = Emit xs x
{-# INLINE sourceListS #-}

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Subject to fusion
--
-- Since 0.4.2
enumFromTo :: (Enum a, Prelude.Ord a, Monad m)
           => a
           -> a
           -> Producer m a
--TODO: why isn't this enumFromToC?
enumFromTo x y = unstream $ streamSource $ enumFromToS x y
{-# INLINE [0] enumFromTo #-}
{-# RULES "conduit: unstream enumFromTo" forall x y.
    enumFromTo x y = unstream (streamSourcePure $ enumFromToS x y)
  #-}

enumFromToC :: (Enum a, Prelude.Ord a, Monad m)
            => a
            -> a
            -> Producer m a
enumFromToC x0 y =
    loop x0
  where
    loop x
        | x Prelude.> y = return ()
        | otherwise = yield x >> loop (Prelude.succ x)
{-# INLINE [0] enumFromToC #-}

enumFromToS :: (Enum a, Prelude.Ord a, Monad m)
            => a
            -> a
            -> Stream m a ()
enumFromToS x0 y =
    Stream step (return x0)
  where
    step x = return $ if x Prelude.> y
        then Stop ()
        else Emit (Prelude.succ x) x
{-# INLINE [0] enumFromToS #-}

enumFromToS_int :: (Prelude.Integral a, Monad m) => a -> a -> Stream m a ()
enumFromToS_int x0 y = x0 `seq` y `seq` Stream step (return x0)
  where
    step x | x <= y    = return $ Emit (x Prelude.+ 1) x
           | otherwise = return $ Stop ()
{-# INLINE enumFromToS_int #-}

{-# RULES "conduit: enumFromTo<Int>"
      enumFromToS = enumFromToS_int :: Monad m => Int -> Int -> Stream m Int ()
  #-}

-- | Produces an infinite stream of repeated applications of f to x.
--
-- Subject to fusion
--
iterate :: Monad m => (a -> a) -> a -> Producer m a
iterate = iterateC
{-# INLINE [0] iterate #-}
{-# RULES "conduit: unstream iterate" forall x y.
    iterate x y = unstream (streamConduit (iterateC x y) (\_ -> iterateS x y))
  #-}

iterateC :: Monad m => (a -> a) -> a -> Producer m a
iterateC f =
    go
  where
    go a = yield a >> go (f a)
{-# INLINE iterateC #-}

iterateS :: Monad m => (a -> a) -> a -> Stream m a ()
iterateS f x0 =
    Stream (return . step) (return x0)
  where
    step x = Emit x' x
      where
        x' = f x
{-# INLINE iterateS #-}

-- | Replicate a single value the given number of times.
--
-- Subject to fusion
--
-- Since 1.2.0
replicate :: Monad m => Int -> a -> Producer m a
replicate = replicateC
{-# INLINE [0] replicate #-}
{-# RULES "conduit: unstream replicate" forall i a.
     replicate i a = unstream (streamConduit (replicateC i a) (\_ -> replicateS i a))
  #-}

replicateC :: Monad m => Int -> a -> Producer m a
replicateC cnt0 a =
    loop cnt0
  where
    loop i
        | i <= 0 = return ()
        | otherwise = yield a >> loop (i - 1)
{-# INLINE replicateC #-}

replicateS :: Monad m => Int -> a -> Stream m a ()
replicateS cnt0 a =
    Stream step (return cnt0)
  where
    step cnt
        | cnt <= 0  = return $ Stop ()
        | otherwise = return $ Emit (cnt - 1) a
{-# INLINE replicateS #-}

-- | Replicate a monadic value the given number of times.
--
-- Subject to fusion
--
-- Since 1.2.0
replicateM :: Monad m => Int -> m a -> Producer m a
replicateM = replicateMC
{-# INLINE [0] replicateM #-}
{-# RULES "conduit: unstream replicateM" forall i a.
     replicateM i a = unstream (streamConduit (replicateMC i a) (\_ -> replicateMS i a))
  #-}

replicateMC :: Monad m => Int -> m a -> Producer m a
replicateMC cnt0 ma =
    loop cnt0
  where
    loop i
        | i <= 0 = return ()
        | otherwise = lift ma >>= yield >> loop (i - 1)
{-# INLINE replicateMC #-}

replicateMS :: Monad m => Int -> m a -> Stream m a ()
replicateMS cnt0 ma =
    Stream step (return cnt0)
  where
    step cnt
        | cnt <= 0  = return $ Stop ()
        | otherwise = Emit (cnt - 1) `liftM` ma
{-# INLINE replicateMS #-}

-- | A strict left fold.
--
-- Subject to fusion
--
-- Since 0.3.0
fold :: Monad m
     => (b -> a -> b)
     -> b
     -> Consumer a m b
fold = foldC
{-# INLINE [0] fold #-}
{-# RULES "conduit: unstream fold" forall f b.
        fold f b = unstream (streamConduit (foldC f b) (foldS f b))
  #-}

foldC :: Monad m
      => (b -> a -> b)
      -> b
      -> Consumer a m b
foldC f =
    loop
  where
    loop !accum = await >>= maybe (return accum) (loop . f accum)
{-# INLINE foldC #-}

foldS :: Monad m => (b -> a -> b) -> b -> Stream m a () -> Stream m o b
foldS f b0 (Stream step ms0) =
    Stream step' (liftM (b0, ) ms0)
  where
    step' (!b, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop b
            Skip s' -> Skip (b, s')
            Emit s' a -> Skip (f b a, s')
{-# INLINE foldS #-}

-- | A monadic strict left fold.
--
-- Subject to fusion
--
-- Since 0.3.0
foldM :: Monad m
      => (b -> a -> m b)
      -> b
      -> Consumer a m b
foldM = foldMC
{-# INLINE [0] foldM #-}
{-# RULES "conduit: unstream foldM" forall f b.
        foldM f b = unstream (streamConduit (foldMC f b) (foldMS f b))
  #-}

foldMC :: Monad m
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

foldMS :: Monad m => (b -> a -> m b) -> b -> Stream m a () -> Stream m o b
foldMS f b0 (Stream step ms0) =
    Stream step' (liftM (b0, ) ms0)
  where
    step' (!b, s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop b
            Skip s' -> return $ Skip (b, s')
            Emit s' a -> do
                b' <- f b a
                return $ Skip (b', s')
{-# INLINE foldMS #-}

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
foldMap f =
    let combiner accum = mappend accum . f
    in fold combiner mempty
{-# INLINE [2] foldMap #-}
{-# RULES "conduit: inline foldMap" forall f.
    foldMap f =
        let combiner accum = mappend accum . f
        in fold combiner mempty
  #-}

-- | A monoidal strict left fold in a Monad.
--
-- Since 1.0.8
foldMapM :: (Monad m, Monoid b)
        => (a -> m b)
        -> Consumer a m b
foldMapM f =
    let combiner accum = liftM (mappend accum) . f
    in foldM combiner mempty
{-# INLINE [2] foldMapM #-}
{-# RULES "conduit: inline foldMapM" forall f.
    foldMapM f =
        let combiner accum = liftM (mappend accum) . f
        in foldM combiner mempty
  #-}

-- | Apply the action to all values in the stream.
--
-- Subject to fusion
--
-- Since 0.3.0
mapM_ :: Monad m
      => (a -> m ())
      -> Consumer a m ()
mapM_ f = awaitForever $ lift . f
{-# INLINE [1] mapM_ #-}
{-# RULES "conduit: unstream mapM_" forall x.
    mapM_ x = unstream (streamConduit (mapM_C x) (mapM_S x))
  #-}

mapM_C :: Monad m
       => (a -> m ())
       -> Consumer a m ()
mapM_C f = awaitForever $ lift . f
{-# INLINE [1] mapM_C #-}

mapM_S :: Monad m
       => (a -> m ())
       -> (Stream m a () -> Stream m () ())
mapM_S f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        case res of
          Stop () -> return $ Stop ()
          Skip s' -> return $ Skip s'
          Emit s' x -> liftM (Emit s') (f x)
{-# INLINE [1] mapM_S #-}

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
drop :: Monad m
     => Int
     -> Consumer a m ()
drop = dropC
{-# INLINE [2] drop #-}
{-# RULES "conduit: unstream drop" forall n.
    drop n = unstream (streamConduit (dropC n) (dropS n))
  #-}

dropC :: Monad m
      => Int
      -> Consumer a m ()
dropC =
    loop
  where
    loop i | i <= 0 = return ()
    loop count = await >>= maybe (return ()) (\_ -> loop (count - 1))
{-# INLINE dropC #-}

dropS :: Monad m
      => Int
      -> (Stream m a () -> Stream m () ())
dropS n0 (Stream step ms0) =
    Stream step' (liftM (, n0) ms0)
  where
    step' (s, n) | n <= 0 = return $ Stop ()
    step' (s, n) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (s', n)
            Emit s' _ -> Skip (s', n - 1)
{-# INLINE dropS #-}

-- | Take some values from the stream and return as a list. If you want to
-- instead create a conduit that pipes data to another sink, see 'isolate'.
-- This function is semantically equivalent to:
--
-- > take i = isolate i =$ consume
--
-- Subject to fusion
--
-- Since 0.3.0
take :: Monad m
     => Int
     -> Consumer a m [a]
take = takeC
{-# INLINE [2] take #-}
{-# RULES "conduit: unstream take" forall n.
    take n = unstream (streamConduit (takeC n) (takeS n))
  #-}

takeC :: Monad m
      => Int
      -> Consumer a m [a]
takeC =
    loop id
  where
    loop front count | count <= 0 = return $ front []
    loop front count = await >>= maybe
        (return $ front [])
        (\x -> loop (front . (x:)) (count - 1))

takeS :: Monad m
      => Int
      -> (Stream m a () -> Stream m () [a])
takeS n (Stream step s0) =
    Stream step' (liftM (id, n,) s0)
  where
    step' (output, n, s) | n <= 0 = return $ Stop (output [])
    step' (output, n, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop (output [])
            Skip s' -> Skip (output, n, s')
            Emit s' x -> Skip (output . (x:), n - 1, s')
{-# INLINE takeS #-}

-- | Take a single value from the stream, if available.
--
-- Subject to fusion
--
-- Since 0.3.0
head :: Monad m => Consumer a m (Maybe a)
head = headC
{-# INLINE [2] head #-}
{-# RULES "conduit: unstream head"
    head = unstream (streamConduit headC headS)

  #-}

headC :: Monad m => Consumer a m (Maybe a)
headC = await
{-# INLINE headC #-}

headS :: Monad m => (Stream m a () -> Stream m () (Maybe a))
headS (Stream step s0) =
    Stream step' s0
  where
    step' s = do
        res <- step s
        return $ case res of
            Stop () -> Stop Nothing
            Skip s' -> Skip s'
            Emit _ x -> Stop (Just x)
{-# INLINE headS #-}

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
map :: Monad m => (a -> b) -> Conduit a m b
map = mapC
{-# INLINE [0] map #-}
{-# RULES "conduit: unstream map"
    map = \f -> unstream (streamConduit (mapC f) (mapS f))
  #-}

mapC :: Monad m => (a -> b) -> Conduit a m b
mapC f = awaitForever $ yield . f
{-# INLINE mapC #-}

mapS :: Monad m => (a -> b) -> Stream m a r -> Stream m b r
mapS f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        return $ case res of
            Stop r -> Stop r
            Emit s' a -> Emit s' (f a)
            Skip s' -> Skip s'
{-# INLINE mapS #-}

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
mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM = mapMC
{-# INLINE [0] mapM #-}
{-# RULES "conduit: unstream mapM" forall f.
    mapM f = unstream (streamConduit (mapMC f) (mapMS f))
  #-}

mapMC :: Monad m => (a -> m b) -> Conduit a m b
mapMC f = awaitForever $ \a -> lift (f a) >>= yield
{-# INLINE mapMC #-}

mapMS :: Monad m => (a -> m b) -> Stream m a r -> Stream m b r
mapMS f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        case res of
            Stop r -> return $ Stop r
            Emit s' a -> Emit s' `liftM` f a
            Skip s' -> return $ Skip s'
{-# INLINE mapMS #-}

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
iterM :: Monad m => (a -> m ()) -> Conduit a m a
iterM = iterMC
{-# INLINE [2] iterM #-}
{-# RULES "conduit: unstream iterM" forall f.
    iterM f = unstream (streamConduit (iterMC f) (iterMS f))
  #-}

iterMC :: Monad m => (a -> m ()) -> Conduit a m a
iterMC f = awaitForever $ \a -> lift (f a) >> yield a
{-# INLINE iterMC #-}

iterMS :: Monad m => (a -> m ()) -> (Stream m a () -> Stream m a ())
iterMS f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip s'
            Emit s' x -> f x >> return (Emit s' x)
{-# INLINE iterMS #-}

-- | Apply a transformation that may fail to all values in a stream, discarding
-- the failures.
--
-- Subject to fusion
--
-- Since 0.5.1
mapMaybe :: Monad m => (a -> Maybe b) -> Conduit a m b
mapMaybe = mapMaybeC
{-# INLINE [2] mapMaybe #-}
{-# RULES "conduit: unstream mapMaybe" forall f.
    mapMaybe f = unstream (streamConduit (mapMaybeC f) (mapMaybeS f))
  #-}

mapMaybeC :: Monad m => (a -> Maybe b) -> Conduit a m b
mapMaybeC f = awaitForever $ maybe (return ()) yield . f
{-# INLINE mapMaybeC #-}

mapMaybeS :: Monad m => (a -> Maybe b) -> (Stream m a () -> Stream m b ())
mapMaybeS f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip s'
            Emit s' x ->
                case f x of
                    Just y -> Emit s' y
                    Nothing -> Skip s'
{-# INLINE mapMaybeS #-}

-- | Apply a monadic transformation that may fail to all values in a stream,
-- discarding the failures.
--
-- Subject to fusion
--
-- Since 0.5.1
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Conduit a m b
mapMaybeM = mapMaybeMC
{-# INLINE [0] mapMaybeM #-}
{-# RULES "conduit: unstream mapMaybeM" forall f.
    mapMaybeM f = unstream (streamConduit (mapMaybeMC f) (mapMaybeMS f))
  #-}

mapMaybeMC :: Monad m => (a -> m (Maybe b)) -> Conduit a m b
mapMaybeMC f = awaitForever $ maybe (return ()) yield <=< lift . f
{-# INLINE mapMaybeMC #-}

mapMaybeMS :: Monad m => (a -> m (Maybe b)) -> (Stream m a () -> Stream m b ())
mapMaybeMS f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip s'
            Emit s' x -> do
                my <- f x
                case my of
                    Just y -> return $ Emit s' y
                    Nothing -> return $ Skip s'
{-# INLINE mapMaybeMS #-}

-- | Filter the @Just@ values from a stream, discarding the @Nothing@  values.
--
-- Subject to fusion
--
-- Since 0.5.1
catMaybes :: Monad m => Conduit (Maybe a) m a
catMaybes = catMaybesC
{-# INLINE [2] catMaybes #-}
{-# RULES "conduit: unstream catMaybes"
    catMaybes = unstream (streamConduit catMaybesC catMaybesS)
  #-}

catMaybesC :: Monad m => Conduit (Maybe a) m a
catMaybesC = awaitForever $ maybe (return ()) yield
{-# INLINE catMaybesC #-}

catMaybesS :: Monad m => Stream m (Maybe a) () -> Stream m a ()
catMaybesS (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip s'
            Emit s' Nothing -> Skip s'
            Emit s' (Just x) -> Emit s' x
{-# INLINE catMaybesS #-}

-- | Generalization of 'catMaybes'. It puts all values from
--   'F.Foldable' into stream.
--
-- Subject to fusion
--
-- Since 1.0.6
concat :: (Monad m, F.Foldable f) => Conduit (f a) m a
concat = concatC
{-# INLINE [2] concat #-}
{-# RULES "conduit: unstream concat"
    concat = unstream (streamConduit concatC concatS)
  #-}

concatC :: (Monad m, F.Foldable f) => Conduit (f a) m a
concatC = awaitForever $ F.mapM_ yield
{-# INLINE concatC #-}

concatS :: (Monad m, F.Foldable f) => Stream m (f a) () -> Stream m a ()
concatS (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip ([], s')
            Emit s' x -> Skip (F.toList x, s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE concatS #-}

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap = concatMapC
{-# INLINE [2] concatMap #-}
{-# RULES "conduit: unstream concatMap" forall f.
    concatMap f = unstream (streamConduit (concatMapC f) (concatMapS f))
  #-}

concatMapC :: Monad m => (a -> [b]) -> Conduit a m b
concatMapC f = awaitForever $ sourceList . f
{-# INLINE concatMapC #-}

concatMapS :: Monad m => (a -> [b]) -> (Stream m a () -> Stream m b ())
concatMapS f (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip ([], s')
            Emit s' x -> Skip (f x, s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE concatMapS #-}

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f = awaitForever $ sourceList <=< lift . f
{-# INLINE [2] concatMapM #-}
{-# RULES "conduit: unstream concatMapM" forall f.
    concatMapM f = unstream (streamConduit (concatMapMC f) (concatMapMS f))
  #-}

concatMapMC :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapMC f = awaitForever $ sourceList <=< lift . f
{-# INLINE concatMapMC #-}

concatMapMS :: Monad m => (a -> m [b]) -> (Stream m a () -> Stream m b ())
concatMapMS f (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip ([], s')
            Emit s' x -> do
                xs <- f x
                return $ Skip (xs, s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE concatMapMS #-}

-- | 'concatMap' with an accumulator.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapAccum :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccum = concatMapAccumC
{-# INLINE [2] concatMapAccum #-}
{-# RULES "conduit: unstream concatMapAccum" forall f x.
    concatMapAccum f x = unstream (streamConduit (concatMapAccumC f x) (concatMapAccumS f x))
  #-}

concatMapAccumC :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccumC f x0 = void (mapAccum f x0) =$= concat
{-# INLINE concatMapAccumC #-}

concatMapAccumS :: Monad m => (a -> accum -> (accum, [b])) -> accum -> (Stream m a () -> Stream m b ())
concatMapAccumS f  initial (Stream step ms0) =
    Stream step' (liftM (initial, [], ) ms0)
  where
    step' (accum, [], s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (accum, [], s')
            Emit s' x ->
                let (accum', xs) = f x accum
                in Skip (accum', xs, s')
    step' (accum, (x:xs), s) = return (Emit (accum, xs, s) x)
{-# INLINE concatMapAccumS #-}

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

-- | Analog of @mapAccumL@ for lists.
--
-- Subject to fusion
--
-- Since 1.1.1
mapAccum :: Monad m => (a -> s -> (s, b)) -> s -> ConduitM a b m s
mapAccum = mapAccumC
{-# INLINE [2] mapAccum #-}
{-# RULES "conduit: unstream mapAccum" forall f x.
    mapAccum f x = unstream (streamConduit (mapAccumC f x) (mapAccumS f x))
  #-}

mapAccumC :: Monad m => (a -> s -> (s, b)) -> s -> ConduitM a b m s
mapAccumC f =
    loop
  where
    loop s = await >>= maybe (return s) go
      where
        go a = case f a s of
                 (s', b) -> yield b >> loop s'

mapAccumS :: Monad m => (a -> s -> (s, b)) -> s -> (Stream m a () -> Stream m b s)
mapAccumS f initial (Stream step ms0) =
    Stream step' (liftM (initial, ) ms0)
  where
    step' (accum, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop accum
            Skip s' -> Skip (accum, s')
            Emit s' x ->
                let (accum', r) = f x accum
                in Emit (accum', s') r
{-# INLINE mapAccumS #-}

-- | Monadic `mapAccum`.
--
-- Subject to fusion
--
-- Since 1.1.1
mapAccumM :: Monad m => (a -> s -> m (s, b)) -> s -> ConduitM a b m s
mapAccumM = mapAccumMC
{-# INLINE [2] mapAccumM #-}
{-# RULES "conduit: unstream mapAccumM" forall f x.
    mapAccumM f x = unstream (streamConduit (mapAccumMC f x) (mapAccumMS f x))
  #-}

mapAccumMC :: Monad m => (a -> s -> m (s, b)) -> s -> ConduitM a b m s
mapAccumMC f =
    loop
  where
    loop s = await >>= maybe (return s) go
      where
        go a = do (s', b) <- lift $ f a s
                  yield b
                  loop s'
{-# INLINE mapAccumMC #-}

mapAccumMS :: Monad m => (a -> s -> m (s, b)) -> s -> (Stream m a () -> Stream m b s)
mapAccumMS f initial (Stream step ms0) =
    Stream step' (liftM (initial, ) ms0)
  where
    step' (accum, s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop accum
            Skip s' -> return $ Skip (accum, s')
            Emit s' x -> do
                (accum', r) <- f x accum
                return $ Emit (accum', s') r
{-# INLINE mapAccumMS #-}

-- | Analog of 'Prelude.scanl' for lists.
--
-- Subject to fusion
--
-- Since 1.1.1
scan :: Monad m => (a -> b -> b) -> b -> ConduitM a b m b
scan f =
    mapAccum $ \a b -> let b' = f a b in (b', b')
{-# INLINE [2] scan #-}
{-# RULES "conduit: inline scan" forall f.
    scan f = mapAccum $ \a b -> let b' = f a b in (b', b')
  #-}

-- | Monadic @scanl@.
--
-- Subject to fusion
--
-- Since 1.1.1
scanM :: Monad m => (a -> b -> m b) -> b -> ConduitM a b m b
scanM f =
    mapAccumM $ \a b -> do b' <- f a b
                           return (b', b')
{-# INLINE [2] scanM #-}
{-# RULES "conduit: inline scanM" forall f.
    scanM f = mapAccumM $ \a b -> do b' <- f a b
                                     return (b', b')
  #-}

-- | 'concatMapM' with an accumulator.
--
-- Subject to fusion
--
-- Since 0.3.0
concatMapAccumM :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
concatMapAccumM = concatMapAccumMC
{-# INLINE [2] concatMapAccumM #-}
{-# RULES "conduit: unstream concatMapAccumM" forall f x.
    concatMapAccumM f x = unstream (streamConduit (concatMapAccumMC f x) (concatMapAccumMS f x))
  #-}

concatMapAccumMC :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
concatMapAccumMC f x0 = void (mapAccumM f x0) =$= concat
{-# INLINE concatMapAccumMC #-}

concatMapAccumMS :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> (Stream m a () -> Stream m b ())
concatMapAccumMS f  initial (Stream step ms0) =
    Stream step' (liftM (initial, [], ) ms0)
  where
    step' (accum, [], s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip (accum, [], s')
            Emit s' x -> do
                (accum', xs) <- f x accum
                return $ Skip (accum', xs, s')
    step' (accum, (x:xs), s) = return (Emit (accum, xs, s) x)
{-# INLINE concatMapAccumMS #-}

-- | Generalization of 'mapMaybe' and 'concatMap'. It applies function
-- to all values in a stream and send values inside resulting
-- 'Foldable' downstream.
--
-- Subject to fusion
--
-- Since 1.0.6
mapFoldable :: (Monad m, F.Foldable f) => (a -> f b) -> Conduit a m b
mapFoldable = mapFoldableC
{-# INLINE [2] mapFoldable #-}
{-# RULES "conduit: unstream mapFoldable" forall f.
    mapFoldable f = unstream (streamConduit (mapFoldableC f) (mapFoldableS f))
  #-}

mapFoldableC :: (Monad m, F.Foldable f) => (a -> f b) -> Conduit a m b
mapFoldableC f = awaitForever $ F.mapM_ yield . f
{-# INLINE mapFoldableC #-}

mapFoldableS :: (Monad m, F.Foldable f) => (a -> f b) -> Stream m a () -> Stream m b ()
mapFoldableS f (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip ([], s')
            Emit s' x -> Skip (F.toList (f x), s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE mapFoldableS #-}

-- | Monadic variant of 'mapFoldable'.
--
-- Subject to fusion
--
-- Since 1.0.6
mapFoldableM :: (Monad m, F.Foldable f) => (a -> m (f b)) -> Conduit a m b
mapFoldableM = mapFoldableMC
{-# INLINE [2] mapFoldableM #-}
{-# RULES "conduit: unstream mapFoldableM" forall f.
    mapFoldableM f = unstream (streamConduit (mapFoldableMC f) (mapFoldableMS f))
  #-}

mapFoldableMC :: (Monad m, F.Foldable f) => (a -> m (f b)) -> Conduit a m b
mapFoldableMC f = awaitForever $ F.mapM_ yield <=< lift . f
{-# INLINE mapFoldableMC #-}

mapFoldableMS :: (Monad m, F.Foldable f) => (a -> m (f b)) -> Stream m a () -> Stream m b ()
mapFoldableMS f (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip ([], s')
            Emit s' x -> do
                y <- f x
                return $ Skip (F.toList y, s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE mapFoldableMS #-}

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
--
-- Subject to fusion
--
-- Since 0.3.0
consume :: Monad m => Consumer a m [a]
consume = consumeC
{-# INLINE [0] consume #-}
{-# RULES "conduit: unstream consume" consume = unstream (streamConduit consumeC consumeS) #-}

consumeC :: Monad m => Consumer a m [a]
consumeC =
    loop id
  where
    loop front = await >>= maybe (return $ front []) (\x -> loop $ front . (x:))
{-# INLINE consumeC #-}

consumeS :: Monad m => Stream m a () -> Stream m o [a]
consumeS (Stream step ms0) =
    Stream step' (liftM (id,) ms0)
  where
    step' (front, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop (front [])
            Skip s' -> Skip (front, s')
            Emit s' a -> Skip (front . (a:), s')
{-# INLINE consumeS #-}

-- | Grouping input according to an equality function.
--
-- Subject to fusion
--
-- Since 0.3.0
groupBy :: Monad m => (a -> a -> Bool) -> Conduit a m [a]
groupBy = groupByC
{-# INLINE [2] groupBy #-}
{-# RULES "conduit: unstream groupBy" forall f.
    groupBy f = unstream (streamConduit (groupByC f) (groupByS f))
  #-}

groupByC :: Monad m => (a -> a -> Bool) -> Conduit a m [a]
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

groupByS :: Monad m => (a -> a -> Bool) -> Stream m a () -> Stream m [a] ()
groupByS f = mapS (Prelude.uncurry (:)) . groupOn1S f
{-# INLINE groupByS #-}

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
groupOn1 :: (Monad m, Eq b)
          => (a -> b)
          -> Conduit a m (a, [a])
groupOn1 = groupOn1C
{-# INLINE [2] groupOn1 #-}
{-# RULES "conduit: unstream groupOn1" forall f.
    groupOn1 f = unstream (streamConduit (groupOn1C f) (groupOn1S f))
  #-}

groupOn1C :: (Monad m, Eq b)
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

data GroupByState m a
    = GBStart
    | GBLoop ([a] -> [a]) a
    | GBDone

groupOn1S :: Monad m => (a -> a -> Bool) -> Stream m a () -> Stream m (a, [a]) ()
groupOn1S f (Stream step ms0) =
    Stream step' (liftM (GBStart, ) ms0)
  where
    step' (GBStart, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (GBStart, s')
            Emit s' x0 -> Skip (GBLoop id x0, s')
    step' (cur@(GBLoop rest x0), s) = do
        res <- step s
        return $ case res of
            Stop () -> Emit (GBDone, s) (x0, rest [])
            Skip s' -> Skip (cur, s')
            Emit s' x
                | f x0 x -> Skip (GBLoop (rest . (x:)) x0, s')
                | otherwise -> Emit (GBLoop id x, s') (x0, rest [])
    step' (GBDone, _) = return $ Stop ()
{-# INLINE groupOn1S #-}

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
isolate :: Monad m => Int -> Conduit a m a
isolate = isolateC
{-# INLINE [2] isolate #-}
{-# RULES "conduit: unstream isolate" forall n.
    isolate n = unstream (streamConduit (isolateC n) (isolateS n))
  #-}

isolateC :: Monad m => Int -> Conduit a m a
isolateC =
    loop
  where
    loop count | count <= 0 = return ()
    loop count = await >>= maybe (return ()) (\x -> yield x >> loop (count - 1))

isolateS :: Monad m => Int -> (Stream m a () -> Stream m a ())
isolateS count (Stream step ms0) =
    Stream step' (liftM (count,) ms0)
  where
    step' (n, s) | n <= 0 = return $ Stop ()
    step' (n, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (n, s')
            Emit s' x -> Emit (n - 1, s') x
{-# INLINE isolateS #-}

-- | Keep only values in the stream passing a given predicate.
--
-- Subject to fusion
--
-- Since 0.3.0
filter :: Monad m => (a -> Bool) -> Conduit a m a
filter = filterC
{-# INLINE [2] filter #-}
{-# RULES "conduit: unstream filter" forall f.
    filter f = unstream (streamConduit (filterC f) (filterS f))
  #-}

filterC :: Monad m => (a -> Bool) -> Conduit a m a
filterC f = awaitForever $ \i -> when (f i) (yield i)

filterS :: Monad m => (a -> Bool) -> (Stream m a () -> Stream m a ())
filterS f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip s'
            Emit s' x
                | f x -> Emit s' x
                | otherwise -> Skip s'

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
sinkNull :: Monad m => Consumer a m ()
sinkNull = sinkNullC
{-# INLINE [2] sinkNull #-}
{-# RULES "conduit: unstream sinkNull"
    sinkNull = unstream (streamConduit sinkNullC sinkNullS)
  #-}

sinkNullC :: Monad m => Consumer a m ()
sinkNullC = awaitForever $ \_ -> return ()
{-# INLINE sinkNullC #-}

sinkNullS :: Monad m => Stream m a () -> Stream m () ()
sinkNullS (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip s'
            Emit s' _ -> Skip s'
{-# INLINE sinkNullS #-}

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
sourceNull :: Monad m => Producer m a
sourceNull = sourceNullC
{-# INLINE [2] sourceNull #-}
{-# RULES "conduit: unstream sourceNull"
    sourceNull = unstream (streamConduit sourceNullC (\_ -> sourceNullS))
  #-}

sourceNullC :: Monad m => Producer m a
sourceNullC = return ()
{-# INLINE sourceNullC #-}

sourceNullS :: Monad m => Stream m a ()
sourceNullS = Stream (\_ -> return (Stop ())) (return ())
{-# INLINE sourceNullS #-}

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

#ifdef QUICKCHECK
props = describe "Data.Conduit.List" $ do
    qit "unfoldC" $
        \(getBlind -> f, initial :: Int) ->
            unfoldC f initial `checkInfiniteProducer`
            (Data.List.unfoldr f initial :: [Int])
    qit "unfoldS" $
        \(getBlind -> f, initial :: Int) ->
            unfoldS f initial `checkInfiniteStreamProducer`
            (Data.List.unfoldr f initial :: [Int])
    todo "unfoldMC"
    todo "unfoldMS"
    qit "sourceListC" $
        \(xs :: [Int]) ->
            sourceListC xs `checkProducer` xs
    qit "sourceListS" $
        \(xs :: [Int]) ->
            sourceListS xs `checkStreamProducer` xs
    qit "enumFromToC" $
        \(fr :: Small Int, to :: Small Int) ->
            enumFromToC fr to `checkProducer`
            Prelude.enumFromTo fr to
    qit "enumFromToS" $
        \(fr :: Small Int, to :: Small Int) ->
            enumFromToS fr to `checkStreamProducer`
            Prelude.enumFromTo fr to
    qit "enumFromToS_int" $
        \(getSmall -> fr :: Int, getSmall -> to :: Int) ->
            enumFromToS_int fr to `checkStreamProducer`
            Prelude.enumFromTo fr to
    qit "iterateC" $
        \(getBlind -> f, initial :: Int) ->
            iterateC f initial `checkInfiniteProducer`
            Prelude.iterate f initial
    qit "iterateS" $
        \(getBlind -> f, initial :: Int) ->
            iterateS f initial `checkInfiniteStreamProducer`
            Prelude.iterate f initial
    qit "replicateC" $
        \(getSmall -> n) ->
            replicateC n '0' `checkProducer`
            Prelude.replicate n '0'
    qit "replicateS" $
        \(getSmall -> n) ->
            replicateS n '0' `checkStreamProducer`
            Prelude.replicate n '0'
    todo "replicateMC"
    todo "replicateMS"
    qit "foldC" $
        \(getBlind -> f, initial :: Int) ->
            foldC f initial `checkConsumer`
            Data.List.foldl' f initial
    {-qit "foldS" $
        \(getBlind -> f, initial :: Int) ->
            foldC f initial `checkStreamConsumer`
            Data.List.foldl' f initial-}
    todo "foldMC"
    todo "foldMS"
    todo "connectFold"
    todo "connectFoldM"
    qit "foldMap" $
        \(getBlind -> (f :: Int -> Sum Int)) ->
            foldMap f `checkConsumer`
            Data.Foldable.foldMap f
    todo "mapM_C"
    todo "mapM_S"
    todo "srcMapM_"
    {-qit "dropC" $
        \(getSmall -> n) ->
            dropC n `checkConsumer`
            Prelude.drop n-}
    todo "dropS"
    qit "take" $
        \(getSmall -> n) ->
            take n `checkConsumer`
            Prelude.take n
    todo "takeS"
    qit "headC" $
        \() ->
            headC `checkConsumer`
            Safe.headMay
    todo "headS"
    qit "peek" $
        \() ->
            peek `checkConsumer`
            Safe.headMay
    qit "mapC" $
        \(getBlind -> f) ->
            mapC f `checkConduit`
            (Prelude.map f :: [Int] -> [Int])
    {-qit "mapS" $
        \(getBlind -> f) ->
            mapS f `checkStreamConduit`
            Prelude.map f-}
    todo "mapMC"
    todo "mapMS"
    todo "iterMC"
    todo "iterMS"
    qit "mapMaybeC" $
        \(getBlind -> f) ->
            mapMaybeC f `checkConduit`
            (Data.Maybe.mapMaybe f :: [Int] -> [Int])
    todo "mapMaybeS"
    todo "mapMaybeMC"
    todo "mapMaybeMS"
    qit "catMaybesC" $
        \() ->
            catMaybesC `checkConduit`
            (Data.Maybe.catMaybes :: [Maybe Int] -> [Int])
    todo "catMaybesS"
    qit "concatC" $
        \() ->
            concatC `checkConduit`
            (Prelude.concat :: [[Int]] -> [Int])
    todo "concatS"
    qit "concatMapC" $
        \(getBlind -> f) ->
            concatMapC f `checkConduit`
            (Prelude.concatMap f :: [Int] -> [Int])
    todo "concatMapS"
    todo "concatMapMC"
    todo "concatMapMS"
    todo "concatMapAccumC"
    todo "concatMapAccumS"
    todo "mapAccumC"
    todo "mapAccumS"
    todo "mapAccumMC"
    todo "mapAccumMS"
    {-qit "scan" $
        \(getBlind -> f, initial :: Int) ->
            scan f initial `checkConduit`
            Prelude.scanr f initial-}
    todo "scanM"
    todo "mapFoldableC"
    todo "mapFoldableS"
    todo "mapFoldableMC"
    todo "mapFoldableMS"
    qit "consumeC" $
        \() ->
            consumeC `checkConsumer`
            id
    todo "consumeS"
    qit "groupByC" $
        \(getBlind -> f) ->
            groupByC f `checkConduit`
            (Data.List.groupBy f :: [Int] -> [[Int]])
    todo "groupByS"
    todo "groupOn1C"
    todo "groupOn1S"
    qit "isolateC" $
        \n ->
            isolateC n `checkConduit`
            (Data.List.take n :: [Int] -> [Int])
    todo "isolateS"
    qit "filterC" $
        \(getBlind -> f) ->
            filterC f `checkConduit`
            (Data.List.filter f :: [Int] -> [Int])
    todo "filterS"
    todo "filterFuseRight"
    todo "sinkNullC"
    todo "sinkNullS"
    todo "srcSinkNull"
    qit "sourceNullC" $
        \() ->
            sourceNull `checkProducer`
            ([] :: [Int])
    todo "sourceNullS"
    todo "sequence"

todo n = it n $ True

qit n f = it n $ property $ forAll arbitrary f

checkProducer :: (Prelude.Show a, Eq a) => Source Identity a -> [a] -> Property
checkProducer c l =
    runIdentity (c $$ consume) === l

checkStreamProducer :: (Prelude.Show a, Eq a) => Stream Identity a () -> [a] -> Property
checkStreamProducer s l =
    runIdentity (unstream (streamSource s) $$ consume) === l

checkInfiniteProducer :: (Prelude.Show a, Eq a) => Source Identity a -> [a] -> Property
checkInfiniteProducer s l =
    checkProducer (s $= isolate 10) (Prelude.take 10 l)

checkInfiniteStreamProducer :: (Prelude.Show a, Eq a) => Stream Identity a () -> [a] -> Property
checkInfiniteStreamProducer s l =
    runIdentity (unstream (streamSource s) $= isolate 10 $$ consume) === Prelude.take 10 l

checkConsumer :: (Prelude.Show b, Eq b) => Consumer Int Identity b -> ([Int] -> b) -> Property
checkConsumer c l = forAll arbitrary $ \xs ->
    runIdentity (sourceList xs $$ c) === l xs

{- TODO
checkStreamConsumer :: (Prelude.Show b, Eq b) => Stream Identity Int b -> ([Int] -> b) -> Property
checkStreamConsumer = forAll arbitrary $ \xs ->
    runIdentity (sourceList xs $$ c) === l xs
-}

checkConduit :: (Prelude.Show a, Arbitrary a, Prelude.Show b, Eq b) => Conduit a Identity b -> ([a] -> [b]) -> Property
checkConduit c l = forAll arbitrary $ \xs ->
    runIdentity (sourceList xs $= c $$ consume) == l xs

-- checkStreamConduit :: Stream Identity Int Int -> ([Int] -> [Int]) -> Property
-- checkStreamConduit c l = forAll arbitrary $ \xs ->
--     runIdentity (unstream (streamSource s))

-- Prefer this to creating an orphan instance for Data.Monoid.Sum:

newtype Sum a = Sum a
  deriving (Eq, Prelude.Show, Arbitrary)

instance Prelude.Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum $ x Prelude.+ y

#endif
