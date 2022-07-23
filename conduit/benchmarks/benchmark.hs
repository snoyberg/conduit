{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
import           Control.Monad
import           Control.Monad.Identity
import           Criterion.Main
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.List (consume)
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Monoid
import qualified Data.NonNull as NonNull
import           Data.Sequence as S
import qualified Data.Sequences as Seq
import           Data.Sequences.Lazy
import qualified Data.Text as T
import           Prelude

maximumC :: (Monad m, Ord a) => ConduitT a o m (Maybe a)
maximumC =
    await >>= maybe (return Nothing) loop
  where
    loop prev = await >>= maybe (return $ Just prev) (loop . max prev)
{-# INLINE maximumC #-}

allC :: Monad m
     => (a -> Bool)
     -> ConduitT a o m Bool
allC f = fmap isNothing $ C.find (Prelude.not . f)
{-# INLINE allC #-}

allOld :: Monad m
       => (a -> Bool)
       -> ConduitT a o m Bool
allOld f =
    loop
  where
    loop = await >>= maybe (return True) go
    go x = if f x then loop else return False
{-# INLINE allOld #-}

foldl1E f = foldC (foldMaybeNull f) Nothing
{-# INLINE foldl1E #-}

-- Helper for foldl1E
foldMaybeNull :: (MonoFoldable mono, e ~ Element mono)
              => (e -> e -> e)
              -> Maybe e
              -> mono
              -> Maybe e
foldMaybeNull f macc mono =
    case (macc, NonNull.fromNullable mono) of
        (Just acc, Just nn) -> Just $ ofoldl' f acc nn
        (Nothing, Just nn) -> Just $ NonNull.ofoldl1' f nn
        _ -> macc
{-# INLINE foldMaybeNull #-}

foldC :: Monad m
      => (b -> a -> b)
      -> b
      -> ConduitT a o m b
foldC f =
    loop
  where
    loop !accum = await >>= maybe (return accum) (loop . f accum)
{-# INLINE foldC #-}

maximumEOld :: (Monad m, Seq.OrdSequence seq) => ConduitT seq o m (Maybe (Element seq))
maximumEOld =
    start
  where
    start = await >>= maybe (return Nothing) start'
    start' x =
        case NonNull.fromNullable x of
            Nothing -> start
            Just y -> loop $ NonNull.maximum y
    loop prev = await >>= maybe (return $ Just prev) (loop . ofoldl' max prev)
{-# INLINE maximumEOld #-}

linesUnboundedOld :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
                  => Conduit seq m seq
linesUnboundedOld =
    start
  where
    start = await >>= maybe (return ()) loop

    loop t =
        if onull y
            then do
                mt <- await
                case mt of
                    Nothing -> unless (onull t) $ yield t
                    Just t' -> loop (t `mappend` t')
            else yield x >> loop (Seq.drop 1 y)
      where
        (x, y) = Seq.break (== '\n') t
{-# INLINE linesUnboundedOld #-}

splitOnUnboundedEC
    :: (Monad m, Seq.IsSequence seq)
    => (Element seq -> Bool) -> Conduit seq m seq
splitOnUnboundedEC f =
    start
  where
    start = await >>= maybe (return ()) loop

    loop t =
        if onull y
            then do
                mt <- await
                case mt of
                    Nothing -> unless (onull t) $ yield t
                    Just t' -> loop (t `mappend` t')
            else yield x >> loop (Seq.drop 1 y)
      where
        (x, y) = Seq.break f t
{-# INLINE splitOnUnboundedEC #-}

#define RUN_SINK_N(n, consumer)            \
    flip whnf (n :: Int) $ \upper ->       \
        runIdentity $                      \
            C.enumFromTo 1 upper           \
         $$ C.map (+2)                     \
         $= C.map (+1)                     \
         $= consumer

#define RUN_SINK(consumer) RUN_SINK_N(100000, consumer)

main = defaultMain
    -- Benchmarks for 'all'
    [ bench "fused all" $
        RUN_SINK(C.all (<90000))
    , bench "unfused all" $
        RUN_SINK(allC (<90000))
    , bench "unfused all (old version)" $
        RUN_SINK(allOld (<90000))
    -- Benchmarks for 'maximumE'
    , bench "fused maximumE" $
        RUN_SINK(C.map (\n -> S.replicate (n `mod` 20) n) =$= C.maximumE)
    , bench "unfused maximumE" $
        RUN_SINK(C.map (\n -> S.replicate (n `mod` 20) n) =$= foldl1E max)
    , bench "unfused maximumE (old version)" $
        RUN_SINK(C.map (\n -> S.replicate (n `mod` 20) n) =$= maximumEOld)
    -- Benchmarks for 'linesUnbounded'
    , bench "fused linesUnbounded" $
        RUN_SINK_N(1000, C.map (\n -> T.replicate (n `mod` 20) (T.singleton (toEnum n))) =$= C.linesUnbounded =$= C.map T.length =$= C.sum)
    , bench "unfused linesUnbounded" $
        RUN_SINK_N(1000, C.map (\n -> T.replicate (n `mod` 20) (T.singleton (toEnum n))) =$= splitOnUnboundedEC (== '\n') =$= C.map T.length =$= C.sum)
    , bench "unfused linesUnbounded (old version)" $
        RUN_SINK_N(1000, C.map (\n -> T.replicate (n `mod` 20) (T.singleton (toEnum n))) =$= linesUnboundedOld =$= C.map T.length =$= C.sum)
    ]
