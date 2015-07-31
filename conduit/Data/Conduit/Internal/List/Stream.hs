{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module Data.Conduit.Internal.List.Stream where

import           Control.Monad (liftM)
import           Data.Conduit.Internal.Fusion
import qualified Data.Foldable as F

--FIXME: Should streamSource / streamSourcePure be used for sources?

unfoldS :: Monad m
        => (b -> Maybe (a, b))
        -> b
        -> StreamProducer m a
unfoldS f s0 _ =
    Stream step (return s0)
  where
    step s = return $
        case f s of
            Nothing -> Stop ()
            Just (x, s') -> Emit s' x
{-# INLINE unfoldS #-}

unfoldMS :: Monad m
         => (b -> m (Maybe (a, b)))
         -> b
         -> StreamProducer m a
unfoldMS f s0 _ =
    Stream step (return s0)
  where
    step s = do
        ms' <- f s
        return $ case ms' of
            Nothing -> Stop ()
            Just (x, s') -> Emit s' x
{-# INLINE unfoldMS #-}

sourceListS :: Monad m => [a] -> StreamProducer m a
sourceListS xs0 _ =
    Stream (return . step) (return xs0)
  where
    step [] = Stop ()
    step (x:xs) = Emit xs x
{-# INLINE sourceListS #-}

enumFromToS :: (Enum a, Prelude.Ord a, Monad m)
            => a
            -> a
            -> StreamProducer m a
enumFromToS x0 y _ =
    Stream step (return x0)
  where
    step x = return $ if x Prelude.> y
        then Stop ()
        else Emit (Prelude.succ x) x
{-# INLINE [0] enumFromToS #-}

enumFromToS_int :: (Prelude.Integral a, Monad m)
                => a
                -> a
                -> StreamProducer m a
enumFromToS_int x0 y _ = x0 `seq` y `seq` Stream step (return x0)
  where
    step x | x <= y    = return $ Emit (x Prelude.+ 1) x
           | otherwise = return $ Stop ()
{-# INLINE enumFromToS_int #-}

{-# RULES "conduit: enumFromTo<Int>" forall f t.
      enumFromToS f t = enumFromToS_int f t :: Monad m => StreamProducer m Int
  #-}

iterateS :: Monad m => (a -> a) -> a -> StreamProducer m a
iterateS f x0 _ =
    Stream (return . step) (return x0)
  where
    step x = Emit x' x
      where
        x' = f x
{-# INLINE iterateS #-}

replicateS :: Monad m => Int -> a -> StreamProducer m a
replicateS cnt0 a _ =
    Stream step (return cnt0)
  where
    step cnt
        | cnt <= 0  = return $ Stop ()
        | otherwise = return $ Emit (cnt - 1) a
{-# INLINE replicateS #-}

replicateMS :: Monad m => Int -> m a -> StreamProducer m a
replicateMS cnt0 ma _ =
    Stream step (return cnt0)
  where
    step cnt
        | cnt <= 0  = return $ Stop ()
        | otherwise = Emit (cnt - 1) `liftM` ma
{-# INLINE replicateMS #-}

foldS :: Monad m => (b -> a -> b) -> b -> StreamConsumer a m b
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

foldMS :: Monad m => (b -> a -> m b) -> b -> StreamConsumer a m b
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

mapM_S :: Monad m
       => (a -> m ())
       -> StreamConsumer a m ()
mapM_S f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        case res of
          Stop () -> return $ Stop ()
          Skip s' -> return $ Skip s'
          Emit s' x -> f x >> return (Skip s')
{-# INLINE [1] mapM_S #-}

dropS :: Monad m
      => Int
      -> StreamConsumer a m ()
dropS n0 (Stream step ms0) =
    Stream step' (liftM (, n0) ms0)
  where
    step' (_, n) | n <= 0 = return $ Stop ()
    step' (s, n) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (s', n)
            Emit s' _ -> Skip (s', n - 1)
{-# INLINE dropS #-}

takeS :: Monad m
      => Int
      -> StreamConsumer a m [a]
takeS n0 (Stream step s0) =
    Stream step' (liftM (id, n0,) s0)
  where
    step' (output, n, _) | n <= 0 = return $ Stop (output [])
    step' (output, n, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop (output [])
            Skip s' -> Skip (output, n, s')
            Emit s' x -> Skip (output . (x:), n - 1, s')
{-# INLINE takeS #-}

headS :: Monad m => StreamConsumer a m (Maybe a)
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

mapS :: Monad m => (a -> b) -> StreamConduit a m b
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

mapMS :: Monad m => (a -> m b) -> StreamConduit a m b
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

iterMS :: Monad m => (a -> m ()) -> StreamConduit a m a
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

mapMaybeS :: Monad m => (a -> Maybe b) -> StreamConduit a m b
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

mapMaybeMS :: Monad m => (a -> m (Maybe b)) -> StreamConduit a m b
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

catMaybesS :: Monad m => StreamConduit (Maybe a) m a
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

concatS :: (Monad m, F.Foldable f) => StreamConduit (f a) m a
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

concatMapS :: Monad m => (a -> [b]) -> StreamConduit a m b
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

concatMapMS :: Monad m => (a -> m [b]) -> StreamConduit a m b
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

concatMapAccumS :: Monad m => (a -> accum -> (accum, [b])) -> accum -> StreamConduit a m b
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

mapAccumS :: Monad m => (a -> s -> (s, b)) -> s -> StreamConduitM a b m s
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

mapAccumMS :: Monad m => (a -> s -> m (s, b)) -> s -> StreamConduitM a b m s
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

concatMapAccumMS :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> StreamConduit a m b
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

mapFoldableS :: (Monad m, F.Foldable f) => (a -> f b) -> StreamConduit a m b
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

mapFoldableMS :: (Monad m, F.Foldable f) => (a -> m (f b)) -> StreamConduit a m b
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

consumeS :: Monad m => StreamConsumer a m [a]
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

groupByS :: Monad m => (a -> a -> Bool) -> StreamConduit a m [a]
groupByS f = mapS (Prelude.uncurry (:)) . groupBy1S id f
{-# INLINE groupByS #-}

groupOn1S :: (Monad m, Eq b) => (a -> b) -> StreamConduit a m (a, [a])
groupOn1S f = groupBy1S f (==)
{-# INLINE groupOn1S #-}

data GroupByState a b s
     = GBStart s
     | GBLoop ([a] -> [a]) a b s
     | GBDone

groupBy1S :: Monad m => (a -> b) -> (b -> b -> Bool) -> StreamConduit a m (a, [a])
groupBy1S f eq (Stream step ms0) =
    Stream step' (liftM GBStart ms0)
  where
    step' (GBStart s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (GBStart s')
            Emit s' x0 -> Skip (GBLoop id x0 (f x0) s')
    step' (GBLoop rest x0 fx0 s) = do
        res <- step s
        return $ case res of
            Stop () -> Emit GBDone (x0, rest [])
            Skip s' -> Skip (GBLoop rest x0 fx0 s')
            Emit s' x
                | fx0 `eq` f x -> Skip (GBLoop (rest . (x:)) x0 fx0 s')
                | otherwise -> Emit (GBLoop id x (f x) s') (x0, rest [])
    step' GBDone = return $ Stop ()
{-# INLINE groupBy1S #-}

isolateS :: Monad m => Int -> StreamConduit a m a
isolateS count (Stream step ms0) =
    Stream step' (liftM (count,) ms0)
  where
    step' (n, _) | n <= 0 = return $ Stop ()
    step' (n, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (n, s')
            Emit s' x -> Emit (n - 1, s') x
{-# INLINE isolateS #-}

filterS :: Monad m => (a -> Bool) -> StreamConduit a m a
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

sinkNullS :: Monad m => StreamConsumer a m ()
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

sourceNullS :: Monad m => StreamProducer m a
sourceNullS _ = Stream (\_ -> return (Stop ())) (return ())
{-# INLINE sourceNullS #-}
