{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Data.StreamConduit where

import Prelude hiding (map, enumFromTo, mapM, take)
import Data.Void (Void, absurd)
import Control.Monad (liftM)
import Control.Arrow (first)

data Step s o r
    = Done r
    | Skip s
    | Yield o s
    deriving Functor
data Stream m o r = forall s. Stream
    (s -> m (Step s o r))
    (m s)
    (s -> m ()) -- cleanup

instance Monad m => Functor (Stream m o) where
    fmap f (Stream step ms cleanup) = Stream (liftM (fmap f) . step) ms cleanup

emptyStream :: Monad m => r -> Stream m o r
emptyStream r = Stream (const $ return $ Done r) (return ()) (const $ return ())

newtype ConduitM i o m r = ConduitM
    { unConduitM :: forall x. Stream m i x -> Stream m o (Stream m i x, r)
    }

{-
instance Monad m => Monad (ConduitM i o m) where
    return
-}

(=$=) :: Monad m => ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r
ConduitM x =$= ConduitM y = ConduitM (fmap (first collapseStream) . y . x)
{-# INLINE (=$=) #-}

collapseStream :: Monad m
               => Stream m b (Stream m a x, ())
               -> Stream m a x
collapseStream = error "collapseStream"

runConduit :: Monad m => ConduitM () Void m r -> m r
runConduit (ConduitM f) =
    go $ f $ emptyStream ()
  where
    go (Stream step ms0 _) =
        ms0 >>= loop
      where
        loop s = do
            res <- step s
            case res of
                Done (_, r) -> return r
                Skip s' -> loop s'
                Yield _ s' -> loop s'
                -- FIXME why doesn't this work? Yield o _ -> absurd o
{-# INLINE runConduit #-}

map :: Monad m => (a -> b) -> ConduitM a b m ()
map f = ConduitM (mapS f)
{-# INLINE map #-}

mapS :: Monad m => (a -> b) -> Stream m a x -> Stream m b (Stream m a x, ())
mapS f (Stream step ms0 cleanup) =
    Stream step' ms0 cleanup
  where
    step' s = do
        res <- step s
        return $ case res of
            Done x -> Done (emptyStream x, ())
            Skip s' -> Skip s'
            Yield a s' -> Yield (f a) s'
{-# INLINE mapS #-}

mapM :: Monad m => (a -> m b) -> ConduitM a b m ()
mapM f = ConduitM (mapMS f)
{-# INLINE mapM #-}

mapMS :: Monad m => (a -> m b) -> Stream m a x -> Stream m b (Stream m a x, ())
mapMS f (Stream step ms0 cleanup) =
    Stream step' ms0 cleanup
  where
    step' s = do
        res <- step s
        case res of
            Done x -> return $ Done (emptyStream x, ())
            Skip s' -> return $ Skip s'
            Yield a s' -> do
                b <- f a
                return $ Yield b s'
{-# INLINE mapMS #-}

enumFromTo :: (Ord a, Enum a, Monad m) => a -> a -> ConduitM i a m ()
enumFromTo x y = ConduitM $ enumFromToS x y

enumFromToS :: (Ord a, Enum a, Monad m)
            => a
            -> a
            -> Stream m i x
            -> Stream m a (Stream m i x, ())
enumFromToS x y upstream =
    Stream (return . step) (return x) (const $ return ())
  where
    step x
        | x > y = Done (upstream, ())
        | otherwise = Yield x (succ x)

sinkList :: Monad m => ConduitM i o m [i]
sinkList = ConduitM sinkListS
{-# INLINE sinkList #-}

sinkListS :: Monad m => Stream m i x -> Stream m o (Stream m i x, [i])
sinkListS (Stream step ms0 cleanup) =
    Stream step' (liftM (, id) ms0) (cleanup . fst)
  where
    step' (s, front) = do
        res <- step s
        return $ case res of
            Done x -> Done (emptyStream x, front [])
            Skip s' -> Skip (s', front)
            Yield i s' -> Skip (s', front . (i:))
{-# INLINE sinkListS #-}

foldl' :: Monad m => (b -> a -> b) -> b -> ConduitM a o m b
foldl' f b = ConduitM (foldl'S f b)
{-# INLINE foldl' #-}

foldl'S :: Monad m => (b -> a -> b) -> b -> Stream m a x -> Stream m o (Stream m a x, b)
foldl'S f b0 (Stream step ms0 cleanup) =
    Stream step' (liftM (, b0) ms0) (cleanup . fst)
  where
    step' (s, !b) = do
        res <- step s
        return $ case res of
            Done x -> Done (emptyStream x, b)
            Skip s' -> Skip (s', b)
            Yield a s' -> Skip (s', f b a)
{-# INLINE foldl'S #-}

foldM' :: Monad m => (b -> a -> m b) -> b -> ConduitM a o m b
foldM' f b = ConduitM (foldM'S f b)
{-# INLINE foldM' #-}

foldM'S :: Monad m => (b -> a -> m b) -> b -> Stream m a x -> Stream m o (Stream m a x, b)
foldM'S f b0 (Stream step ms0 cleanup) =
    Stream step' (liftM (, b0) ms0) (cleanup . fst)
  where
    step' (s, !b) = do
        res <- step s
        case res of
            Done x -> return $ Done (emptyStream x, b)
            Skip s' -> return $ Skip (s', b)
            Yield a s' -> do
                !b' <- f b a
                return $ Skip (s', b')
{-# INLINE foldM'S #-}

take :: Monad m => Int -> ConduitM a a m ()
take i = ConduitM (takeS i)
{-# INLINE take #-}

takeS :: Monad m => Int -> Stream m a x -> Stream m a (Stream m a x, ())
takeS cnt0 (Stream step ms0 cleanup) =
    Stream step' (liftM (, cnt0) ms0) (cleanup . fst)
  where
    step' (s, cnt)
        | cnt <= 0 = return $ Done (Stream step (return s) cleanup, ())
        | otherwise = do
            res <- step s
            case res of
                Done x -> return $ Done (emptyStream x, ())
                Yield a s' -> return $ Yield a (s', cnt - 1)
                Skip s' -> return $ Skip (s', cnt)
