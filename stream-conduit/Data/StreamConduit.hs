{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Data.StreamConduit where

import Prelude hiding (map, enumFromTo, mapM)
import Data.Void (Void, absurd)
import Control.Monad (liftM)

data Step s m o r
    = Done r
    | Skip s
    | Yield o s
    | StepM (m s)
    deriving Functor
data Stream m o r = forall s. Stream
    (s -> Step s m o r)
    (m s)
    (s -> m ()) -- cleanup

emptyStream :: Monad m => Stream m o ()
emptyStream = Stream (const $ Done ()) (return ()) (const $ return ())

newtype ConduitM i o m r = ConduitM
    { unConduitM :: Stream m i () -> Stream m o r
    }

(=$=) :: ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r
ConduitM x =$= ConduitM y = ConduitM (y . x)
{-# INLINE (=$=) #-}

runConduit :: Monad m => ConduitM () Void m r -> m r
runConduit (ConduitM f) =
    go (f emptyStream)
  where
    go (Stream step ms0 _) =
        ms0 >>= loop
      where
        loop s =
            case step s of
                Done r -> return r
                Skip s' -> loop s'
                Yield o _ -> absurd o
                StepM ms -> ms >>= loop
{-# INLINE runConduit #-}

map :: Monad m => (a -> b) -> ConduitM a b m ()
map f = ConduitM (mapS f)
{-# INLINE map #-}

mapS :: Monad m => (a -> b) -> Stream m a () -> Stream m b ()
mapS f (Stream step ms0 cleanup) =
    Stream step' ms0 cleanup
  where
    step' s =
        case step s of
            Done () -> Done ()
            Skip s' -> Skip s'
            Yield a s' -> Yield (f a) s'
            StepM ms -> StepM ms
{-# INLINE mapS #-}

mapM :: Monad m => (a -> m b) -> ConduitM a b m ()
mapM f = ConduitM (mapMS f)
{-# INLINE mapM #-}

mapMS :: Monad m => (a -> m b) -> Stream m a () -> Stream m b ()
mapMS f (Stream step ms0 cleanup) =
    Stream step' (liftM Left ms0) (cleanup . either id snd)
  where
    step' (Left s) =
        case step s of
            Done () -> Done ()
            Skip s' -> Skip $ Left s'
            Yield a s' -> StepM $ liftM (Right . (, s')) (f a)
            StepM ms -> StepM $ liftM Left ms
    step' (Right (b, s)) = Yield b $ Left s
{-# INLINE mapMS #-}

enumFromTo :: (Ord a, Enum a, Monad m) => a -> a -> ConduitM i a m ()
enumFromTo x y = ConduitM (const $ enumFromToS x y)

enumFromToS :: (Ord a, Enum a, Monad m) => a -> a -> Stream m a ()
enumFromToS x y =
    Stream step (return x) (const $ return ())
  where
    step x
        | x > y = Done ()
        | otherwise = Yield x (succ x)

sinkList :: Monad m => ConduitM i o m [i]
sinkList = ConduitM sinkListS
{-# INLINE sinkList #-}

sinkListS :: Monad m => Stream m i () -> Stream m o [i]
sinkListS (Stream step ms0 cleanup) =
    Stream step' (liftM (, id) ms0) (cleanup . fst)
  where
    step' (s, front) =
        case step s of
            Done () -> Done (front [])
            Skip s' -> Skip (s', front)
            Yield i s' -> Skip (s', front . (i:))
            StepM ms -> StepM $ liftM (, front) ms
{-# INLINE sinkListS #-}

foldl' :: Monad m => (b -> a -> b) -> b -> ConduitM a o m b
foldl' f b = ConduitM (foldl'S f b)
{-# INLINE foldl' #-}

foldl'S :: Monad m => (b -> a -> b) -> b -> Stream m a () -> Stream m o b
foldl'S f b0 (Stream step ms0 cleanup) =
    Stream step' (liftM (, b0) ms0) (cleanup . fst)
  where
    step' (s, !b) =
        case step s of
            Done () -> Done b
            Skip s' -> Skip (s', b)
            Yield a s' -> Skip (s', f b a)
            StepM ms -> StepM $ liftM (, b) ms
{-# INLINE foldl'S #-}

foldM' :: Monad m => (b -> a -> m b) -> b -> ConduitM a o m b
foldM' f b = ConduitM (foldM'S f b)
{-# INLINE foldM' #-}

foldM'S :: Monad m => (b -> a -> m b) -> b -> Stream m a () -> Stream m o b
foldM'S f b0 (Stream step ms0 cleanup) =
    Stream step' (liftM (, b0) ms0) (cleanup . fst)
  where
    step' (s, !b) =
        case step s of
            Done () -> Done b
            Skip s' -> Skip (s', b)
            Yield a s' -> StepM $ do
                !b' <- f b a
                return (s', b')
            StepM ms -> StepM $ liftM (, b) ms
{-# INLINE foldM'S #-}
