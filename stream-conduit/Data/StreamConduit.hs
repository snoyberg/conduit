{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Data.StreamConduit where

import Prelude hiding (map, enumFromTo, mapM, take)
import Data.Void (Void, absurd)
import Control.Monad (liftM, ap)
import Control.Arrow (first)
import Control.Applicative (Applicative (..))

data Step s m o r
    = Done r
    | Skip s
    | Yield o s
    | Switch (Stream m o r)
    deriving Functor
data Stream m o r = forall s. Stream
    (s -> m (Step s m o r))
    (m s)
    (s -> m ()) -- cleanup

instance Monad m => Functor (Stream m o) where
    fmap f (Stream step ms cleanup) = Stream (liftM (fmap f) . step) ms cleanup

emptyStream :: Monad m => r -> Stream m o r
emptyStream r = Stream (const $ return $ Done r) (return ()) (const $ return ())

singletonStream :: Monad m => o -> r -> Stream m o r
singletonStream o r =
    Stream step (return False) (const $ return ())
  where
    step False = return $ Yield o True
    step True  = return $ Done r

newtype ConduitM i o m r = ConduitM
    { unConduitM :: forall x. Stream m i x -> Stream m o (Stream m i x, r)
    }

instance Monad m => Monad (ConduitM i o m) where
    return r = ConduitM $ \up -> emptyStream (up, r)
    ConduitM f >>= g = ConduitM $ \up ->
        fixBind g $ f up
instance Monad m => Functor (ConduitM i o m) where
    fmap = liftM
instance Monad m => Applicative (ConduitM i o m) where
    pure = return
    (<*>) = ap

fixBind :: Monad m
        => (a -> ConduitM i o m b)
        -> Stream m o (Stream m i x, a)
        -> Stream m o (Stream m i x, b)
fixBind g (Stream step1 ms1 cleanup) =
    Stream step ms1 cleanup
  where
    step s = do
        res <- step1 s
        case res of
            Yield o s' -> return $ Yield o s'
            Skip s' -> return $ Skip s'
            Done (up, a) ->
                case g a of
                    ConduitM g' -> return $ Switch $ g' up
            Switch stream -> return $ Switch $ fixBind g stream

(=$=) :: Monad m => ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r
ConduitM x =$= ConduitM y = ConduitM (fmap (first collapseStream) . y . x)
{-# INLINE (=$=) #-}

yield :: Monad m => o -> ConduitM i o m ()
yield o = ConduitM $ \up -> singletonStream o (up, ())

await :: Monad m => ConduitM i o m (Maybe i)
await = ConduitM awaitS

awaitS :: Monad m => Stream m i x -> Stream m o (Stream m i x, Maybe i)
awaitS (Stream step ms0 cleanup) =
    Stream step' ms0 cleanup
  where
    step' s = do
        res <- step s
        return $ case res of
            Done x -> Done (emptyStream x, Nothing)
            Skip s' -> Skip s'
            Yield i s' -> Done (Stream step (return s') cleanup, Just i)
            Switch stream -> Switch $ awaitS stream

collapseStream :: Monad m
               => Stream m b (Stream m a x, ()) -- FIXME we need to be able to early terminate upstream
               -> Stream m a x
collapseStream (Stream step ms0 cleanup) =
    Stream step' ms (\(Stream _ s f) -> s >>= f)
  where
    step' (Stream f s cleanup) = do
        res <- s >>= f
        return $ case res of
            Done r -> Done r
            Skip s' -> Skip $ Stream f (return s') cleanup
            Yield a s' -> Yield a $ Stream f (return s') cleanup
    ms = do
        s <- ms0
        (str, ()) <- exhaust s
        return str

    exhaust s = do
        res <- step s
        case res of
            Done x -> return x

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
                Switch stream -> go stream
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
            Switch stream -> Switch $ mapS f stream
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
sinkList = ConduitM (sinkListS id)
{-# INLINE sinkList #-}

sinkListS :: Monad m => ([i] -> [i]) -> Stream m i x -> Stream m o (Stream m i x, [i])
sinkListS front (Stream step ms0 cleanup) =
    Stream step' (liftM (, front) ms0) (cleanup . fst)
  where
    step' (s, front) = do
        res <- step s
        return $ case res of
            Done x -> Done (emptyStream x, front [])
            Skip s' -> Skip (s', front)
            Yield i s' -> Skip (s', front . (i:))
            Switch stream -> Switch $ sinkListS front stream
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
            Switch stream -> Switch $ foldl'S f b stream
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
{-# INLINE takeS #-}
