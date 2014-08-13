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
import Control.Monad.Trans.State.Strict

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

singletonStream :: Monad m => o -> r -> Stream m o r
singletonStream o r =
    Stream step (return False) (const $ return ())
  where
    step False = return $ Yield o True
    step True  = return $ Done r

newtype ConduitM i o m r = ConduitM
    { unConduitM :: forall x up. ConduitM' x up i o m r
    }

type ConduitM' x up i o m r
    = (up -> m (Step up i x))
   -> m up
   -> (up -> m ())
   -> Stream m o (Either (m up) x, r)

instance Monad m => Monad (ConduitM i o m) where
    return r = ConduitM $ \_ up _ -> emptyStream (Left up, r)
    ConduitM f >>= g = ConduitM $ \step up cleanup ->
        fixBind step cleanup g $ f step up cleanup
instance Monad m => Functor (ConduitM i o m) where
    fmap = liftM
instance Monad m => Applicative (ConduitM i o m) where
    pure = return
    (<*>) = ap

fixBind :: Monad m
        => (up -> m (Step up i x))
        -> (up -> m ())
        -> (a -> ConduitM i o m b)
        -> Stream m o (Either (m up) x, a)
        -> Stream m o (Either (m up) x, b)
fixBind stepU cleanupU g (Stream stepD msD cleanupD) =
    Stream step (liftM Left msD) cleanup
  where
    step (Left s) = do
        res <- stepD s
        return $ case res of
            Yield o s' -> Yield o (Left s')
            Skip s' -> Skip $ Left s'
            Done (Left mup, a) ->
                case g a of
                    ConduitM g' -> Skip $ Right $ g' stepU mup cleanupU
            Done (Right x, a) ->
                case g a of
                    ConduitM g' -> Skip $ Right $ g'
                        (const $ return $ Done x)
                        (return $ error "fixBind: value forced")
                        (const $ return ())

    step (Right (Stream step' mup cleanup)) = do
        res <- mup >>= step'
        return $ case res of
            Done x -> Done x
            Skip s' -> Skip $ Right $ Stream step' (return s') cleanup -- FIXME this is where slowness comes from...
            Yield o s' -> Yield o $ Right $ Stream step' (return s') cleanup

    cleanup (Left s) = cleanupD s
    cleanup (Right (Stream _ ms cleanup)) = ms >>= cleanup

(=$=) :: Monad m => ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r
ConduitM x =$= ConduitM y =
    ConduitM $ \step up cleanup ->
        case x step up cleanup of
            Stream step' up' cleanup' ->
                fmap collapseStream (y step' up' cleanup')
{-# INLINE (=$=) #-}

-- FIXME we need to be able to early terminate upstream
collapseStream :: Monad m
               => (Int, Either (m s) (Either (m up) x, ()))
               -> (Int, Either (m up) x)
collapseStream = error "collapseStream"
{-
collapseStream :: Monad m
               => Stream m b (Either (m up) x, ()) -- FIXME we need to be able to early terminate upstream
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
-}

yield :: Monad m => o -> ConduitM i o m ()
yield o = ConduitM $ \_ up _ -> singletonStream o (Left up, ())

await :: Monad m => ConduitM i o m (Maybe i)
await = ConduitM awaitS

awaitS :: Monad m => ConduitM' x up i o m (Maybe i)
awaitS step ms0 cleanup =
    Stream step' ms0 cleanup
  where
    step' s = do
        res <- step s
        return $ case res of
            Done x -> Done (Right x, Nothing)
            Skip s' -> Skip s'
            Yield i s' -> Done (Left $ return s', Just i)

runConduit :: Monad m => ConduitM () Void m r -> m r
runConduit (ConduitM f) =
    go $ f (const $ return $ Done ()) (return ()) (const $ return ())
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

mapS :: Monad m => (a -> b) -> ConduitM' x up a b m ()
mapS f step ms0 cleanup =
    Stream step' ms0 cleanup
  where
    step' s = do
        res <- step s
        return $ case res of
            Done x -> Done (Right x, ())
            Skip s' -> Skip s'
            Yield a s' -> Yield (f a) s'
{-# INLINE mapS #-}

mapM :: Monad m => (a -> m b) -> ConduitM a b m ()
mapM f = ConduitM (mapMS f)
{-# INLINE mapM #-}

mapMS :: Monad m => (a -> m b) -> ConduitM' x up a b m ()
mapMS f step ms0 cleanup =
    Stream step' ms0 cleanup
  where
    step' s = do
        res <- step s
        case res of
            Done x -> return $ Done (Right x, ())
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
            -> ConduitM' x up i a m ()
enumFromToS x y _ upstream _ =
    Stream (return . step) (return x) (const $ return ())
  where
    step x
        | x > y = Done (Left upstream, ())
        | otherwise = Yield x (succ x)

sinkList :: Monad m => ConduitM i o m [i]
sinkList = ConduitM sinkListS
{-# INLINE sinkList #-}

sinkListS :: Monad m => ConduitM' x up i o m [i]
sinkListS step ms0 cleanup =
    Stream step' (liftM (, id) ms0) (cleanup . fst)
  where
    step' (s, front) = do
        res <- step s
        return $ case res of
            Done x -> Done (Right x, front [])
            Skip s' -> Skip (s', front)
            Yield i s' -> Skip (s', front . (i:))
{-# INLINE sinkListS #-}

foldl' :: Monad m => (b -> a -> b) -> b -> ConduitM a o m b
foldl' f b = ConduitM (foldl'S f b)
{-# INLINE foldl' #-}

foldl'S :: Monad m => (b -> a -> b) -> b -> ConduitM' x up a o m b
foldl'S f b0 step ms0 cleanup =
    Stream step' (liftM (, b0) ms0) (cleanup . fst)
  where
    step' (s, !b) = do
        res <- step s
        return $ case res of
            Done x -> Done (Right x, b)
            Skip s' -> Skip (s', b)
            Yield a s' -> Skip (s', f b a)
{-# INLINE foldl'S #-}

foldM' :: Monad m => (b -> a -> m b) -> b -> ConduitM a o m b
foldM' f b = ConduitM (foldM'S f b)
{-# INLINE foldM' #-}

foldM'S :: Monad m => (b -> a -> m b) -> b -> ConduitM' x up a o m b
foldM'S f b0 step ms0 cleanup =
    Stream step' (liftM (, b0) ms0) (cleanup . fst)
  where
    step' (s, !b) = do
        res <- step s
        case res of
            Done x -> return $ Done (Right x, b)
            Skip s' -> return $ Skip (s', b)
            Yield a s' -> do
                !b' <- f b a
                return $ Skip (s', b')
{-# INLINE foldM'S #-}

take :: Monad m => Int -> ConduitM a a m ()
take i = ConduitM (takeS i)
{-# INLINE take #-}

takeS :: Monad m => Int -> ConduitM' x up a a m ()
takeS cnt0 step ms0 cleanup =
    Stream step' (liftM (, cnt0) ms0) (cleanup . fst)
  where
    step' (s, cnt)
        | cnt <= 0 = return $ Done (Left $ return s, ())
        | otherwise = do
            res <- step s
            case res of
                Done x -> return $ Done (Right x, ())
                Yield a s' -> return $ Yield a (s', cnt - 1)
                Skip s' -> return $ Skip (s', cnt)
{-# INLINE takeS #-}
