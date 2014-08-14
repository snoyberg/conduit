{-# LANGUAGE GADTs #-}
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
import Unsafe.Coerce (unsafeCoerce)

data Step s o r
    = Done r
    | Skip s
    | Yield o s
    deriving Functor
data Stream m o r where
    Stream :: (s -> m (Step s o r))
           -> m s
           -> Stream m o r

instance Monad m => Functor (Stream m o) where
    fmap f (Stream step ms) = Stream (liftM (fmap f) . step) ms

runConduit :: Monad m => ConduitM () Void m r -> m r
runConduit (ConduitM ms0 step') =
    ms0 (return $ error "runConduit: initial state forced") >>= loop
  where
    step = step' (const $ return $ Done $ error $ "runConduit: final result forced")
    loop s = do
        res <- step s
        case res of
            Done (_, r) -> return r
            Skip s' -> loop s'
            Yield o _ -> absurd o
{-# INLINE runConduit #-}

(=$=) :: Monad m
      => ConduitM a b m ()
      -> ConduitM b c m r
      -> ConduitM a c m r
left =$= right =
    ConduitM state step
  where
    (stateL, stepL) = toNoBind left
    (stateR, stepR) = toNoBind right
    state = stateR . unsafeCoerce . stateL

    step step' = helper stepR stepL step' -- unsafeCoerce $ stepR (unsafeCoerce $ liftM (fmap snd) . stepL step')
{-# INLINE (=$=) #-}

helper :: Monad m
       => ((s0 -> m (Step s0 b ())) -> s1 -> m (Step s1 c (Either (m s0) (),  r)))
       -> ((s1 -> m (Step s1 a ())) -> s0 -> m (Step s0 b (Either (m s1) (), ())))
       -> ((s1 -> m (Step s1 a ())) -> s1 -> m (Step s1 c (Either (m s1) (),  r)))
helper stepR stepL stepU sR =
    error "helper" -- stepR (\sL -> stepL stepU sL) sR

toNoBind :: Monad m
         => ConduitM i o m r
         -> (m s -> m t,
           (s -> m (Step s i ()))
            -> t
            -> m (Step t o (Either (m s) (), r)))
toNoBind (ConduitM x y) = unsafeCoerce (x, y)
--toNoBind (ConduitBind f) = toNoBind $ f return

toBind :: ConduitM i o m r
       -> ((r -> ConduitM i o m b) -> ConduitM i o m b)
toBind (ConduitM state step) = error "toBind"
--toBind (ConduitBind f) = f

data ConduitM i o m r =
    forall s t. ConduitM
        (m s -> m t)
        ((s -> m (Step s i ()))
            -> t
            -> m (Step t o (Either (m s) (), r)))

    {-
    | forall s. ConduitBind
        (m s -> m t)
        (forall b.
            (s -> m (Step s i ()))
         -> (Either (m s) (), r)
         -> 
            ((r -> ConduitM i o m b) -> ConduitM i o m b))
        -}

newtype Cont s i m o b = Cont
    { unCont :: (s -> m (Step s i ()))
             -> m (Step (Cont s i m o b) o (Either (m s) (), b))
    }

instance Monad m => Monad (ConduitM i o m) where
    return r = ConduitM return $ \_ ms -> return $ Done (Left ms, r)
    ConduitM stateL stepL >>= f =
        ConduitM state step'
      where
        state ms = do
            t <- stateL ms
            return $ go stepHelperFull t

        go stepUpHelper t = Cont $ \stepUp' -> do
            res <- stepL (stepUpHelper stepUp') t
            case res of
                Skip t' -> return $ Skip $ go stepUpHelper t'
                Yield o t' -> return $ Yield o $ go stepUpHelper t'
                Done (ems, r) ->
                    case f r of
                        ConduitM stateR stepR ->
                            case ems of
                                Left ms -> do
                                    t' <- stateR $ liftM unsafeCoerce ms
                                    return $ Skip $ go2 (unsafeCoerce stepR) stepUpHelper t'
                                Right () -> do
                                    t' <- stateR $ return $ error ">>=: state value forced"
                                    return $ Skip $ go2 stepR stepHelperEmpty t'

        go2 stepR stepUpHelper t = Cont $ \stepUp' -> do
            res <- stepR (stepUpHelper stepUp') t
            return $ case res of
                Skip t' -> Skip $ go2 stepR stepUpHelper t'
                Yield o t' -> Yield o $ go2 stepR stepUpHelper t'
                Done x -> Done $ unsafeCoerce x

        step' step f = unCont f step

        stepHelperFull step s = do
            res <- step s
            return $ case res of
                Done x -> Done x
                Skip s' -> Skip s'
                Yield i s' -> Yield i s'

        stepHelperEmpty _ _ = return $ Done ()

    --f >>= g = ConduitBind $ \c -> toBind f (\a -> toBind (g a) c)
    {-
    ConduitM f >>= g = ConduitM $ \up ->
        fixBind g $ f up
        -}
instance Monad m => Functor (ConduitM i o m) where
    fmap = liftM
instance Monad m => Applicative (ConduitM i o m) where
    pure = return
    (<*>) = ap

yield :: Monad m => o -> ConduitM i o m ()
yield o =
    ConduitM (return . (, False)) step
  where
    step _ (s, False) = return $ Yield o (s, True)
    step _ (s, True) = return $ Done (Left s, ())
{-# INLINE yield #-}

await :: Monad m => ConduitM i o m (Maybe i)
await =
    ConduitM id step'
  where
    step' step s = do
        res <- step s
        return $ case res of
            Done x -> Done (Right x, Nothing)
            Skip s' -> Skip s'
            Yield i s' -> Done (Left (return s'), Just i)
{-# INLINE await #-}

map :: Monad m => (a -> b) -> ConduitM a b m ()
map f =
    ConduitM id step'
  where
    step' step s = do
        res <- step s
        return $ case res of
            Done x -> Done (Right x, ())
            Skip s' -> Skip s'
            Yield a s' -> Yield (f a) s'

{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> ConduitM a b m ()
mapM f =
    ConduitM id step'
  where
    step' step s = do
        res <- step s
        case res of
            Done x -> return $ Done (Right x, ())
            Skip s' -> return $ Skip s'
            Yield a s' -> do
                b <- f a
                return $ Yield b s'
{-# INLINE mapM #-}

enumFromTo :: (Ord a, Enum a, Monad m) => a -> a -> ConduitM i a m ()
enumFromTo x0 y =
    ConduitM (return . (, x0)) step
  where
    step _ (s, x)
        | x > y = return $ Done (Left s, ())
        | otherwise = return $ Yield x (s, succ x)
{-# INLINE enumFromTo #-}

sinkList :: Monad m => ConduitM i o m [i]
sinkList =
    ConduitM (liftM (, id)) step'
  where
    step' step (s, front) = do
        res <- step s
        return $ case res of
            Done x -> Done (Right x, front [])
            Skip s' -> Skip (s', front)
            Yield i s' -> Skip (s', front . (i:))
{-# INLINE sinkList #-}

foldl' :: Monad m => (b -> a -> b) -> b -> ConduitM a o m b
foldl' f b0 =
    ConduitM (liftM (, b0)) step'
  where
    step' step (s, !b) = do
        res <- step s
        return $ case res of
            Done x -> Done (Right x, b)
            Skip s' -> Skip (s', b)
            Yield a s' -> Skip (s', f b a)
{-# INLINE foldl' #-}

foldM' :: Monad m => (b -> a -> m b) -> b -> ConduitM a o m b
foldM' f b0 =
    ConduitM (liftM (, b0)) step'
  where
    step' step (s, !b) = do
        res <- step s
        case res of
            Done x -> return $ Done (Right x, b)
            Skip s' -> return $ Skip (s', b)
            Yield a s' -> do
                !b' <- f b a
                return $ Skip (s', b')
{-# INLINE foldM' #-}

take :: Monad m => Int -> ConduitM a a m ()
take cnt0 =
    ConduitM (liftM (, cnt0)) step'
  where
    step' step (s, cnt)
        | cnt <= 0 = return $ Done (Left $ return s, ())
        | otherwise = do
            res <- step s
            return $ case res of
                Done x -> Done (Right x, ())
                Yield a s' -> Yield a (s', cnt - 1)
                Skip s' -> Skip (s', cnt)
{-# INLINE take #-}
