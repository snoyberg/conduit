{-# LANGUAGE ExistentialQuantification #-}
module Data.Conduit.Fusion where

import Data.Conduit.Internal
import Data.Void
import Control.Monad (liftM)

data Step s a r
    = Yield a s
    | Skip s
    | DoneS r

data Stream m a r = forall s. Stream (s -> m (Step s a r)) s

type Stream' m a b r = Stream m a () -> Stream m b r

nullStream :: Monad m => Stream m () ()
nullStream = Stream (return . DoneS) ()

connect :: Monad m => Stream' m () a () -> Stream' m a Void r -> m r
connect src sink =
    case sink (src nullStream) of
        Stream f s0 -> do
            let loop s = do
                    step <- f s
                    case step of
                        Yield a _ -> absurd a
                        DoneS r -> return r
            loop s0
{-# INLINE connect #-}

enumFromTo :: (Ord a, Enum a, Monad m) => a -> a -> Stream' m i a ()
enumFromTo curr0 final _ =
    Stream go curr0
  where
    go curr
        | curr > final = return (DoneS ())
        | otherwise = return (Yield curr (succ curr))
{-# INLINE enumFromTo #-}

foldM :: Monad m => (b -> a -> m b) -> b -> Stream' m a o b
foldM f b0 (Stream src seed0) =
    flip Stream () $ const $ DoneS `liftM` loop b0 seed0
  where
    loop b seed = do
        step <- src seed
        case step of
            Yield a seed' -> do
                b' <- f b a
                loop b' seed'
            DoneS () -> return b
{-# INLINE foldM #-}

replicateM_ :: Monad m => Int -> m o -> Stream' m i o ()
replicateM_ cnt0 m _ =
    Stream go cnt0
  where
    go 0 = return $ DoneS ()
    go cnt = do
        a <- m
        return (Yield a (cnt - 1))
{-# INLINE replicateM_ #-}

fold :: Monad m => (b -> a -> b) -> b -> Stream' m a o b
fold f b0 (Stream src seed0) =
    flip Stream () $ const $ DoneS `liftM` loop b0 seed0
  where
    loop b seed = do
        step <- src seed
        case step of
            Yield a seed' -> loop (f b a) seed'
            DoneS () -> return b
{-# INLINE fold #-}
