-- http://www.cs.indiana.edu/~sabry/papers/yield-pp.pdf
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Gen where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

type GenT e m = ReaderT (e -> m ()) m
type Producer m e = GenT e m ()
type Consumer m e = e -> m ()
type Transducer m1 m2 e1 e2 = Producer m1 e1 -> Producer m2 e2

yield :: Monad m => e -> Producer m e
yield e = ReaderT $ \f -> f e
{-# INLINE yield #-}

enumFromToG :: (Ord e, Enum e, Monad m) => e -> e -> Producer m e
enumFromToG x0 y = ReaderT $ \f ->
    let loop x
            | x > y = return ()
            | otherwise = f x >> loop (succ x)
     in loop x0
{-# INLINE enumFromToG #-}

foldG :: Monad m => (s -> e -> m s) -> s -> Producer (StateT s m) e -> m s
foldG f s0 p =
    execStateT (runReaderT p fs) s0
  where
    fs e = do
        !s <- get
        !s' <- lift $ f s e
        put s'
{-# INLINE foldG #-}

mapG :: Monad m => (e1 -> e2) -> Transducer (GenT e2 m) m e1 e2
mapG f gen = runReaderT gen (yield . f)
{-# INLINE mapG #-}
