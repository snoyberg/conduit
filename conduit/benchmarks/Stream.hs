{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Stream where

import Control.Monad (liftM)
import Data.Conduit
import qualified Data.Conduit.Internal as CI

data Step s a
    = Done
    | Skip !s
    | Yield !a !s
    deriving Functor

data Stream m a = forall s. Stream (s -> m (Step s a)) !s

enumFromToS :: (Ord a, Enum a, Monad m) => a -> a -> Stream m a
enumFromToS x0 y =
    Stream go x0
  where
    go x
        | x > y = return Done
        | otherwise = return (Yield x (succ x))
{-# INLINE enumFromToS #-}

mapS :: Monad m => (a -> b) -> Stream m a -> Stream m b
mapS f (Stream step s0) = Stream (liftM (fmap f) . step) s0
{-# INLINE mapS #-}

foldS :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldS f b0 (Stream step s0) =
    go b0 s0
  where
    go !b !s = do
        res <- step s
        case res of
            Done -> return $! b
            Skip s' -> go b s'
            Yield a s' -> go (f b a) s'
{-# INLINE foldS #-}

conduitToStream :: Monad m => ConduitM i o m () -> Stream m i -> Stream m o
conduitToStream (CI.ConduitM con0) (Stream src s0) =
    Stream step (con0, Just s0, [], return ())
  where
    step (con1, ms, ls, f) =
        go con1
      where
        go (CI.HaveOutput con f' o) = return $ Yield o (con, ms, ls, f')
        go (CI.NeedInput more done) =
            case ls of
                l:ls' -> return $ Skip (more l, ms, ls', f)
                [] -> do
                    case ms of
                        Nothing -> return $ Skip (done (), Nothing, [], f)
                        Just s -> do
                            res <- src s
                            return $ Skip $ case res of
                                Done -> (done (), Nothing, [], return ())
                                Skip s' -> (con1, Just s', ls, f)
                                Yield i s' -> (more i, Just s', [], f)
        go (CI.Done ()) = return Done
{-# INLINE conduitToStream #-}

toList :: Monad m => Stream m a -> m [a]
toList (Stream step s0) =
    loop s0 id
  where
    loop s front = do
        res <- step s
        case res of
            Done -> return $! front []
            Skip s' -> loop s' front
            Yield a s' -> loop s' $ front . (a:)
