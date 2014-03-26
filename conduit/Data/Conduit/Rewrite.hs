{-# LANGUAGE RankNTypes #-}
-- | Internal helper functions, usually used for rewrite rules.
module Data.Conduit.Rewrite where

import Data.Conduit hiding (($$))
import qualified Data.Conduit as C
import Data.Conduit.Internal (ConduitM (..), Pipe (..), injectLeftovers)
import Data.Void (absurd, Void)
import Control.Monad.Trans.Class (lift)
import Control.Monad (replicateM_, forever)

($$) :: Monad m => ConduitM () a m () -> ConduitM a Void m r -> m r
($$) = (C.$$)
{-# INLINE [1] ($$) #-}

-- | Acquire the seed value and perform the given action with it n times,
-- yielding each result.
--
-- Since 0.2.1
initReplicate :: Monad m => m seed -> (seed -> m a) -> Int -> Producer m a
initReplicate mseed f cnt = do
    seed <- lift mseed
    replicateM_ cnt (lift (f seed) >>= yield)
{-# INLINE [1] initReplicate #-}

-- | Optimized version of initReplicate for the special case of connecting with
-- a @Sink@.
--
-- Since 0.2.1
initReplicateConnect :: Monad m
                     => m seed
                     -> (seed -> m a)
                     -> Int
                     -> Sink a m b
                     -> m b
initReplicateConnect mseed f cnt0 (ConduitM sink0) = do
    seed <- mseed
    let loop cnt sink | cnt <= 0 = finish sink
        loop _ (Done r) = return r
        loop cnt (NeedInput p _) = f seed >>= loop (pred cnt) . p
        loop _ (HaveOutput _ _ o) = absurd o
        loop cnt (PipeM mp) = mp >>= loop cnt
        loop _ (Leftover _ i) = absurd i

    loop cnt0 (injectLeftovers sink0)
  where
    finish (Done r) = return r
    finish (HaveOutput _ _ o) = absurd o
    finish (NeedInput _ p) = finish (p ())
    finish (PipeM mp) = mp >>= finish
    finish (Leftover _ i) = absurd i
{-# INLINE initReplicateConnect #-}
{-# RULES "initReplicateConnect" forall mseed f cnt sink.
    initReplicate mseed f cnt $$ sink
    = initReplicateConnect mseed f cnt sink
  #-}

-- | Acquire the seed value and perform the given action with it forever,
-- yielding each result.
--
-- Since 0.2.1
initRepeat :: Monad m => m seed -> (seed -> m a) -> Producer m a
initRepeat mseed f = do
    seed <- lift mseed
    forever $ lift (f seed) >>= yield

-- | Optimized version of initRepeat for the special case of connecting with
-- a @Sink@.
--
-- Since 0.2.1
initRepeatConnect :: Monad m
                  => m seed
                  -> (seed -> m a)
                  -> Sink a m b
                  -> m b
initRepeatConnect mseed f (ConduitM sink0) = do
    seed <- mseed
    let loop (Done r) = return r
        loop (NeedInput p _) = f seed >>= loop . p
        loop (HaveOutput _ _ o) = absurd o
        loop (PipeM mp) = mp >>= loop
        loop (Leftover _ i) = absurd i

    loop (injectLeftovers sink0)
{-# RULES "initRepeatConnect" forall mseed f sink.
    initRepeat mseed f $$ sink
    = initRepeatConnect mseed f sink
  #-}

foldRewrite
    :: Monad m
    => (b -> a -> b)
    -> b
    -> Consumer a m b
foldRewrite f =
    ConduitM . loop
  where
    loop b =
        NeedInput go (\() -> Done b)
      where
        go a =
            let b' = f b a
             in b' `seq` loop b'
{-# INLINE [1] foldRewrite #-}

foldMRewrite
    :: Monad m
    => (b -> a -> m b)
    -> m b
    -> Consumer a m b
foldMRewrite f mb =
    ConduitM $ PipeM (mb >>= return . loop)
  where
    loop b =
        NeedInput go (\() -> Done b)
      where
        go a = PipeM $ do
            b' <- f b a
            b' `seq` return (loop b')
{-# INLINE [1] foldMRewrite #-}

initReplicateFold
    :: Monad m
    => m seed
    -> (seed -> m a)
    -> Int
    -> (b -> a -> b)
    -> b
    -> m b
initReplicateFold mseed f cnt0 g b0 = do
    seed <- mseed
    let loop b cnt
            | cnt <= 0 = return b
            | otherwise = do
                a <- f seed
                let b' = g b a
                b' `seq` loop b' (cnt - 1)
    loop b0 cnt0
{-# INLINE initReplicateFold #-}
{-# RULES "initReplicateFold" forall mseed f cnt g b.
    initReplicate mseed f cnt $$ foldRewrite g b
    = initReplicateFold mseed f cnt g b
  #-}

initReplicateFoldM
    :: Monad m
    => m seed
    -> (seed -> m a)
    -> Int
    -> (b -> a -> m b)
    -> m b
    -> m b
initReplicateFoldM mseed f cnt0 g mb = do
    b0 <- mb
    seed <- mseed
    let loop b cnt
            | cnt <= 0 = return b
            | otherwise = do
                a <- f seed
                b' <- g b a
                b' `seq` loop b' (cnt - 1)
    loop b0 cnt0
{-# INLINE initReplicateFoldM #-}
{-# RULES "initReplicateFoldM" forall mseed f cnt g b.
    initReplicate mseed f cnt $$ foldMRewrite g b
    = initReplicateFoldM mseed f cnt g b
  #-}

unfoldM
    :: Monad m
    => m seed
    -> (forall x. seed -> m x -> (a -> seed -> m x) -> m x)
    -> Producer m a
unfoldM mseed f =
    ConduitM (PipeM (mseed >>= loop))
  where
    loop seed = f
        seed
        (return (Done ()))
        (\a seed' -> return (HaveOutput (PipeM (loop seed')) (return ()) a))
{-# INLINE [0] unfoldM #-}

unfoldMFoldM
    :: Monad m
    => m seed
    -> (forall x. seed -> m x -> (a -> seed -> m x) -> m x)
    -> (b -> a -> m b)
    -> m b
    -> m b
unfoldMFoldM mseed f g mb = do
    mb >>= \b0 -> mseed >>= loop b0
  where
    loop b seed = f
        seed
        (return b)
        (\a seed' -> do
            b' <- g b a
            b' `seq` loop b' seed')
{-# INLINE unfoldMFoldM #-}
{-# RULES "unfoldMFoldM" forall mseed (f :: forall x. seed -> m x -> (a -> seed -> m x) -> m x) g b.
    unfoldM mseed f $$ foldMRewrite g b
    = unfoldMFoldM mseed f g b
  #-}
