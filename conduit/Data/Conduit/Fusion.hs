{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Conduit.Fusion
    ( -- * Producers
      SourceStream (..)
    , sourceStream
      -- * Consumers
    , sinkFold
    , sinkFoldM
    ) where

import Data.Conduit.Internal.Conduit
import Control.Monad.Trans.Class (lift)

data SourceStream a = forall s. SourceStream
    (forall r. s
        -> r -- ^ end of stream
        -> (a -> s -> r) -- ^ new element and new state
        -> (a -> r) -- ^ final element
        -> r)
    s

sourceStream :: Monad m
             => SourceStream a
             -> Producer m a
sourceStream (SourceStream f s0) =
    go s0
  where
    go s = f s (return ()) (\a s' -> yield a >> go s') yield
{-# INLINE [0] sourceStream #-}

sinkFold
    :: Monad m
    => (s -> a -> s)
    -> s
    -> (s -> r)
    -> Consumer a m r
sinkFold f s0 final =
    loop s0
  where
    loop !s = await >>= maybe (return $! final s) (loop . f s)
{-# INLINE [0] sinkFold #-}

sinkFoldM
    :: Monad m
    => (s -> a -> m s)
    -> m s
    -> (s -> m r)
    -> Consumer a m r
sinkFoldM f s0 final =
    lift s0 >>= loop
  where
    loop !s = await >>= maybe (lift $ final s) (\a -> lift (f s a) >>= loop)
{-# INLINE [0] sinkFoldM #-}

-- Fusion

-- sourceStream + sinkFold
sourceStreamFold :: Monad m
                 => SourceStream a
                 -> (s -> a -> s)
                 -> s
                 -> (s -> b)
                 -> m b
sourceStreamFold (SourceStream step s0) f b0 g =
    go s0 b0
  where
    go s !b = step s
        (return $! g b)
        (\a s' -> go s' (f b a))
        (\a -> return $! g (f b a))
{-# INLINE sourceStreamFold #-}
{-# RULES "sourceStreamFold" forall s f b g.
        sourceStream s $$ sinkFold f b g = sourceStreamFold s f b g
  #-}

-- sourceStream + sinkFoldM
sourceStreamFoldM :: Monad m
                  => SourceStream a
                  -> (s -> a -> m s)
                  -> m s
                  -> (s -> m b)
                  -> m b
sourceStreamFoldM (SourceStream step s0) f b0 g =
    b0 >>= go s0
  where
    go s !b = step s
        (g b)
        (\a s' -> f b a >>= go s')
        (\a -> f b a >>= g)
{-# INLINE sourceStreamFoldM #-}
{-# RULES "sourceStreamFoldM" forall s f b g.
        sourceStream s $$ sinkFoldM f b g = sourceStreamFoldM s f b g
  #-}
