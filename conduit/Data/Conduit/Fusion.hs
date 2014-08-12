{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Conduit.Fusion
    ( -- * Producers
      SourceStream (..)
    , sourceStream
      -- * Consumers
    , sinkFold
    ) where

import Data.Conduit.Internal.Conduit

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

-- Fusion
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
