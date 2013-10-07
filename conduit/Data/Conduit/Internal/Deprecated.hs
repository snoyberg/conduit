module Data.Conduit.Internal.Deprecated
    ( mapOutput
    , mapOutputMaybe
    , mapInput
    , sourceList
    ) where

import Data.Conduit.Internal.Pipe
import Data.Conduit.Internal.Primitives
import Data.Conduit.Internal.Composition
import Data.Maybe (mapMaybe)
import Data.Void

haltPipe :: Monad m => Pipe i o d t m d
haltPipe = closeDownstream >>= return . snd

-- | Apply a function to all the output values of a @ConduitM@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days.
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> Pipe i o1 d t m r -> Pipe i o2 d t m r
mapOutput f = (>+> mapPipe f)
{-# DEPRECATED mapOutput "Just use normal fusion" #-}

mapPipe :: Monad m => (a -> b) -> Pipe a b r t m r
mapPipe f =
    go
  where
    go = await >>= maybe haltPipe (\x -> tryYield (f x) >>= maybe go (return . snd))

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> Pipe i o1 d t m r -> Pipe i o2 d t m r
mapOutputMaybe f = (>+> mapMaybePipe f)
{-# DEPRECATED mapOutputMaybe "Just use normal fusion" #-}

mapMaybePipe :: Monad m => (a -> Maybe b) -> Pipe a b r t m r
mapMaybePipe f =
    go
  where
    go = await >>= maybe haltPipe (maybe go (\x -> tryYield x >>= maybe go (return . snd)) . f)

-- | Apply a function to all the input values of a @ConduitM@.
--
-- Since 0.5.0
mapInput :: Monad m
         => (i1 -> i2) -- ^ map initial input to new input
         -> (i2 -> Maybe i1) -- ^ map new leftovers to initial leftovers
         -> Pipe i2 o d Void m r
         -> Pipe i1 o d t m r
mapInput f g = pipe absurd id (const Pure) (const Pure) (mapLeftoverPipe f g)
{-# DEPRECATED mapInput "Just use normal fusion" #-}

mapLeftoverPipe :: Monad m => (a -> b) -> (b -> Maybe a) -> Pipe a b r t m r
mapLeftoverPipe f g =
    go
  where
    go = await >>= maybe haltPipe (\x -> tryYield (f x) >>= maybe go done)

    done (bs, result) = Pure (mapMaybe g bs) result

-- | Convert a list into a source.
--
-- Since 0.3.0
sourceList :: Monad m => [a] -> Pipe i a d t m ()
sourceList [] = return ()
sourceList (a:as) = tryYield a >>= maybe (sourceList as) (const $ return ())
{-# DEPRECATED sourceList "Just use mapM_ yield" #-}
