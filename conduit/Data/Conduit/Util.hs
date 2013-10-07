{-# LANGUAGE RankNTypes #-}
-- | Various utility functions versions of @conduit@.
module Data.Conduit.Util
    ( -- * Misc
      zip
    , zipSinks
    ) where

import Prelude hiding (zip)
import Control.Monad.Trans.Class (lift)
import Data.Conduit (Source, Sink, ($$))
import Data.Conduit.Internal
import Data.Void (Void, absurd)

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 0.3.0
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip left right = do
    mleft <- lift $ draw left
    case mleft of
        (left', Nothing) -> lift $ do
            left' $$ return ()
            right $$ return ()
        (left', Just a) -> do
            mright <- lift $ draw right
            case mright of
                (right', Nothing) -> lift $ do
                    left' $$ return ()
                    right' $$ return ()
                (right', Just b) -> do
                    yield (a, b)
                    zip left' right'

-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- Any leftovers are discarded.
--
-- Since 0.4.1
zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Pipe i Void d t m (r, r')
zipSinks =
    go
  where
    go (Yield _ _ o) _ = absurd o
    go _ (Yield _ _ o) = absurd o
    go (Empty x) y = go (x [] ()) y
    go x (Empty y) = go x (y [] ())
    go (Pure _ x) (Pure _ y) = Pure [] (x, y)
    go Terminate{} _ = error "zipSinks: left termination"
    go _ Terminate{} = error "zipSinks: right termination"
    go (M x) y = lift x >>= \x' -> go x' y
    go x (M y) = lift y >>= \y' -> go x y'
    go (Await xm xd) (Await ym yd) = Await
        (\i -> go (xm i) (ym i))
        (go xd yd)
    go (Await xm xd) (Pure is y) = Await
        (\i -> go (xm i) (Pure is y))
        (go xd (Pure is y))
    go (Pure is x) (Await ym yd) = Await
        (\i -> go (Pure is x) (ym i))
        (go (Pure is x) yd)
    go (Check x _) y = go x y
    go x (Check y _) = go x y
