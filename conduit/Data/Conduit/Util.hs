{-# LANGUAGE RankNTypes #-}
-- | Various utility functions versions of @conduit@.
module Data.Conduit.Util
    ( -- * Misc
      zip
    , zipSinks
    ) where

import Prelude hiding (zip)
import Control.Monad (liftM, liftM2)
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
        Nothing -> lift $ right $$ return ()
        Just (left', a) -> do
            mright <- lift $ draw right
            case mright of
                Nothing -> lift $ left' $$ return ()
                Just (right', b) -> do
                    yield (a, b)
                    zip left' right'

-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- Any leftovers are discarded.
--
-- Since 0.4.1
zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
zipSinks (Pipe x0) (Pipe y0) =
    Pipe $ \m -> go m (x0 m) (y0 m)
  where

    go m (Yield _ (Just o)) _ = absurd o
    go m _ (Yield _ (Just o)) = absurd o
    go m (Yield x Nothing) y = go m (x m) y
    go m x (Yield y Nothing) = go m x (y m)
    go m (Pure (PipeCont (Endpoint _ x) _)) (Pure (PipeCont (Endpoint _ y) _)) = Pure $ PipeCont (Endpoint [] (x, y)) m
    go _ (Pure PipeTerm{}) _ = error "zipSinks: PipeTerm"
    go _ _ (Pure PipeTerm{}) = error "zipSinks: PipeTerm"
    go m (M x) y = lift x >>= \x' -> go m x' y
    go m x (M y) = lift y >>= \y' -> go m x y'
    go m (Await x) (Await y) = do
        mi <- Await Pure
        go m (x mi) (y mi)
    go m (Await x) (Pure y) = do
        mi <- Await Pure
        go m (x mi) (Pure y)
    go m (Pure x) (Await y) = do
        mi <- Await Pure
        go m (Pure x) (y mi)
