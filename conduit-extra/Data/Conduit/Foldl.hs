{-# LANGUAGE RankNTypes #-}
-- | Adapter module to work with the <https://hackage.haskell.org/package/foldl foldl> package.
--
-- @since 1.1.16
module Data.Conduit.Foldl where

import Data.Conduit
import Control.Monad.Trans.Class (lift)
import qualified Data.Conduit.List as CL

-- | Convert a left fold into a 'Consumer'. This function is intended
-- to be used with @purely@ from the
-- <https://hackage.haskell.org/package/foldl foldl> package.
--
-- @since 1.1.16
sinkFold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> ConduitT a o m b
sinkFold combine seed extract = fmap extract (CL.fold combine seed)

-- | Convert a monadic left fold into a 'Consumer'. This function is
-- intended to be used with @impurely@ from the
-- <https://hackage.haskell.org/package/foldl foldl> package.
--
-- @since 1.1.16
sinkFoldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> ConduitT a o m b
sinkFoldM combine seed extract =
  lift . extract =<< CL.foldM combine =<< lift seed
