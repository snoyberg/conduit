{-# LANGUAGE RankNTypes #-}
-- | Adapter module to work with the foldl package.
module Data.Conduit.Extra.Foldl where

import qualified Control.Foldl as F
import Data.Conduit
import Control.Monad.Trans.Class (lift)
import qualified Data.Conduit.List as CL

-- | Convert a 'F.Fold' into a 'Consumer'.
--
-- Since 0.1.6
sinkFold :: Monad m => F.Fold a b -> Consumer a m b
sinkFold (F.Fold f seed extract) = fmap extract (CL.fold f seed)

-- | Convert a 'F.FoldM' into a 'Consumer'.
--
-- Since 0.1.6
sinkFoldM :: Monad m => F.FoldM m a b -> Consumer a m b
sinkFoldM (F.FoldM f mseed extract) =
    lift mseed >>= CL.foldM f >>= lift . extract
