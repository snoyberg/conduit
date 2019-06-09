-- |
-- Simple conduits to do async operations
module Data.Conduit.Binary
    (
      mapAsyncIO
    ) where

import Conduit
import qualified Data.Conduit.List as CL
import Control.Concurrent.Async (mapConcurrently)

-- | Map `a` asynchronously to `b` using batches of at most `n` elements using `mapConcurrently`
mapAsync :: Int -> (a -> IO b) -> ConduitT a b IO ()
mapAsync n op = CL.chunksOf n .| mapAsynced .| CL.concat
    where
        mapAsynced = mapMC (mapConcurrently op)
