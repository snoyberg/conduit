-- |
-- Simple conduits to do async operations
module Data.Conduit.Async
    (
      mapAsync
    ) where

import Conduit
-- import qualified Data.Conduit.List as CL
import Control.Concurrent.Async (async, wait)

-- | Map `a` asynchronously to `b` using batches of at most `n` elements using `mapConcurrently`
-- The idea behind this approach is that it will set up a conduit which starts the async process,
-- passes along the handle `n` times and then awaits for the result.
mapAsync :: Int -> (a -> IO b) -> ConduitT a b IO ()
mapAsync n op = startAsync .| queue .| joinAsync
    where
        nop = awaitForever yield
        startAsync = mapMC (\x -> async (op x))
        queue = foldl (.|) nop (replicate n nop)
        joinAsync = mapMC wait
