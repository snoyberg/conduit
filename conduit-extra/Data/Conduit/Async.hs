-- |
-- Simple conduits to do async operations
module Data.Conduit.Async
    (
      mapAsync
    ) where

import Conduit
import Control.Concurrent.Async (async, wait)

-- | Yield the given value only after awaiting a value from upstream
delayOneOf :: o -> ConduitT o o IO ()
delayOneOf v = do
    a <- await
    case a of
        Nothing -> do
            yield v
            return ()
        Just b -> do
            yield v
            delayOneOf b
-- | Delay a value by one yield acting as a buffer
delayOne :: ConduitT o o IO ()
delayOne = do
    a <- await
    case a of
        Nothing -> return ()
        Just b -> delayOneOf b

-- | Map `a` asynchronously to `b` using `op` and approximately `n` threads
-- The idea behind this approach is that it will set up a conduit which starts the async process,
-- passes along the handle `n` times using a delayed conduit and then awaits for the result.
mapAsync :: Int -> (a -> IO b) -> ConduitT a b IO ()
mapAsync n op = startAsync .| queue .| joinAsync
    where
        startAsync = mapMC (async . op)
        queue = foldl (.|) delayOne (replicate (n - 1) delayOne)
        joinAsync = mapMC wait
