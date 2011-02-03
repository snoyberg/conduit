module Timeout
    ( Manager
    , Handle
    , initialize
    , register
    , tickle
    ) where

import qualified Data.IORef as I
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

newtype Manager = Manager (I.IORef [Handle])
data Handle = Handle (IO ()) (I.IORef Bool)

initialize :: Int -> IO Manager
initialize timeout = do
    ref <- I.newIORef []
    _ <- forkIO $ forever $ do
        threadDelay timeout
        ms <- I.atomicModifyIORef ref (\x -> ([], x))
        ms' <- go ms id
        I.atomicModifyIORef ref (\x -> (ms' x, ()))
    return $ Manager ref
  where
    go [] front = return front
    go (m@(Handle onTimeout iactive):rest) front = do
        active <- I.atomicModifyIORef iactive (\x -> (False, x))
        if active
            then go rest (front . (:) m)
            else do
                onTimeout
                go rest front

register :: Manager -> IO () -> IO Handle
register (Manager ref) onTimeout = do
    iactive <- I.newIORef True
    let h = Handle onTimeout iactive
    I.atomicModifyIORef ref (\x -> (h : x, ()))
    return h

tickle :: Handle -> IO ()
tickle (Handle _ iactive) = I.writeIORef iactive True
