import Data.Conduit
import Data.Conduit.Internal (ConduitM (..), Pipe (..))
import qualified Data.Conduit.Internal as CI
import Control.Monad.Trans.Class
import Criterion.Main
import Data.IORef
import Control.Monad (replicateM_)
import Data.Conduit.List (sinkNull)
import Control.Monad (liftM, (>=>))
import Control.Monad.Codensity

start, count :: Int
start = 0
count = 10000

myReplicateM_ :: Monad m => Int -> m () -> m ()
myReplicateM_ i0 m
    | i0 < 1 = return ()
    | otherwise = loop i0
  where
    loop 0 = return ()
    loop i = m >> loop (pred i)
{-# INLINE myReplicateM_ #-}

main :: IO ()
main = defaultMain
    [ bench "ConduitM, high level" $ whnfIO $ do
        ref <- newIORef start
        replicateM_ count (lift (modifyIORef' ref succ) >>= yield)
            $$ sinkNull
        readIORef ref
    , bench "ConduitM, high level, local replicate" $ whnfIO $ do
        ref <- newIORef start
        myReplicateM_ count (lift (modifyIORef' ref succ) >>= yield)
            $$ sinkNull
        readIORef ref
    , bench "Pipe, high level" $ whnfIO $ do
        ref <- newIORef start
        ConduitM (replicateM_ count (lift (modifyIORef' ref succ) >>= CI.yield))
            $$ sinkNull
        readIORef ref
    , bench "Pipe, high level, unroll replicate, left" $ whnfIO $ do
        ref <- newIORef start
        let loop 0 = return ()
            loop i = do
                (lift (modifyIORef' ref succ) >>= CI.yield) >>= (const (loop (pred i)))
        ConduitM (loop (1000 :: Int))
            $$ sinkNull
        readIORef ref
    , bench "Conduit, unroll replicate, left" $ whnfIO $ do
        ref <- newIORef start
        let loop 0 = return ()
            loop i = do
                ((lift $ modifyIORef' ref succ) >>= yield) >> (loop (pred i))
        loop (1000 :: Int) $$ sinkNull
        readIORef ref
    , bench "Conduit Codensity, unroll replicate, left" $ whnfIO $ do
        ref <- newIORef start
        let loop 0 = return ()
            loop i = do
                (lift (lift $ modifyIORef' ref succ) >>= yield) >> (loop (pred i))
        lowerCodensity (loop (1000 :: Int) $$ sinkNull)
        readIORef ref
    , bench "Conduit Codensity, unroll replicate, right" $ whnfIO $ do
        ref <- newIORef start
        let loop 0 = return ()
            loop i = do
                lift (lift $ modifyIORef' ref succ) >>= (\x -> yield x >> (loop (pred i)))
        lowerCodensity (loop (1000 :: Int) $$ sinkNull)
        readIORef ref
    , bench "Conduit, unroll replicate, right" $ whnfIO $ do
        ref <- newIORef start
        let loop 0 = return ()
            loop i = do
                lift (modifyIORef' ref succ) >>= (\x -> yield x >> (loop (pred i)))
        (loop (1000 :: Int)) $$ sinkNull
        readIORef ref
    , bench "Pipe, high level, unroll replicate, right" $ whnfIO $ do
        ref <- newIORef start
        let loop 0 = return ()
            loop i = do
                lift (modifyIORef' ref succ) >>= (\x -> CI.yield x >> (loop (pred i)))
        ConduitM (loop (1000 :: Int))
            $$ sinkNull
        readIORef ref
    , bench "Pipe, low level" $ whnfIO $ do
        ref <- newIORef start
        let loop 0 = Done ()
            loop i =
                let next = HaveOutput (loop (pred i)) (return ())
                 in PipeM (liftM next (modifyIORef' ref succ))

        ConduitM (loop (1000 :: Int))
            $$ sinkNull
        readIORef ref
    ]
