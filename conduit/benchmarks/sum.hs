{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
import Criterion.Main
import Data.Conduit
import Data.Conduit.Internal (ConduitM (..), Pipe (..))
import qualified Data.Conduit.List as CL
import Data.List (foldl')
import Control.Monad (foldM)
import Data.IORef
import Data.Functor.Identity (runIdentity)

upper :: Int
upper = 10000

plusM :: (Num a, Monad m) => a -> a -> m a
plusM x y = return $! x + y

main :: IO ()
main = do
    upperRef <- newIORef upper
    defaultMain $ reverse
        [ bench "foldl'" $ flip whnf upper $ \upper' ->
            foldl' (+) 0 [1..upper']
        , bench "foldM" $ whnfIO $ do
            upper' <- readIORef upperRef
            foldM plusM 0 [1..upper']
        , bench "conduit pure" $ flip whnf upper $ \upper' ->
            runIdentity (CL.enumFromTo 1 upper' $$ CL.fold (+) 0)
        , bench "conduit pure unrolled" $ whnfIO $ do
            upper' <- readIORef upperRef
            CL.enumFromTo 1 upper' $$ sumC
        , bench "conduit IO unrolled" $ whnfIO $ do
            upper' <- readIORef upperRef
            CL.enumFromTo 1 upper' $$ sumMC
        , bench "conduit IO" $ whnfIO $ do
            upper' <- readIORef upperRef
            CL.enumFromTo 1 upper' $$ CL.foldM plusM 0
        ]

sumC :: (Num a, Monad m) => Consumer a m a
sumC =
    ConduitM (go 0)
  where
    go total =
        NeedInput next done
      where
        done () = Done total

        next i =
            total' `seq` go total'
          where
            total' = total + i

sumMC :: (Num a, Monad m) => Consumer a m a
sumMC =
    ConduitM (go 0)
  where
    go total =
        NeedInput next done
      where
        done () = Done total

        next i = PipeM $ do
            total' <- plusM total i
            total' `seq` return (go total')
