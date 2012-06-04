{-# LANGUAGE PackageImports #-}
import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.ST (runST)
import Control.Monad (foldM)
import Data.List (foldl')
import "pipes" Control.Pipe
import Data.Void
import Control.Applicative
import Data.Maybe
import qualified "pipes-core" Control.Pipe as PC
import qualified "pipes-core" Control.Pipe.Combinators as PCC

main :: IO ()
main = defaultMain
    [ bench "bigsum-resourcet-io" (whnfIO $ C.runResourceT $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "bigsum-io" (whnfIO $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) 0)
    , bench "bigsum-st" $ whnf (\i -> (runST $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) i)) 0
    , bench "bigsum-identity" $ whnf (\i -> (runIdentity $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) i)) 0
    , bench "bigsum-spipe" $ whnf (\i -> (runIdentity $ (mapM_ CI.yield [1..1000 :: Int]) C.$$ CL.fold (+) i)) 0
    , bench "bigsum-foldM" $ whnf (\i -> (runIdentity $ foldM (\a b -> return $! a + b) i [1..1000 :: Int])) 0
    , bench "bigsum-pure" $ whnf (\i -> foldl' (+) i [1..1000 :: Int]) 0
    , bench "bigsum-pipes" $ whnf pipeTest 0
    , bench "bigsum-pipes-core" $ whnf pipeCoreTest 0
    , bench "bigsum-pipes-core-seq" $ whnf pipeCoreTestSeq 0
    ]

pipeFold :: Monad m => (b -> a -> b) -> b -> Frame a o m b
pipeFold f =
    Frame . go
  where
    go accum = do
        mx <- await
        case mx of
            Nothing -> return $ return accum
            Just x ->
                let accum' = f accum x
                 in accum' `seq` go accum

pipeSourceList :: Monad m => [a] -> Frame i a m ()
pipeSourceList list = Frame $ do
    mapM_ yieldF list
    close $ return ()

pipeTest :: Int -> Int
pipeTest start = fromMaybe (-1) $ runIdentity $ runFrame $ (Just <$> pipeFold (+) start) <-< (Nothing <$ pipeSourceList [1..1000 :: Int])

pipesCoreFold :: Monad m => (b -> a -> b) -> b -> PC.Pipe a x m b
pipesCoreFold f = go
  where
    go x = PCC.tryAwait >>= maybe (return x) (let y = f x in y `seq` go . y)

pipeCoreTest :: Int -> Int
pipeCoreTest start = fromMaybe (-1) $ runIdentity $ PC.runPurePipe_ $ PCC.fromList [1..1000 :: Int] PCC.$$ PCC.fold (+) start

pipeCoreTestSeq :: Int -> Int
pipeCoreTestSeq start = fromMaybe (-1) $ runIdentity $ PC.runPurePipe_ $ PCC.fromList [1..1000 :: Int] PCC.$$ pipesCoreFold (+) start
