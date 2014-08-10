import Criterion.Main
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.List (foldl')
import Control.Monad (foldM)
import Data.IORef
import Data.Functor.Identity (runIdentity)

upper :: Int
upper = 10000

plusM :: Monad m => Int -> Int -> m Int
plusM x y = return $! x + y

main :: IO ()
main = do
    upperRef <- newIORef upper
    defaultMain
        [ bench "foldl'" $ flip whnf upper $ \upper' ->
            foldl' (+) 0 [1..upper']
        , bench "foldM" $ whnfIO $ do
            upper' <- readIORef upperRef
            foldM plusM 0 [1..upper']
        , bench "conduit pure" $ flip whnf upper $ \upper' ->
            runIdentity (CL.enumFromTo 1 upper' $$ CL.fold (+) 0)
        , bench "conduit IO" $ whnfIO $ do
            upper' <- readIORef upperRef
            CL.enumFromTo 1 upper' $$ CL.foldM plusM 0
        ]
