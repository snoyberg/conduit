import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO (liftIO))

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Conduit
import qualified Data.Conduit.List as CL

-- Test the expected behaviour of @Data.Conduit.List.iterM@
prop_behaviour :: [a] -> Property
prop_behaviour l = monadicIO $ do
    v <- run $ do
        ref <- newMVar 0
        CL.sourceList l $= counter ref $$ CL.mapM_ (const $ return ())
        readMVar ref

    assert $ v == length l
  where
    counter :: (Num a, MonadIO m) => MVar a -> Conduit b m b
    counter ref = CL.iterM (const $ liftIO $ modifyMVar_ ref (\i -> return $! i + 1))

-- Test the equivalence relation as mentioned in the @iterM@ docs:
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
prop_mapM_equivalence :: Num a => [a] -> Property
prop_mapM_equivalence l = monadicIO $ do
    (c1, s1) <- runTest CL.iterM
    (c2, s2) <- runTest (\f -> CL.mapM (\a -> f a >>= \() -> return a))

    assert $ c1 == c2
    assert $ s1 == s2
  where
    runTest h = run $ do
        ref <- newMVar (0 :: Int)
        let f = action ref
        s <- CL.sourceList l $= h f $$ CL.fold (+) 0
        c <- readMVar ref

        return (c, s)

    action :: (Num a, MonadIO m) => MVar a -> b -> m ()
    action ref = const $ liftIO $ modifyMVar_ ref (\i -> return $! i + 1)

main :: IO ()
main = do
    quickCheck (prop_behaviour :: [Int] -> Property)
    quickCheck (prop_mapM_equivalence :: [Int] -> Property)
