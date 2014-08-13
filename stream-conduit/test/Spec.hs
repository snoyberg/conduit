import Test.Hspec
import Test.Hspec.QuickCheck

import Data.StreamConduit
import qualified Data.StreamConduit as C
import qualified Prelude as P
import Prelude
import Data.Functor.Identity
import qualified Data.List as P (foldl')
import Control.Monad (replicateM)

main :: IO ()
main = hspec $ do
    prop "enumFromTo/map/sinkList" $ \x' y' -> do
        let (x, y)
                | x' < y' = (x' :: Int, y')
                | otherwise = (y', x')
            expected = P.map (+ 1) $ P.enumFromTo x y
            actual = runIdentity
                   $ runConduit
                   $ C.enumFromTo x y
                 =$= C.map (+ 1)
                 =$= sinkList
        actual `shouldBe` expected
    prop "enumFromTo/map/foldl'" $ \x' y' -> do
        let (x, y)
                | x' < y' = (x' :: Int, y')
                | otherwise = (y', x')
            expected = P.foldl' (+) 0 $ P.map (+ 1) $ P.enumFromTo x y
            actual = runIdentity
                   $ runConduit
                   $ C.enumFromTo x y
                 =$= C.map (+ 1)
                 =$= C.foldl' (+) 0
        actual `shouldBe` expected
    prop "enumFromTo/map/take/foldl'" $ \x' y' -> do
        let (x, y)
                | x' < y' = (x' :: Int, y')
                | otherwise = (y', x')
            expected = P.foldl' (+) 0 $ P.take 5 $ P.map (+ 1) $ P.enumFromTo x y
            actual = runIdentity
                   $ runConduit
                   $ C.enumFromTo x y
                 =$= C.map (+ 1)
                 =$= C.take 5
                 =$= C.foldl' (+) 0
        actual `shouldBe` expected
    it "take/monadic compisition" $ do
        let sink = do
                x <- C.take 5 =$= sinkList
                y <- sinkList
                return (x, y :: [Int])
        res <- runConduit $ C.enumFromTo 1 10 =$= sink
        res `shouldBe` ([1..5], [6..10])
    prop "mapM_ yield works" $ \is -> do
        res <- runConduit $ mapM_ yield is =$= sinkList
        res `shouldBe` (is :: [Int])
    prop "mapM_ await works" $ \is -> do
        res <- runConduit $ mapM_ yield is =$= do
            x <- replicateM (length is) await
            y <- replicateM 5 await
            return (x, y)
        res `shouldBe` (P.map Just is :: [Maybe Int], replicate 5 Nothing)
    -- FIXME add some finalization test
