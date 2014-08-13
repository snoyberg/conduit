import Test.Hspec
import Test.Hspec.QuickCheck

import Data.StreamConduit
import qualified Data.StreamConduit as C
import qualified Prelude as P
import Prelude
import Data.Functor.Identity
import qualified Data.List as P (foldl')

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
    -- FIXME add some finalization test
