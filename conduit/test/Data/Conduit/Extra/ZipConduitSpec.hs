module Data.Conduit.Extra.ZipConduitSpec (spec) where
import Test.Hspec
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Applicative ((<*), pure)

spec :: Spec
spec = describe "Data.Conduit.Extra.ZipConduit" $ do
    it "ZipConduit" $ do
        let src = mapM_ yield [1..3 :: Int]
            conduit1 = CL.map (+1)
            conduit2 = CL.concatMap (replicate 2)
            conduit = getZipConduit $ ZipConduit conduit1 <* ZipConduit conduit2
            sink = CL.consume
        res <- src $$ conduit =$ sink
        res `shouldBe` [2, 1, 1, 3, 2, 2, 4, 3, 3]
    it "sequenceConduits" $ do
        let src = mapM_ yield [1..3 :: Int]
            conduit1 = CL.map (+1)
            conduit2 = CL.concatMap (replicate 2)
            conduit = do
                x <- sequenceConduits [conduit1, conduit2]
                yield $ length x + 10
            sink = CL.consume
        res <- src $$ conduit =$ sink
        res `shouldBe` [2, 1, 1, 3, 2, 2, 4, 3, 3, 12]
    it "ZipConduitMonad" $ do
        let src = mapM_ yield [1..3 :: Int]
            conduit1 = CL.mapM (pure . (+1))
            conduit2 = CL.map id
            conduit = getZipConduit $ ZipConduit conduit1 <* ZipConduit conduit2
            sink = CL.consume
        res <- src $$ conduit =$ sink
        res `shouldBe` [2, 1, 3, 2, 4, 3]
