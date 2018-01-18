module Data.Conduit.ExtraSpec where

import Data.Conduit
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Conduit.List (isolate, peek, consume)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import qualified Data.Conduit.Text as CT

spec :: Spec
spec = describe "Data.Conduit.Extra" $ do
    it "basic test" $ do
        let sink2 :: ConduitT a o IO (Maybe a, Maybe a)
            sink2 = do
                ma1 <- fuseLeftovers id (isolate 10) peek
                ma2 <- peek
                return (ma1, ma2)

            source = yield 1 >> yield 2
        res <- runConduit $ source .| sink2
        res `shouldBe` (Just 1, Just (1 :: Int))

    it "get leftovers" $ do
        let sink2 :: ConduitT a o IO ([a], [a], [a])
            sink2 = do
                (x, y) <- fuseReturnLeftovers (isolate 2) peek3
                z <- CL.consume
                return (x, y, z)

            peek3 = do
                x <- CL.take 3
                mapM_ leftover $ reverse x
                return x

            source = mapM_ yield [1..5 :: Int]
        res <- runConduit $ source .| sink2
        res `shouldBe` ([1..2], [1..2], [3..5])

    it "multiple values" $ do
        let sink2 :: ConduitT a o IO ([a], Maybe a)
            sink2 = do
                ma1 <- fuseLeftovers id (isolate 10) peek3
                ma2 <- peek
                return (ma1, ma2)

            peek3 = do
                x <- CL.take 3
                mapM_ leftover $ reverse x
                return x

            source = mapM_ yield [1..5]
        res <- runConduit $ source .| sink2
        res `shouldBe` ([1..3], Just (1 :: Int))

    prop "more complex" $ \ss cnt -> do
        let ts = map T.pack ss
            src = mapM_ (yield . T.encodeUtf8) ts
            conduit = CL.map T.decodeUtf8
            sink = CT.take cnt .| consume
            undo = return . T.encodeUtf8 . T.concat
        res <- runConduit $ src .| do
            x <- fuseLeftovers undo conduit sink
            y <- consume
            return (T.concat x, T.decodeUtf8 $ S.concat y)
        res `shouldBe` T.splitAt cnt (T.concat ts)

main :: IO ()
main = hspec spec
