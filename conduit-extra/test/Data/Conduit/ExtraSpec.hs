module Data.Conduit.ExtraSpec where

import Data.Conduit
import Data.Conduit.Extra
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
        let sink2 :: Sink a IO (Maybe a, Maybe a)
            sink2 = do
                ma1 <- fuseReturnLeftovers id (isolate 10) peek
                ma2 <- peek
                return (ma1, ma2)

            source = yield 1 >> yield 2
        res <- source $$ sink2
        res `shouldBe` (Just 1, Just 1)

    prop "more complex" $ \ss cnt -> do
        let ts = map T.pack ss
            src = mapM_ (yield . T.encodeUtf8) ts
            conduit = CL.map T.decodeUtf8
            sink = CT.take cnt =$ consume
            undo = return . T.encodeUtf8 . T.concat
        res <- src $$ do
            x <- fuseReturnLeftovers undo conduit sink
            y <- consume
            return (T.concat x, T.decodeUtf8 $ S.concat y)
        res `shouldBe` T.splitAt cnt (T.concat ts)

main :: IO ()
main = hspec spec
