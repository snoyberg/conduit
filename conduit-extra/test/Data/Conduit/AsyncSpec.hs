module Data.Conduit.AsyncSpec where

import Data.Conduit
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Conduit.Async
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import qualified Data.Conduit.Text as CT
import Control.Concurrent

spec :: Spec
spec = describe "Data.Conduit.Async" $
    it "maintains order on mapAsync" $ do
        let source = CL.sourceList "abcdefg"
        res <- runConduit $ source .| mapAsync 3 return .| CL.consume
        res `shouldBe` "abcdefg"

main :: IO ()
main = hspec spec
