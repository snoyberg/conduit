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
spec = describe "Data.Conduit.Async" $ do
    it "prints" $ do
        let source = CL.sourceList ["anna", "banana", "cuckoo"]
            printAndPassthrough s = do
                putStrLn $ "Starting " ++ s
                threadDelay 10000000
                putStrLn $ "Done " ++ s
                return s
        res <- runConduit $ source .| mapAsync 2 printAndPassthrough .| CL.consume
        res `shouldBe` ["anna", "banana", "cuckoo"]

main :: IO ()
main = hspec spec
