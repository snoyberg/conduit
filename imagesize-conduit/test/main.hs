{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit.ImageSize

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit

main :: IO ()
main = hspec $ do
    describe "image size" $ do
        it "png" $ check (Just $ Size 271 61) "test/logo.png"
        it "jpg" $ check (Just $ Size 271 61) "test/logo.jpg"
        it "gif" $ check (Just $ Size 271 61) "test/logo.gif"
        it "invalid" $ check Nothing "test/main.hs"

check :: Maybe Size -> FilePath -> Assertion
check ex fp = do
    size <- C.runResourceT $ CB.sourceFile fp C.$$ sinkImageSize
    size @?= ex
