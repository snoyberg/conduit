{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.List (consume, sinkNull)
import Data.Conduit.HTTP

import Test.Hspec
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Exception (try)

main :: IO ()
main = hspec $ do
    describe "headerLines" $ do
        it "works" $ do
            x <- mapM_ yield ["foo\r\n", "bar\r", "\nbaz\r\n\r\nbin\r\n"] $$ headerLines 1000 =$ consume
            x `shouldBe` ["foo", "bar", "baz"]
        it "overlargeheader" $ do
            x <- try $ mapM_ yield ["foo\r\n", "bar\r", "\nbaz\r\n\r\nbin\r\n"] $$ headerLines 5 =$ consume
            case x of
                Left OverLargeHeader -> return ()
                _ -> error $ show x
        it "incompleteheaders" $ do
            x <- try $ mapM_ yield ["foo\r\n", "bar\r", "\nbaz\r\nbin\r\n"] $$ headerLines 1000 =$ consume
            case x of
                Left IncompleteHeaders -> return ()
                _ -> error $ show x
        it "multiline" $ do
            x <- mapM_ yield ["foo\r\n", " foo2\r\nbar\r", "\n\tbar2\r\nbaz\r\n\r\nbin\r\n"] $$ headerLines 1000 =$ consume
            x `shouldBe` ["foo foo2", "bar\tbar2", "baz"]
        it "body remains" $ do
            x <- mapM_ yield ["foo\r\n", " foo2\r\nbar\r", "\n\tbar2\r\nbaz\r\n\r\nbin\r\n"] $$ do
                headerLines 1000 =$ sinkNull
                consume
            L.fromChunks x `shouldBe` "bin\r\n"
        it "doesn't consume too much" $ do
            x <- mapM_ yield ["foo\r\n", "bar\r", "\nbaz\r\n\r\n", error "too much"] $$ headerLines 1000 =$ consume
            x `shouldBe` ["foo", "bar", "baz"]
