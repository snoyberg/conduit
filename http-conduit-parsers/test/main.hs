{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.List (consume)
import Data.Conduit.HTTP

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit
import Data.ByteString.Char8 ()
import Control.Exception (try)

main :: IO ()
main = hspec $ do
    describe "headerLines" $ do
        it "works" $ do
            x <- mapM_ yield ["foo\r\n", "bar\r", "\nbaz\r\n\r\nbin\r\n"] $$ headerLines 1000 =$ consume
            x @?= ["foo", "bar", "baz"]
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
            x @?= ["foo foo2", "bar\tbar2", "baz"]
