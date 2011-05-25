{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Handler.Launch (insideHead)
import Test.Hspec
import Test.Hspec.HUnit
import Data.Enumerator hiding (map)
import Test.HUnit
import qualified Data.ByteString as S

main :: IO ()
main = hspec $ describe "insideHead"
    [ it "handles single chunks" $ helper "bar<head>foobaz" ["bar<head>baz"]
    , it "handles single chunks, no <head>" $ helper "barfoo" ["bar"]
    , it "handles whole chunks" $ helper "bar<head>foobaz" ["bar", "<head>", "baz"]
    , it "handles split chunks" $ helper "bar<head>foobaz" ["bar", "<he", "ad>", "baz"]
    , it "handles many chunks" $ helper "bar<head>foobaz1234567890" ["bar", "<he", "ad>", "baz", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
    , it "handles many pieces" $ helper "bar<head>foobaz" $ map S.singleton $ S.unpack "bar<head>baz"
    ]

helper :: S.ByteString -> [S.ByteString] -> Assertion
helper res pieces = do
    x <- run_ $ enumList 1 pieces $$ joinI $ insideHead "foo" $$ consume
    S.concat x @?= res
