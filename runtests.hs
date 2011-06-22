{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)

import Network.Wai.Handler.Warp (takeHeaders, InvalidRequest (..), readInt)
import Data.Enumerator (run_, ($$), enumList, run)
import Control.Exception (fromException)
import qualified Data.ByteString.Char8 as S8

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Text.Hamlet"
    [ testCase "takeUntilBlank safe" caseTakeUntilBlankSafe
    , testCase "takeUntilBlank too many lines" caseTakeUntilBlankTooMany
    , testCase "takeUntilBlank too large" caseTakeUntilBlankTooLarge
    , testProperty "takeInt" $ \i' ->
        let i = abs i'
         in i == readInt (S8.pack $ show i)
    ]

caseTakeUntilBlankSafe = do
    x <- run_ $ (enumList 1 ["f", "oo\n", "bar\nbaz\n\r\n"]) $$ takeHeaders
    x @?= ["foo", "bar", "baz"]

assertException x (Left se) =
    case fromException se of
        Just e -> e @?= x
        Nothing -> assertFailure "Not an exception"
assertException _ _ = assertFailure "Not an exception"

caseTakeUntilBlankTooMany = do
    x <- run $ (enumList 1 $ repeat "f\n") $$ takeHeaders
    assertException OverLargeHeader x

caseTakeUntilBlankTooLarge = do
    x <- run $ (enumList 1 $ repeat "f") $$ takeHeaders
    assertException OverLargeHeader x
