{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Network.Wai.Handler.Warp (takeLineMax, takeUntilBlank, InvalidRequest (..))
import Data.Enumerator (run_, ($$), enumList, run)
import Control.Exception (fromException)

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Text.Hamlet"
    [ testCase "takeLineMax safe" caseTakeLineMaxSafe
    , testCase "takeUntilBlank safe" caseTakeUntilBlankSafe
    , testCase "takeLineMax unsafe" caseTakeLineMaxUnsafe
    , testCase "takeLineMax incomplete" caseTakeLineMaxIncomplete
    , testCase "takeUntilBlank too many lines" caseTakeUntilBlankTooMany
    , testCase "takeUntilBlank too large" caseTakeUntilBlankTooLarge
    ]

caseTakeLineMaxSafe = do
    x <- run_ $ (enumList 1 ["f", "oo\n\n", "bar\n\nbaz\n"]) $$ do
        a <- takeLineMax 0 id
        b <- takeLineMax 0 id
        c <- takeLineMax 0 id
        d <- takeLineMax 0 id
        e <- takeLineMax 0 id
        return (a, b, c, d, e)
    x @?= ("foo", "", "bar", "", "baz")

caseTakeUntilBlankSafe = do
    x <- run_ $ (enumList 1 ["f", "oo\n", "bar\nbaz\n\n"]) $$ takeUntilBlank 0 id
    x @?= ["foo", "bar", "baz"]

caseTakeLineMaxUnsafe = do
    x <- run $ (enumList 1 $ repeat "abc") $$ do
        a <- takeLineMax 0 id
        b <- takeLineMax 0 id
        c <- takeLineMax 0 id
        d <- takeLineMax 0 id
        e <- takeLineMax 0 id
        return (a, b, c, d, e)
    assertException OverLargeHeader x

assertException x (Left se) =
    case fromException se of
        Just e -> e @?= x
        Nothing -> assertFailure "Not an exception"
assertException _ _ = assertFailure "Not an exception"

caseTakeLineMaxIncomplete = do
    x <- run $ (enumList 1 ["f", "oo\n\n", "bar\n\nbaz"]) $$ do
        a <- takeLineMax 0 id
        b <- takeLineMax 0 id
        c <- takeLineMax 0 id
        d <- takeLineMax 0 id
        e <- takeLineMax 0 id
        return (a, b, c, d, e)
    assertException IncompleteHeaders x

caseTakeUntilBlankTooMany = do
    x <- run $ (enumList 1 $ repeat "f\n") $$ takeUntilBlank 0 id
    assertException TooManyHeaders x

caseTakeUntilBlankTooLarge = do
    x <- run $ (enumList 1 $ repeat "f") $$ takeUntilBlank 0 id
    assertException OverLargeHeader x
