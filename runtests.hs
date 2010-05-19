import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Network.Wai.Parse
import qualified Data.ByteString.Char8 as B8
import Control.Arrow

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Network.Wai.Parse"
    [ testCase "parseQueryString" caseParseQueryString
    , testCase "parseCookies" caseParseCookies
    , testCase "parseHttpAccept" caseParseHttpAccept
    ]

caseParseQueryString :: Assertion
caseParseQueryString = do
    let go l r =
            map (B8.pack *** B8.pack) l @=? parseQueryString (B8.pack r)

    go [] ""
    go [("foo", "")] "foo"
    go [("foo", "bar")] "foo=bar"
    go [("foo", "bar"), ("baz", "bin")] "foo=bar&baz=bin"
    go [("%Q", "")] "%Q"
    go [("%1Q", "")] "%1Q"
    go [("%1", "")] "%1"
    go [("/", "")] "%2F"
    go [("/", "")] "%2f"
    go [("foo bar", "")] "foo+bar"

caseParseCookies :: Assertion
caseParseCookies = do
    let input = B8.pack "a=a1;b=b2; c=c3"
        expected = [("a", "a1"), ("b", "b2"), ("c", "c3")]
    map (B8.pack *** B8.pack) expected @=? parseCookies input

caseParseHttpAccept :: Assertion
caseParseHttpAccept = do
    let input =
          B8.pack "text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c"
        expected = ["text/html", "text/x-c", "text/x-dvi", "text/plain"]
    map B8.pack expected @=? parseHttpAccept input
