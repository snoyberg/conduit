{-# LANGUAGE OverloadedStrings #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Network.Wai
import Network.Wai.Test
import Network.Wai.Parse
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as T
import Control.Arrow

import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Vhost
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.Debug (debug, debugDest)
import Codec.Compression.GZip (decompress)

import Data.Enumerator (run_, enumList, ($$), Iteratee)
import Data.Enumerator.Binary (enumFile)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (parseSimpleQuery, status200)

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Network.Wai.Parse"
    [ testCase "parseQueryString" caseParseQueryString
    , testCase "parseQueryString with question mark" caseParseQueryStringQM
    , testCase "parseHttpAccept" caseParseHttpAccept
    , testCase "parseRequestBody" caseParseRequestBody
    {-
    , testCase "findBound" caseFindBound
    , testCase "sinkTillBound" caseSinkTillBound
    , testCase "killCR" caseKillCR
    , testCase "killCRLF" caseKillCRLF
    , testCase "takeLine" caseTakeLine
    -}
    , testCase "jsonp" caseJsonp
    , testCase "gzip" caseGzip
    , testCase "gzip not for MSIE" caseGzipMSIE
    , testCase "vhost" caseVhost
    , testCase "autohead" caseAutohead
    , testCase "method override" caseMethodOverride
    , testCase "accept override" caseAcceptOverride
    , testCase "dalvik multipart" caseDalvikMultipart
    , testCase "debug request body" caseDebugRequestBody
    ]

caseParseQueryString :: Assertion
caseParseQueryString = do
    let go l r =
            map (S8.pack *** S8.pack) l @=? parseSimpleQuery (S8.pack r)

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

caseParseQueryStringQM :: Assertion
caseParseQueryStringQM = do
    let go l r =
            map (S8.pack *** S8.pack) l
                @=? parseSimpleQuery (S8.pack $ '?' : r)

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

caseParseHttpAccept :: Assertion
caseParseHttpAccept = do
    let input = "text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c"
        expected = ["text/html", "text/x-c", "text/x-dvi", "text/plain"]
    expected @=? parseHttpAccept input

parseRequestBody' :: Sink ([S8.ByteString] -> [S8.ByteString]) L.ByteString
                  -> SRequest
                  -> Iteratee S.ByteString IO ([(S.ByteString, S.ByteString)], [(S.ByteString, FileInfo L.ByteString)])
parseRequestBody' sink (SRequest req bod) =
    enumList 1 (L.toChunks bod) $$ parseRequestBody sink req

caseParseRequestBody :: Assertion
caseParseRequestBody = run_ t where
    content2 = S8.pack $
        "--AaB03x\n" ++
        "Content-Disposition: form-data; name=\"document\"; filename=\"b.txt\"\n" ++
        "Content-Type: text/plain; charset=iso-8859-1\n\n" ++
        "This is a file.\n" ++
        "It has two lines.\n" ++
        "--AaB03x\n" ++
        "Content-Disposition: form-data; name=\"title\"\n" ++
        "Content-Type: text/plain; charset=iso-8859-1\n\n" ++
        "A File\n" ++
        "--AaB03x\n" ++
        "Content-Disposition: form-data; name=\"summary\"\n" ++
        "Content-Type: text/plain; charset=iso-8859-1\n\n" ++
        "This is my file\n" ++
        "file test\n" ++
        "--AaB03x--"
    content3 = S8.pack "------WebKitFormBoundaryB1pWXPZ6lNr8RiLh\r\nContent-Disposition: form-data; name=\"yaml\"; filename=\"README\"\r\nContent-Type: application/octet-stream\r\n\r\nPhoto blog using Hack.\n\r\n------WebKitFormBoundaryB1pWXPZ6lNr8RiLh--\r\n"
    t = do
        let content1 = "foo=bar&baz=bin"
        let ctype1 = "application/x-www-form-urlencoded"
        result1 <- parseRequestBody' lbsSink $ toRequest ctype1 content1
        liftIO $ assertEqual "parsing post x-www-form-urlencoded"
                    (map (S8.pack *** S8.pack) [("foo", "bar"), ("baz", "bin")], [])
                    result1

        let ctype2 = "multipart/form-data; boundary=AaB03x"
        result2 <- parseRequestBody' lbsSink $ toRequest ctype2 content2
        let expectedsmap2 =
              [ ("title", "A File")
              , ("summary", "This is my file\nfile test")
              ]
        let textPlain = S8.pack $ "text/plain; charset=iso-8859-1"
        let expectedfile2 =
              [(S8.pack "document", FileInfo (S8.pack "b.txt") textPlain $ L8.pack
                 "This is a file.\nIt has two lines.")]
        let expected2 = (map (S8.pack *** S8.pack) expectedsmap2, expectedfile2)
        liftIO $ assertEqual "parsing post multipart/form-data"
                    expected2
                    result2

        let ctype3 = "multipart/form-data; boundary=----WebKitFormBoundaryB1pWXPZ6lNr8RiLh"
        result3 <- parseRequestBody' lbsSink $ toRequest ctype3 content3
        let expectedsmap3 = []
        let expectedfile3 = [(S8.pack "yaml", FileInfo (S8.pack "README") (S8.pack "application/octet-stream") $
                                L8.pack "Photo blog using Hack.\n")]
        let expected3 = (expectedsmap3, expectedfile3)
        liftIO $ assertEqual "parsing actual post multipart/form-data"
                    expected3
                    result3

        result2' <- parseRequestBody' lbsSink $ toRequest' ctype2 content2
        liftIO $ assertEqual "parsing post multipart/form-data 2"
                    expected2
                    result2'

        result3' <- parseRequestBody' lbsSink $ toRequest' ctype3 content3
        liftIO $ assertEqual "parsing actual post multipart/form-data 2"
                    expected3
                    result3'

toRequest :: S8.ByteString -> S8.ByteString -> SRequest
toRequest ctype content = SRequest (Request
    { requestHeaders = [("Content-Type", ctype)]
    , requestMethod = "POST"
    , rawPathInfo = ""
    , rawQueryString = ""
    , queryString = []
    }) (L.fromChunks [content])

toRequest' :: S8.ByteString -> S8.ByteString -> SRequest
toRequest' ctype content = SRequest (Request
    { requestHeaders = [("Content-Type", ctype)]
    }) (L.fromChunks $ map S.singleton $ S.unpack content)

{-
caseFindBound :: Assertion
caseFindBound = do
    findBound (S8.pack "def") (S8.pack "abcdefghi") @?=
        FoundBound (S8.pack "abc") (S8.pack "ghi")
    findBound (S8.pack "def") (S8.pack "ABC") @?= NoBound
    findBound (S8.pack "def") (S8.pack "abcd") @?= PartialBound
    findBound (S8.pack "def") (S8.pack "abcdE") @?= NoBound
    findBound (S8.pack "def") (S8.pack "abcdEdef") @?=
        FoundBound (S8.pack "abcdE") (S8.pack "")

caseSinkTillBound :: Assertion
caseSinkTillBound = do
    let iter () _ = return ()
    let src = S8.pack "this is some text"
        bound1 = S8.pack "some"
        bound2 = S8.pack "some!"
    let enum = enumList 1 [src]
    let helper _ _ = return ()
    (_, res1) <- run_ $ enum $$ sinkTillBound bound1 helper ()
    res1 @?= True
    (_, res2) <- run_ $ enum $$ sinkTillBound bound2 helper ()
    res2 @?= False

caseKillCR :: Assertion
caseKillCR = do
    "foo" @=? killCR "foo"
    "foo" @=? killCR "foo\r"
    "foo\r\n" @=? killCR "foo\r\n"
    "foo\r'" @=? killCR "foo\r'"

caseKillCRLF :: Assertion
caseKillCRLF = do
    "foo" @=? killCRLF "foo"
    "foo\r" @=? killCRLF "foo\r"
    "foo" @=? killCRLF "foo\r\n"
    "foo\r'" @=? killCRLF "foo\r'"
    "foo" @=? killCRLF "foo\n"

caseTakeLine :: Assertion
caseTakeLine = do
    helper "foo\nbar\nbaz" "foo"
    helper "foo\r\nbar\nbaz" "foo"
    helper "foo\nbar\r\nbaz" "foo"
    helper "foo\rbar\r\nbaz" "foo\rbar"
  where
    helper haystack needle = do
        x <- run_ $ enumList 1 [haystack] $$ takeLine
        Just needle @=? x
-}

jsonpApp = jsonp $ const $ return $ responseLBS
    status200
    [("Content-Type", "application/json")]
    "{\"foo\":\"bar\"}"

caseJsonp :: Assertion
caseJsonp = flip runSession jsonpApp $ do
    sres1 <- request Request
                { queryString = [("callback", Just "test")]
                , requestHeaders = [("Accept", "text/javascript")]
                }
    assertContentType "text/javascript" sres1
    assertBody "test({\"foo\":\"bar\"})" sres1

    sres2 <- request Request
                { queryString = [("call_back", Just "test")]
                , requestHeaders = [("Accept", "text/javascript")]
                }
    assertContentType "application/json" sres2
    assertBody "{\"foo\":\"bar\"}" sres2

    sres3 <- request Request
                { queryString = [("callback", Just "test")]
                , requestHeaders = [("Accept", "text/html")]
                }
    assertContentType "application/json" sres3
    assertBody "{\"foo\":\"bar\"}" sres3

gzipApp = gzip True $ const $ return $ responseLBS status200 [] "test"

caseGzip :: Assertion
caseGzip = flip runSession gzipApp $ do
    sres1 <- request Request
                { requestHeaders = [("Accept-Encoding", "gzip")]
                }
    assertHeader "Content-Encoding" "gzip" sres1
    liftIO $ decompress (simpleBody sres1) @?= "test"

    sres2 <- request Request
                { requestHeaders = []
                }
    assertNoHeader "Content-Encoding" sres2
    assertBody "test" sres2

caseGzipMSIE :: Assertion
caseGzipMSIE = flip runSession gzipApp $ do
    sres1 <- request Request
                { requestHeaders =
                    [ ("Accept-Encoding", "gzip")
                    , ("User-Agent", "Mozilla/4.0 (Windows; MSIE 6.0; Windows NT 6.0)")
                    ]
                }
    assertNoHeader "Content-Encoding" sres1
    liftIO $ simpleBody sres1 @?= "test"

vhostApp1 = const $ return $ responseLBS status200 [] "app1"
vhostApp2 = const $ return $ responseLBS status200 [] "app2"
vhostApp = vhost
    [ ((== "foo.com") . serverName, vhostApp1)
    ]
    vhostApp2

caseVhost = flip runSession vhostApp $ do
    sres1 <- request Request
                { serverName = "foo.com"
                }
    assertBody "app1" sres1

    sres2 <- request Request
                { serverName = "bar.com"
                }
    assertBody "app2" sres2

autoheadApp = autohead $ const $ return $ responseLBS status200
    [("Foo", "Bar")] "body"

caseAutohead = flip runSession autoheadApp $ do
    sres1 <- request Request
                { requestMethod = "GET"
                }
    assertHeader "Foo" "Bar" sres1
    assertBody "body" sres1

    sres1 <- request Request
                { requestMethod = "HEAD"
                }
    assertHeader "Foo" "Bar" sres1
    assertBody "" sres1

moApp = methodOverride $ \req -> return $ responseLBS status200
    [("Method", requestMethod req)] ""

caseMethodOverride = flip runSession moApp $ do
    sres1 <- request Request
                { requestMethod = "GET"
                , queryString = []
                }
    assertHeader "Method" "GET" sres1

    sres2 <- request Request
                { requestMethod = "POST"
                , queryString = []
                }
    assertHeader "Method" "POST" sres2

    sres3 <- request Request
                { requestMethod = "POST"
                , queryString = [("_method", Just "PUT")]
                }
    assertHeader "Method" "PUT" sres3

aoApp = acceptOverride $ \req -> return $ responseLBS status200
    [("Accept", fromMaybe "" $ lookup "Accept" $ requestHeaders req)] ""

caseAcceptOverride = flip runSession aoApp $ do
    sres1 <- request Request
                { queryString = []
                , requestHeaders = [("Accept", "foo")]
                }
    assertHeader "Accept" "foo" sres1

    sres2 <- request Request
                { queryString = []
                , requestHeaders = [("Accept", "bar")]
                }
    assertHeader "Accept" "bar" sres2

    sres3 <- request Request
                { queryString = [("_accept", Just "baz")]
                , requestHeaders = [("Accept", "bar")]
                }
    assertHeader "Accept" "baz" sres3

caseDalvikMultipart = do
    let headers =
            [ ("content-length", "12098")
            , ("content-type", "multipart/form-data;boundary=*****")
            , ("GATEWAY_INTERFACE", "CGI/1.1")
            , ("PATH_INFO", "/")
            , ("QUERY_STRING", "")
            , ("REMOTE_ADDR", "192.168.1.115")
            , ("REMOTE_HOST", "ganjizza")
            , ("REQUEST_URI", "http://192.168.1.115:3000/")
            , ("REQUEST_METHOD", "POST")
            , ("HTTP_CONNECTION", "Keep-Alive")
            , ("HTTP_COOKIE", "_SESSION=fgUGM5J/k6mGAAW+MMXIJZCJHobw/oEbb6T17KQN0p9yNqiXn/m/ACrsnRjiCEgqtG4fogMUDI+jikoFGcwmPjvuD5d+MDz32iXvDdDJsFdsFMfivuey2H+n6IF6yFGD")
            , ("HTTP_USER_AGENT", "Dalvik/1.1.0 (Linux; U; Android 2.1-update1; sdk Build/ECLAIR)")
            , ("HTTP_HOST", "192.168.1.115:3000")
            , ("HTTP_ACCEPT", "*, */*")
            , ("HTTP_VERSION", "HTTP/1.1")
            , ("REQUEST_PATH", "/")
            ]
    let request = Request
            { requestHeaders = headers
            }
    (params, files) <- run_ $ enumFile "test/dalvik-request" $$ parseRequestBody lbsSink request
    lookup "scannedTime" params @?= Just "1.298590056748E9"
    lookup "geoLong" params @?= Just "0"
    lookup "geoLat" params @?= Just "0"
    length files @?= 1

caseDebugRequestBody :: Assertion
caseDebugRequestBody = flip runSession debugApp $ do
    let req = toRequest "application/x-www-form-urlencoded" "foo=bar&baz=bin"
    res <- srequest req
    assertStatus 200 res
  where
    params = [("foo", "bar"), ("baz", "bin")]
    debugOutput = T.pack $ "POST \nAccept: \n" ++ (show params)

    debugApp = debugDest (\t -> liftIO $ assertEqual "debug" debugOutput t) $ \req -> do
        return $ responseLBS status200 [ ] ""
    {-debugApp = debug $ \req -> do-}
        {-return $ responseLBS status200 [ ] ""-}
