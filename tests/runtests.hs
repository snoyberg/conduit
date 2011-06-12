{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Application.Static

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.HUnit ((@?=), assert)
import Distribution.Simple.Utils (isInfixOf)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


import Network.Wai
import Network.Wai.Test

import Network.Socket.Internal as Sock
import qualified Network.HTTP.Types as H
import Control.Monad.IO.Class (liftIO)

defRequest :: Request
defRequest = Request {
  rawQueryString = ""
, queryString = []
, requestMethod = "GET"
, rawPathInfo = ""
, pathInfo = []
, requestHeaders = []
, serverName = "wai-test"
, httpVersion = H.http11
, serverPort = 80
, isSecure = False
, remoteHost = Sock.SockAddrUnix "/dev/null"
}

setRawPathInfo :: Request -> S8.ByteString -> Request
setRawPathInfo r rawPinfo = 
  let pInfo = T.split (== '/') $ TE.decodeUtf8 rawPinfo
  in  r { rawPathInfo = rawPinfo, pathInfo = pInfo }



main :: IO a
main = hspecX $ do
  let must = liftIO . assert

  describe "Pieces: pathFromPieces" $ do
    it "converts to a file path" $
      (pathFromPieces "prefix" ["a", "bc"]) @?= "prefix/a/bc"

    prop "each piece is in file path" $ \pieces ->
      let piecesT = (map T.pack pieces) in
      all (\p -> ("/" ++ (unfixPathName $ T.unpack p)) `isInfixOf` (pathFromPieces "root" $ piecesT)) piecesT


  let cacheLookup = Forever (\fp h -> fp == "tests/runtests.hs" && h == "cached")
  let statApp = flip runSession $ staticApp $ (defaultStaticSettings cacheLookup) {ssFolder = "tests"}
  describe "staticApp" $ do
    it "403 for unsafe paths" $ statApp $
      flip mapM_ ["..", "."] $ \path ->
        assertStatus 403 =<<
          request (setRawPathInfo defRequest path)

    it "200 for hidden paths" $ statApp $
      flip mapM_ [".hidden/folder.png", ".hidden/haskell.png"] $ \path ->
        assertStatus 200 =<<
          request (setRawPathInfo defRequest path)

    it "directory listing for index" $ statApp $ do
      resp <- request (setRawPathInfo defRequest "a/")
      assertStatus 200 resp
      let body = simpleBody resp
      let contains a b = isInfixOf b (L8.unpack a)
      must $ body `contains` "<img src=\"../.hidden/haskell.png\" />"
      must $ body `contains` "<img src=\"../.hidden/folder.png\" alt=\"Folder\" />"
      must $ body `contains` "<a href=\"b\">b</a>"

    it "404 for non-existant files" $ statApp $
      assertStatus 404 =<<
        request (setRawPathInfo defRequest "doesNotExist")

    it "301 redirect when multiple slashes" $ statApp $ do
      req <- request (setRawPathInfo defRequest "a//b/c")
      assertStatus 301 req
      assertHeader "Location" "../../a/b/c" req

    let absoluteApp = flip runSession $ staticApp $ (defaultStaticSettings cacheLookup) {
          ssFolder = "tests", ssMkRedirect = \_ u -> S8.append "http://www.example.com" u
        }
    it "301 redirect when multiple slashes" $ absoluteApp $
      flip mapM_ ["/a//b/c", "a//b/c"] $ \path -> do
        req <- request (setRawPathInfo defRequest path)
        assertStatus 301 req
        assertHeader "Location" "http://www.example.com/a/b/c" req

  describe "when requesting a static asset" $ do
    let runtests = setRawPathInfo defRequest "runtests.hs"
    it "200 when no query parameters" $ statApp $ do
      req <- request runtests {
        requestHeaders = [("If-Modified-Since", "")]
      }
      assertStatus 200 req
      assertNoHeader "Cache-Control" req

    it "200 when no cache headers sent" $ statApp $ do
      req <- request runtests { rawQueryString ="?cached" }
      assertStatus 200 req
      assertHeader "Cache-Control" "max-age=31536000" req

    it "200 when If-Unmodified-Since" $ statApp $ do
      req <- request runtests { rawQueryString ="?cached",
        requestHeaders = [("If-Unmodified-Since", "")]
      }
      assertStatus 200 req
      assertHeader "Cache-Control" "max-age=31536000" req

    it "304 when cache header and query parameter sent" $ statApp $ do
      req <- request runtests {  rawQueryString ="?cached",
        requestHeaders = [("If-Modified-Since", "")]
      }
      assertStatus 304 req
      assertNoHeader "Cache-Control" req

  let pubApp = flip runSession $ staticApp (defaultPublicSettings $ ETag (\_ -> return $ Just "hash")) {ssFolder = "tests"}
  describe "staticApp when requesting a public asset - etags" $ do
    it "200 when no etag parameters" $ pubApp $ do
      req <- request (setRawPathInfo defRequest "runtests.hs")
      assertStatus 200 req
      assertHeader "Etag" "hash" req

    it "304 when etag parameter sent" $ pubApp $ do
      req <- request (setRawPathInfo defRequest "runtests.hs") { requestHeaders  = [("If-None-Match", "hash")] }
      assertStatus 304 req
      assertNoHeader "Etag" req
