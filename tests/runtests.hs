{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
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
import System.IO (stderr, hPutStrLn)


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


-- debug :: String -> m0 ()
debug = liftIO . hPutStrLn stderr

main :: IO a
main = hspecX $ do
  let must = liftIO . assert

{-
  describe "Pieces: pathFromPieces" $ do
    it "converts to a file path" $
      (pathFromPieces "prefix" [Piece "a" "a", Piece "bc" "bc"]) @?= "prefix/a/bc"

    prop "each piece is in file path" $ \piecesS ->
      let pieces = map (\p -> Piece p "") piecesS
      in  all (\p -> ("/" ++ p) `isInfixOf` (pathFromPieces "root" $ pieces)) piecesS
-}


  let fileServerApp = flip runSession $ staticApp defaultFileServerSettings  {ssFolder = fileSystemLookup "tests"}

  describe "fileServerApp" $ do
    it "directory listing for index" $ fileServerApp $ do
      resp <- request (setRawPathInfo defRequest "a/")
      assertStatus 200 resp
      let body = simpleBody resp
      let contains a b = isInfixOf b (L8.unpack a)
      must $ body `contains` "<img src=\"../.hidden/haskell.png\" />"
      must $ body `contains` "<img src=\"../.hidden/folder.png\" alt=\"Folder\" />"
      must $ body `contains` "<a href=\"b\">b</a>"

  let webApp = flip runSession $ staticApp defaultWebAppSettings  {ssFolder = fileSystemLookup "tests"}

  {-
  describe "webApp" $ do
    it "403 for unsafe paths" $ webApp $
      flip mapM_ ["..", "."] $ \path ->
        assertStatus 403 =<<
          request (setRawPathInfo defRequest path)

    it "200 for hidden paths" $ webApp $
      flip mapM_ [".hidden/folder.png", ".hidden/haskell.png"] $ \path ->
        assertStatus 200 =<<
          request (setRawPathInfo defRequest path)

-}

{-
    it "404 for non-existant files" $ webApp $
      assertStatus 404 =<<
        request (setRawPathInfo defRequest "doesNotExist")

    it "301 redirect when multiple slashes" $ webApp $ do
      req <- request (setRawPathInfo defRequest "a//b/c")
      assertStatus 301 req
      assertHeader "Location" "../../a/b/c" req

    let absoluteApp = flip runSession $ staticApp $ defaultWebAppSettings {
          ssFolder = fileSystemLookup "tests", ssMkRedirect = \_ u -> S8.append "http://www.example.com" u
        }
    it "301 redirect when multiple slashes" $ absoluteApp $
      flip mapM_ ["/a//b/c", "a//b/c"] $ \path -> do
        req <- request (setRawPathInfo defRequest path)
        assertStatus 301 req
        assertHeader "Location" "http://www.example.com/a/b/c" req
-}

  describe "webApp when requesting a static asset" $ do
    let statFile = setRawPathInfo defRequest "a/b"
    {-
    it "200 when no query parameters" $ webApp $ do
      req <- request statFile
      assertStatus 200 req
      assertNoHeader "Cache-Control" req

    it "200 when no cache headers and bad cache query string" $ webApp $ do
      req <- request statFile { queryString = [("etag", Just "cached")] }
      assertStatus 301 req
      assertHeader "Location" "../a/b?etag=1B2M2Y8AsgTpgAmY7PhCfg%3D%3D" req
      assertNoHeader "Cache-Control" req

    it "200 when no cache headers and empty cache query string" $ webApp $ do
      req <- request statFile { queryString = [("etag", Nothing)] }
      assertStatus 301 req
      assertHeader "Location" "../a/b?etag=1B2M2Y8AsgTpgAmY7PhCfg%3D%3D" req
      assertNoHeader "Cache-Control" req

    it "Cache-Control set when etag parameter is correct" $ webApp $ do
      req <- request statFile { queryString = [("etag", Just "1B2M2Y8AsgTpgAmY7PhCfg==")] }
      assertStatus 200 req
      assertHeader "Cache-Control" "max-age=31536000" req
      -}

    it "200 when invalid if-modified-since header" $ webApp $ do
      req <- request statFile {
        requestHeaders = [("If-Modified-Since", "123")]
      }
      assertStatus 200 req
      assertNoHeader "Cache-Control" req

    it "200 when empty If-Unmodified-Since" $ webApp $ do
      req <- request statFile {
        requestHeaders = [("If-Unmodified-Since", "")]
      }
      assertStatus 200 req
      assertNoHeader "Cache-Control" req

    it "304 when if-modified-since matches" $ webApp $ do
      req <- request statFile {
        requestHeaders = [("If-Modified-Since", "")]
      }
      assertStatus 304 req
      assertNoHeader "Cache-Control" req
{-

  let pubApp = flip runSession $ staticApp defaultFileServerSettings {ssFolder = fileSystemLookup "tests"}
  describe "staticApp when requesting a public asset - etags" $ do
    it "200 when no etag parameters" $ pubApp $ do
      req <- request (setRawPathInfo defRequest "runtests.hs")
      assertStatus 200 req
      assertHeader "Etag" "hash" req

    it "304 when etag parameter sent" $ pubApp $ do
      req <- request (setRawPathInfo defRequest "runtests.hs") { requestHeaders  = [("If-None-Match", "hash")] }
      assertStatus 304 req
      assertNoHeader "Etag" req
      -}
