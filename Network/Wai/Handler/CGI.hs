module Network.Wai.Handler.CGI
    ( run
    , run'
    ) where

import Network.Wai
import Network.Wai.Enumerator (fromEitherFile)
import Network.Wai.Handler.Helper
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import Control.Arrow ((***))
import Data.Char (toLower)
import qualified System.IO

safeRead :: Read a => a -> String -> a
safeRead d s =
  case reads s of
    ((x, _):_) -> x
    [] -> d

lookup' :: String -> [(String, String)] -> String
lookup' key pairs = fromMaybe "" $ lookup key pairs

run :: Application -> IO ()
run app = do
    vars <- getEnvironment
    run' vars System.IO.stdin System.IO.stdout app

run' :: [(String, String)] -- ^ all variables
     -> System.IO.Handle -- ^ responseBody of input
     -> System.IO.Handle -- ^ destination for output
     -> Application
     -> IO ()
run' vars inputH outputH app = do
    let rmethod = safeRead GET $ lookup' "REQUEST_METHOD" vars
        pinfo = lookup' "PATH_INFO" vars
        qstring = lookup' "QUERY_STRING" vars
        servername = lookup' "SERVER_NAME" vars
        serverport = safeRead 80 $ lookup' "SERVER_PORT" vars
        contentLength = safeRead 0 $ lookup' "CONTENT_LENGTH" vars
        remoteHost' =
            case lookup "REMOTE_HOST" vars of
                Just x -> x
                Nothing ->
                    case lookup "REMOTE_ADDR" vars of
                        Just x -> x
                        Nothing -> ""
        urlScheme' =
            case map toLower $ lookup' "SERVER_PROTOCOL" vars of
                "https" -> HTTPS
                _ -> HTTP
    let env = Request
            { requestMethod = rmethod
            , pathInfo = B.pack pinfo
            , queryString = B.pack qstring
            , serverName = B.pack servername
            , serverPort = serverport
            , requestHeaders = map (cleanupVarName *** B.pack) vars
            , urlScheme = urlScheme'
            , requestBody = Source contentLength $ requestBodyHandle inputH
            , errorHandler = System.IO.hPutStr System.IO.stderr
            , remoteHost = B.pack remoteHost'
            , httpVersion = HttpVersion B.empty
            }
    res <- app env
    let h = responseHeaders res
    let h' = case lookup ContentType h of
                Nothing -> (ContentType, B.pack "text/html; charset=utf-8")
                         : h
                Just _ -> h
    let hPut = B.hPut outputH
    hPut $ B.pack $ "Status: " ++ (show $ statusCode $ status res) ++ " "
    hPut $ statusMessage $ status res
    hPut $ B.singleton '\n'
    mapM_ (printHeader hPut) h'
    hPut $ B.singleton '\n'
    _ <- runEnumerator (fromEitherFile (responseBody res)) (myPut outputH) ()
    return ()

myPut :: System.IO.Handle -> () -> B.ByteString -> IO (Either () ())
myPut outputH _ bs = B.hPut outputH bs >> return (Right ())

printHeader :: (B.ByteString -> IO ())
            -> (ResponseHeader, B.ByteString)
            -> IO ()
printHeader f (x, y) = do
    f $ responseHeaderToBS x
    f $ B.pack ": "
    f y
    f $ B.singleton '\n'

cleanupVarName :: String -> RequestHeader
cleanupVarName ('H':'T':'T':'P':'_':a:as) =
  requestHeaderFromBS $ B.pack $ a : helper' as where
    helper' ('_':x:rest) = '-' : x : helper' rest
    helper' (x:rest) = toLower x : helper' rest
    helper' [] = []
cleanupVarName "CONTENT_TYPE" = ReqContentType
cleanupVarName "CONTENT_LENGTH" = ReqContentLength
cleanupVarName "SCRIPT_NAME" = RequestHeader $ B.pack "CGI-Script-Name"
cleanupVarName x = requestHeaderFromBS $ B.pack x -- FIXME remove?
