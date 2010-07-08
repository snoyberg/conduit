{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.CGI
    ( run
    , run'
    ) where

import Network.Wai
import Network.Wai.Enumerator (fromResponseBody)
import Network.Wai.Handler.Helper
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import Control.Arrow ((***))
import Data.Char (toLower)
import qualified System.IO
import Data.String (fromString)

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
    let rmethod = B.pack $ lookup' "REQUEST_METHOD" vars
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
        isSecure' =
            case map toLower $ lookup' "SERVER_PROTOCOL" vars of
                "https" -> True
                _ -> False
    let env = Request
            { requestMethod = rmethod
            , pathInfo = B.pack pinfo
            , queryString = B.pack qstring
            , serverName = B.pack servername
            , serverPort = serverport
            , requestHeaders = map (cleanupVarName *** B.pack) vars
            , isSecure = isSecure'
            , requestBody = requestBodyHandle inputH contentLength
            , errorHandler = System.IO.hPutStr System.IO.stderr
            , remoteHost = B.pack remoteHost'
            , httpVersion = "1.1" -- FIXME
            }
    res <- app env
    let h = responseHeaders res
    let h' = case lookup "Content-Type" h of
                Nothing -> ("Content-Type", "text/html; charset=utf-8")
                         : h
                Just _ -> h
    let hPut = B.hPut outputH
    hPut $ B.pack $ "Status: " ++ (show $ statusCode $ status res) ++ " "
    hPut $ statusMessage $ status res
    hPut $ B.singleton '\n'
    mapM_ (printHeader hPut) h'
    hPut $ B.singleton '\n'
    _ <- runEnumerator (fromResponseBody (responseBody res)) (myPut outputH) ()
    return ()

myPut :: System.IO.Handle -> () -> B.ByteString -> IO (Either () ())
myPut outputH _ bs = B.hPut outputH bs >> return (Right ())

printHeader :: (B.ByteString -> IO ())
            -> (ResponseHeader, B.ByteString)
            -> IO ()
printHeader f (x, y) = do
    f $ ciOriginal x
    f $ B.pack ": "
    f y
    f $ B.singleton '\n'

cleanupVarName :: String -> RequestHeader
cleanupVarName ('H':'T':'T':'P':'_':a:as) =
    fromString $ a : helper' as
  where
    helper' ('_':x:rest) = '-' : x : helper' rest
    helper' (x:rest) = toLower x : helper' rest
    helper' [] = []
cleanupVarName "CONTENT_TYPE" = "Content-Type"
cleanupVarName "CONTENT_LENGTH" = "Content-Length"
cleanupVarName "SCRIPT_NAME" = "CGI-Script-Name"
cleanupVarName x = fromString x -- FIXME remove?
