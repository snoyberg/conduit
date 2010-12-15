{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Network.Wai.Handler.CGI
    ( run
    , run'
    , run''
    , runSendfile
    ) where

import Network.Wai
import Network.Wai.Handler.Helper
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import Control.Arrow ((***))
import Data.Char (toLower)
import qualified System.IO
import Data.String (fromString)
import Data.Enumerator (Enumerator)

safeRead :: Read a => a -> String -> a
safeRead d s =
  case reads s of
    ((x, _):_) -> x
    [] -> d

lookup' :: String -> [(String, String)] -> String
lookup' key pairs = fromMaybe "" $ lookup key pairs

run :: Application () -> IO ()
run app = do
    vars <- getEnvironment
    let input = requestBodyHandle System.IO.stdin
        output = B.hPut System.IO.stdout
    run'' vars input output Nothing app

runSendfile :: String -- ^ sendfile header
            -> Application () -> IO ()
runSendfile sf app = do
    vars <- getEnvironment
    let input = requestBodyHandle System.IO.stdin
        output = B.hPut System.IO.stdout
    run'' vars input output (Just sf) app

run' :: [(String, String)] -- ^ all variables
     -> System.IO.Handle -- ^ responseBody of input
     -> System.IO.Handle -- ^ destination for output
     -> Application ()
     -> IO ()
run' vars inputH outputH app = do
    let input = requestBodyHandle inputH
        output = B.hPut outputH
    run'' vars input output Nothing app

run'' :: [(String, String)] -- ^ all variables
     -> (forall a. Int -> Enumerator B.ByteString IO a) -- ^ responseBody of input
     -> (B.ByteString -> IO ()) -- ^ destination for output
     -> Maybe String -- ^ does the server support the X-Sendfile header?
     -> Application ()
     -> IO ()
run'' vars inputH outputH xsendfile app = do
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
            , requestBody = inputH contentLength
            , errorHandler = System.IO.hPutStr System.IO.stderr
            , remoteHost = B.pack remoteHost'
            , httpVersion = "1.1" -- FIXME
            }
    res <- app env
    error "FIXME"
    {-
    outputH $ B.pack $ "Status: " ++ (show $ statusCode $ status res) ++ " "
    outputH $ statusMessage $ status res
    outputH $ B.singleton '\n'
    mapM_ (printHeader hPut) h'
    case (xsendfile, responseBody res) of
        (Just sf, ResponseFile fp) ->
            hPut $ B.pack $ concat
                [ sf
                , ": "
                , fp
                , "\n\n"
                , sf
                , " not supported"
                ]
        _ -> do
            hPut $ B.singleton '\n'
            _ <- runEnumerator (fromResponseBody (responseBody res))
                               (myPut outputH) ()
            return ()
    -}

fixHeaders h =
    case lookup "content-type" h of
        Nothing -> ("Content-Type", "text/html; charset=utf-8") : h
        Just _ -> h

myPut :: (B.ByteString -> IO ()) -> () -> B.ByteString -> IO (Either () ())
myPut output _ bs = output bs >> return (Right ())

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
