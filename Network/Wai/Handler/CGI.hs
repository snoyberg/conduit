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
import qualified Data.ByteString.Lazy as L
import Control.Arrow ((***))
import Data.Char (toLower)
import qualified System.IO
import qualified Data.String as String
import Data.Enumerator
    ( Enumerator, Step (..), Stream (..), continue, yield
    , enumList, ($$), joinI
    )
import Data.Monoid (mconcat)
import Blaze.ByteString.Builder (fromByteString, toLazyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Control.Monad.IO.Class (liftIO)

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

runSendfile :: B.ByteString -- ^ sendfile header
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
     -> (B.ByteString -> IO ()) -- ^ destination for output FIXME use an iteratee
     -> Maybe B.ByteString -- ^ does the server support the X-Sendfile header?
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
    case (xsendfile, res) of
        (Just sf, ResponseFile s hs fp) ->
            mapM_ outputH $ L.toChunks $ toLazyByteString $ sfBuilder s hs sf fp
        _ -> responseEnumerator res $ \s hs ->
            joinI $ enumList 1 [headers s hs, fromChar '\n'] $$ builderIter
  where
    headers s hs = mconcat (map header $ status s : map header' (fixHeaders hs))
    status (Status i m) = (fromByteString "Status", mconcat
        [ fromString $ show i
        , fromChar ' '
        , fromByteString m
        ])
    header' (x, y) = (fromByteString $ ciOriginal x, fromByteString y)
    header (x, y) = mconcat
        [ x
        , fromByteString ": "
        , y
        , fromChar '\n'
        ]
    sfBuilder s hs sf fp = mconcat
        [ headers s hs
        , header $ (fromByteString sf, fromString fp)
        , fromChar '\n'
        , fromByteString sf
        , fromByteString " not supported"
        ]
    bsStep = Continue bsStep'
    bsStep' EOF = yield () EOF
    bsStep' (Chunks []) = continue bsStep'
    bsStep' (Chunks bss) = liftIO (mapM_ outputH bss) >> continue bsStep'
    builderIter = builderToByteString bsStep
    fixHeaders h =
        case lookup "content-type" h of
            Nothing -> ("Content-Type", "text/html; charset=utf-8") : h
            Just _ -> h

cleanupVarName :: String -> RequestHeader
cleanupVarName ('H':'T':'T':'P':'_':a:as) =
    String.fromString $ a : helper' as
  where
    helper' ('_':x:rest) = '-' : x : helper' rest
    helper' (x:rest) = toLower x : helper' rest
    helper' [] = []
cleanupVarName "CONTENT_TYPE" = "Content-Type"
cleanupVarName "CONTENT_LENGTH" = "Content-Length"
cleanupVarName "SCRIPT_NAME" = "CGI-Script-Name"
cleanupVarName x = String.fromString x -- FIXME remove?
