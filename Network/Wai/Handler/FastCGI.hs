module Network.Wai.Handler.FastCGI
    ( run
    ) where

import qualified Network.Wai as W
import Network.FastCGI
import Control.Concurrent (forkIO)
import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified System.IO
import Control.Arrow ((***))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Network.Wai.Enumerator (fromEitherFile)

run :: W.Application -> IO ()
run = acceptLoop forkIO . conv

safeRead :: Read a => a -> String -> a
safeRead d s =
  case reads s of
    ((x, _):_) -> x
    [] -> d

lookup' :: String -> [(String, String)] -> String
lookup' key pairs = fromMaybe "" $ lookup key pairs

conv :: W.Application -> FastCGI ()
conv app = do
    vars <- getAllRequestVariables
    let rmethod = W.methodFromBS $ B.pack $ lookup' "REQUEST_METHOD" vars
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
            case map toLower $ lookup' "SERVER_PROTOCOL" vars of -- FIXME get httpVersion too
                "https" -> W.HTTPS
                _ -> W.HTTP
    state <- ask
    let env = W.Request
            { W.requestMethod = rmethod
            , W.pathInfo = B.pack pinfo
            , W.queryString = B.pack qstring
            , W.serverName = B.pack servername
            , W.serverPort = serverport
            , W.requestHeaders = map (cleanupVarName *** B.pack) vars
            , W.urlScheme = urlScheme'
            , W.requestBody = requestBody state contentLength
            , W.errorHandler = System.IO.hPutStr System.IO.stderr
            , W.remoteHost = B.pack remoteHost'
            , W.httpVersion = W.HttpVersion B.empty
            }
    res <- liftIO $ app env
    setResponseStatus $ W.statusCode $ W.status res
    mapM_ setHeader $ W.responseHeaders res
    _ <- liftIO $ W.runEnumerator
                    (fromEitherFile (W.responseBody res))
                    (myPut state)
                    ()
    return ()

cleanupVarName :: String -> W.RequestHeader
cleanupVarName ('H':'T':'T':'P':'_':a:as) =
  W.requestHeaderFromBS $ B.pack $ a : helper' as where
    helper' ('_':x:rest) = '-' : x : helper' rest
    helper' (x:rest) = toLower x : helper' rest
    helper' [] = []
cleanupVarName "CONTENT_TYPE" = W.ReqContentType
cleanupVarName "CONTENT_LENGTH" = W.ReqContentLength
cleanupVarName "SCRIPT_NAME" = W.requestHeaderFromBS $ B.pack "CGI-Script-Name"
cleanupVarName x = W.requestHeaderFromBS $ B.pack x -- FIXME remove?

requestBody :: FastCGIState -> Int -> W.Source
requestBody _ 0 = W.Source $ return Nothing
requestBody state len = W.Source $ do
    bs <- runReaderT (fGet defaultChunkSize) state
    let newLen = len - B.length bs
    return $ Just (bs, requestBody state newLen)

setHeader :: MonadFastCGI m => (W.ResponseHeader, B.ByteString) -> m ()
setHeader (k, v) =
    setResponseHeader
       k'
       (B.unpack v)
  where
    k'
      | k == W.ContentType = HttpContentType -- avoid double-sent c-type
      | otherwise = HttpExtensionHeader $ B.unpack $ W.responseHeaderToBS k


myPut :: FastCGIState -> () -> B.ByteString -> IO (Either () ())
myPut state _ bs = do
    runReaderT (fPut bs) state
    return $ Right ()
