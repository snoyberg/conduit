{-# LANGUAGE ExistentialQuantification #-}
module Network.Wai
    ( RequestMethod (..)
    , UrlScheme (..)
    , Request (..)
    , Response (..)
    , Application
    , Middleware
    , RequestBody (..)
    , RequestBodyClass (..)
    , ResponseBody (..)
    , ResponseBodyClass (..)
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

data RequestMethod =
     OPTIONS
  |  GET
  |  HEAD
  |  POST
  |  PUT
  |  DELETE
  |  TRACE
  |  CONNECT
  deriving (Show, Read, Eq)

data UrlScheme = HTTP | HTTPS deriving (Show, Eq)

data Request = Request
  {  requestMethod  :: RequestMethod
  ,  pathInfo       :: B.ByteString
  ,  queryString    :: B.ByteString
  ,  serverName     :: B.ByteString
  ,  serverPort     :: Int
  ,  httpHeaders    :: [(B.ByteString, B.ByteString)]
  ,  urlScheme      :: UrlScheme
  ,  requestBody    :: RequestBody
  ,  errorHandler   :: String -> IO ()
  ,  remoteHost     :: String
  }

data Response = Response
  { status        :: Int
  , statusMessage :: B.ByteString
  , headers       :: [(B.ByteString, B.ByteString)]
  , body          :: ResponseBody -> IO ()
  }

type Application = Request -> IO Response

type Middleware = Application -> Application

data RequestBody = forall a. RequestBodyClass a => RequestBody a
class RequestBodyClass a where
    receiveByteString :: a -> IO (Maybe B.ByteString)
instance RequestBodyClass RequestBody where
    receiveByteString (RequestBody a) = receiveByteString a

data ResponseBody = forall a. ResponseBodyClass a => ResponseBody a
class ResponseBodyClass a where
    sendByteString :: a -> B.ByteString -> IO ()

    sendLazyByteString :: a -> L.ByteString -> IO ()
    sendLazyByteString a bs = mapM_ (sendByteString a) $ L.toChunks bs

    sendFile :: a -> FilePath -> IO ()
    -- FIXME do not use lazy I/O
    sendFile a fp = L.readFile fp >>= sendLazyByteString a
instance ResponseBodyClass ResponseBody where
    sendByteString (ResponseBody a) = sendByteString a
    sendLazyByteString (ResponseBody a) = sendLazyByteString a
    sendFile (ResponseBody a) = sendFile a
