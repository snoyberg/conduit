{-# LANGUAGE Rank2Types #-}
module Network.Wai
    ( -- * Data types
      -- ** Request method
      Method (..)
    , methodFromBS
    , methodToBS
      -- ** URL scheme (http versus https)
    , UrlScheme (..)
      -- ** HTTP protocol versions
    , HttpVersion (..)
    , httpVersionFromBS
    , httpVersionToBS
      -- ** Request header names
    , RequestHeader (..)
    , requestHeaderFromBS
    , requestHeaderToBS
      -- ** Response header names
    , ResponseHeader (..)
    , responseHeaderFromBS
    , responseHeaderToBS
      -- ** Response status code
    , Status (..)
    , statusCode
    , statusMessage
      -- * Enumerator
    , Enumerator
      -- * WAI interface
    , Request (..)
    , Response (..)
    , Application
    , Middleware
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- | Please do not use the Show and Read instances for anything other than
-- debugging purposes. Instead, the 'methodFromBS' and 'methodToBS' provide a
-- more appropriate interface.
data Method =
    OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | Method B.ByteString
  deriving (Show, Read, Eq)

methodFromBS :: B.ByteString -> Method
methodFromBS bs
    | B.length bs <= 7 = case B8.unpack bs of
        "OPTIONS" -> OPTIONS
        "GET" -> GET
        "HEAD" -> HEAD
        "POST" -> POST
        "PUT" -> PUT
        "DELETE" -> DELETE
        "TRACE" -> TRACE
        "CONNECT" -> CONNECT
        _ -> Method bs
    | otherwise = Method bs

methodToBS :: Method -> B.ByteString
methodToBS OPTIONS = B8.pack "OPTIONS"
methodToBS GET = B8.pack "GET"
methodToBS HEAD = B8.pack "HEAD"
methodToBS POST = B8.pack "POST"
methodToBS PUT = B8.pack "PUT"
methodToBS DELETE = B8.pack "DELETE"
methodToBS TRACE = B8.pack "TRACE"
methodToBS CONNECT = B8.pack "CONNECT"
methodToBS (Method bs) = bs

data UrlScheme = HTTP | HTTPS deriving (Show, Eq)

-- | The value of HttpVersion should be the information trailing HTTP/.
data HttpVersion =
      Http09
    | Http10
    | Http11
    | HttpVersion B.ByteString
    deriving (Show, Eq)

httpVersionFromBS :: B.ByteString -> HttpVersion
httpVersionFromBS bs
    | B.length bs == 3 = case B8.unpack bs of
        "0.9" -> Http09
        "1.0" -> Http10
        "1.1" -> Http11
        _ -> HttpVersion bs
    | otherwise = HttpVersion bs

httpVersionToBS :: HttpVersion -> B.ByteString
httpVersionToBS Http09 = B8.pack "0.9"
httpVersionToBS Http10 = B8.pack "1.0"
httpVersionToBS Http11 = B8.pack "1.1"
httpVersionToBS (HttpVersion bs) = bs

data RequestHeader =
      Accept
    | AcceptCharset
    | AcceptEncoding
    | AcceptLanguage
    | Authorization
    | Cookie
    | ReqContentLength
    | ReqContentType
    | Host
    | Referer
    | RequestHeader B.ByteString
    deriving (Show)

instance Eq RequestHeader where
    x == y = requestHeaderToBS x == requestHeaderToBS y

requestHeaderFromBS :: B.ByteString -> RequestHeader
requestHeaderFromBS bs = case B8.unpack bs of
    "Accept" -> Accept
    "Accept-Charset" -> AcceptCharset
    "Accept-Encoding" -> AcceptEncoding
    "Accept-Language" -> AcceptLanguage
    "Authorization" -> Authorization
    "Cookie" -> Cookie
    "Content-Length" -> ReqContentLength
    "Content-Type" -> ReqContentType
    "Host" -> Host
    "Referer" -> Referer
    _ -> RequestHeader bs

requestHeaderToBS :: RequestHeader -> B.ByteString
requestHeaderToBS Accept = B8.pack "Accept"
requestHeaderToBS AcceptCharset = B8.pack "Accept-Charset"
requestHeaderToBS AcceptEncoding = B8.pack "Accept-Encoding"
requestHeaderToBS AcceptLanguage = B8.pack "Accept-Language"
requestHeaderToBS Authorization = B8.pack "Authorization"
requestHeaderToBS Cookie = B8.pack "Cookie"
requestHeaderToBS ReqContentLength = B8.pack "Content-Length"
requestHeaderToBS ReqContentType = B8.pack "Content-Type"
requestHeaderToBS Host = B8.pack "Host"
requestHeaderToBS Referer = B8.pack "Referer"
requestHeaderToBS (RequestHeader bs) = bs

data ResponseHeader =
      ContentEncoding
    | ContentLanguage
    | ContentLength
    | ContentDisposition
    | ContentType
    | Expires
    | Location
    | Server
    | SetCookie
    | ResponseHeader B.ByteString
     deriving (Show)

instance Eq ResponseHeader where
    x == y = responseHeaderToBS x == responseHeaderToBS y


responseHeaderFromBS :: B.ByteString -> ResponseHeader
responseHeaderFromBS bs = case B8.unpack bs of
    "Content-Encoding" -> ContentEncoding
    "Content-Language" -> ContentLanguage
    "Content-Length" -> ContentLength
    "Content-Disposition" -> ContentDisposition
    "Content-Type" -> ContentType
    "Expires" -> Expires
    "Location" -> Location
    "Server" -> Server
    "Set-Cookie" -> SetCookie
    _ -> ResponseHeader bs

responseHeaderToBS :: ResponseHeader -> B.ByteString
responseHeaderToBS ContentEncoding = B8.pack "Content-Encoding"
responseHeaderToBS ContentLanguage = B8.pack "Content-Language"
responseHeaderToBS ContentLength = B8.pack "Content-Length"
responseHeaderToBS ContentDisposition = B8.pack "Content-Disposition"
responseHeaderToBS ContentType = B8.pack "Content-Type"
responseHeaderToBS Expires = B8.pack "Expires"
responseHeaderToBS Location = B8.pack "Location"
responseHeaderToBS Server = B8.pack "Server"
responseHeaderToBS SetCookie = B8.pack "Set-Cookie"
responseHeaderToBS (ResponseHeader bs) = bs

-- | This attempts to provide the most common HTTP status codes, not all of
-- them. Use the 'Status' constructor when you want to create a status code not
-- provided.
--
-- The 'Eq' instance tests equality based only on the numeric status code
-- value. See 'statusCode'.
data Status =
      Status200
    | Status301
    | Status302
    | Status303
    | Status400
    | Status401
    | Status403
    | Status404
    | Status405
    | Status500
    | Status Int B.ByteString
    deriving Show

instance Eq Status where
    x == y = statusCode x == statusCode y

statusCode :: Status -> Int
statusCode Status200 = 200
statusCode Status301 = 301
statusCode Status302 = 302
statusCode Status303 = 303
statusCode Status400 = 400
statusCode Status401 = 401
statusCode Status403 = 403
statusCode Status404 = 404
statusCode Status405 = 405
statusCode Status500 = 500
statusCode (Status i _) = i

statusMessage :: Status -> B.ByteString
statusMessage Status200 = B8.pack "OK"
statusMessage Status301 = B8.pack "Moved Permanently"
statusMessage Status302 = B8.pack "Found"
statusMessage Status303 = B8.pack "See Other"
statusMessage Status400 = B8.pack "Bad Request"
statusMessage Status401 = B8.pack "Unauthorized"
statusMessage Status403 = B8.pack "Forbidden"
statusMessage Status404 = B8.pack "Not Found"
statusMessage Status405 = B8.pack "Method Not Allowed"
statusMessage Status500 = B8.pack "Internal Server Error"
statusMessage (Status _ m) = m

type Enumerator a = (a -> B.ByteString -> IO (Either a a))
                 -> a
                 -> IO (Either a a)

data Request = Request
  {  requestMethod  :: Method
  ,  httpVersion    :: HttpVersion
  ,  pathInfo       :: B.ByteString
  ,  queryString    :: B.ByteString
  ,  serverName     :: B.ByteString
  ,  serverPort     :: Int
  ,  httpHeaders    :: [(RequestHeader, B.ByteString)]
  ,  urlScheme      :: UrlScheme
  ,  requestBody    :: forall a. Enumerator a
  ,  errorHandler   :: String -> IO ()
  ,  remoteHost     :: B.ByteString
  }

data Response = Response
  { status        :: Status
  , headers       :: [(ResponseHeader, B.ByteString)]
  , body          :: forall a. Either FilePath (Enumerator a)
  }

type Application = Request -> IO Response

type Middleware = Application -> Application
