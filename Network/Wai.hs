{-# LANGUAGE Rank2Types #-}
{-|

This module defines a generic web application interface. It is a common
protocol between web servers and web applications.

The overriding design principles here are performance, generality and type
safety. To address performance, this library is built on 'Enumerator's for the
request and response bodies. The advantages of this approach over lazy IO has
been debated elsewhere.

Nonetheless, many people find 'Enumerator's difficult to work with. For that
reason, this library includes the "Network.Wai.Enumerator" module to provide
more familiar abstractions, including lazy IO.

Generality is achieved by removing many variables commonly found in similar
projects that are not universal to all servers. The goal is that the 'Request'
object contains only data which is meaningful in all circumstances.

Unlike other approaches, this package declares many data types to assist in
type safety. This feels more in the general Haskell spirit.

A final note: please remember when using this package that, while your
application my compile without a hitch against many different servers, there
are other considerations to be taken when moving to a new backend. For example,
if you transfer from a CGI application to a FastCGI one, you might suddenly
find you have a memory leak. Conversely, a FastCGI application would be
well served to preload all templates from disk when first starting; this
would kill the performance of a CGI application.

-}
module Network.Wai
    ( -- * Data types

      -- $show_read
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

-- $show_read
--
-- For the data types below, you should only use the 'Show' and 'Read'
-- instances for debugging purposes. Each datatype (excepting 'UrlScheme') has
-- associated functions for converting to and from strict 'B.ByteString's;
-- these are approrpiate for generating content.

-- | HTTP request method. This data type is extensible via the Method
-- constructor. Request methods are case-sensitive, and comparison is achieved
-- by converting to a 'B.ByteString' via 'methodToBS'.
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
  deriving (Show, Read)

instance Eq Method where
    x == y = methodToBS x == methodToBS y

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

data UrlScheme = HTTP | HTTPS
    deriving (Show, Eq)

-- | Version of HTTP protocol used in current request. This data type is
-- extensible via the HttpVersion constructor. Comparison is achieved by
-- converting to a 'B.ByteString' via 'httpVersionToBS'.
data HttpVersion =
      Http09
    | Http10
    | Http11
    | HttpVersion B.ByteString
    deriving (Show, Read)

instance Eq HttpVersion where
    x == y = httpVersionToBS x == httpVersionToBS y

-- | This function takes the information after \"HTTP\/\". For example:
--
-- @ 'httpVersionFromBS' ('B8.pack' \"1.0\") == 'Http10' @
httpVersionFromBS :: B.ByteString -> HttpVersion
httpVersionFromBS bs
    | B.length bs == 3 = case B8.unpack bs of
        "0.9" -> Http09
        "1.0" -> Http10
        "1.1" -> Http11
        _ -> HttpVersion bs
    | otherwise = HttpVersion bs

-- | Returns the version number, for example:
--
-- @ 'B8.unpack' ('httpVersionToBS' 'Http10') == \"1.0\" @
httpVersionToBS :: HttpVersion -> B.ByteString
httpVersionToBS Http09 = B8.pack "0.9"
httpVersionToBS Http10 = B8.pack "1.0"
httpVersionToBS Http11 = B8.pack "1.1"
httpVersionToBS (HttpVersion bs) = bs

-- | Headers sent from the client to the server. Clearly, this is not a
-- complete list of all possible headers, but rather a selection of common
-- ones. If other headers are required, they can be created with the
-- RequestHeader constructor.
--
-- The naming rules are simple: removing any hyphens from the actual name, and
-- if there is a naming conflict with a 'ResponseHeader', prefix with Req.
--
-- Equality determined by conversion via 'requestHeaderToBS'. Remember, headers
-- are case sensitive.
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
    deriving (Show, Read)

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


-- | Headers sent from the server to the client. Clearly, this is not a
-- complete list of all possible headers, but rather a selection of common
-- ones. If other headers are required, they can be created with the
-- ResponseHeader constructor.
--
-- if there is a naming conflict with a 'ResponseHeader', prefix with Req.
--
-- Equality determined by conversion via 'responseHeaderToBS'. Remember,
-- headers are case sensitive.
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
-- them. Use the Status constructor when you want to create a status code not
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

-- | An enumerator is a data producer. It takes two arguments: a function to
-- enumerate over (the iteratee) and an accumulating parameter. As the
-- enumerator produces output, it calls the iteratee, thereby avoiding the need
-- to allocate large amounts of memory for storing the entire piece of data.
--
-- Normally in Haskell, we can achieve the same results with laziness. For
-- example, an inifinite list does not require inifinite memory storage; we
-- simply get away with thunks. However, when operating in the IO monad, we do
-- not have this luxury. There are other approaches, such as lazy I\/O. If you
-- would like to program in this manner, please see
-- "Network.Wai.Enumerator", in particular toLBS.
--
-- That said, let's address the details of this particular enumerator
-- implementation. You'll notice that the iteratee is a function that takes two
-- arguments and returns an 'Either' value. The second argument is simply the
-- piece of data generated by the enumerator. The 'Either' value at the end is
-- a means to alert the enumerator whether to continue or not. If it returns
-- 'Left', then the enumeration should cease. If it returns 'Right', it should
-- continue.
--
-- The accumulating parameter (a) has meaning only to the iteratee; the
-- enumerator simply passes it around. The enumerator itself also returns an
-- 'Either' value; a 'Right' means the enumerator ran to completion, while a
-- 'Left' indicates early termination was requested by the iteratee.
--
-- 'Enumerator's are used twice in the WAI: once in the 'Request' datatype to
-- access the request body, and once in the 'Response' datatype for the
-- application to return the body of the response.
type Enumerator a = (a -> B.ByteString -> IO (Either a a))
                 -> a
                 -> IO (Either a a)

-- | Information on the request sent by the client. This abstracts away the
-- details of the underlying implementation.
data Request = Request
  {  requestMethod  :: Method
  ,  httpVersion    :: HttpVersion
  -- | Extra path information sent by the client. The meaning varies slightly
  -- depending on backend; in a standalone server setting, this is most likely
  -- all information after the domain name. In a CGI application, this would be
  -- the information following the path to the CGI executable itself.
  ,  pathInfo       :: B.ByteString
  ,  queryString    :: B.ByteString
  ,  serverName     :: B.ByteString
  ,  serverPort     :: Int
  ,  httpHeaders    :: [(RequestHeader, B.ByteString)]
  ,  urlScheme      :: UrlScheme
  ,  requestBody    :: forall a. Enumerator a
  ,  errorHandler   :: String -> IO ()
  -- | The client\'s host information.
  ,  remoteHost     :: B.ByteString
  }

data Response = Response
  { status        :: Status
  , headers       :: [(ResponseHeader, B.ByteString)]
  -- | A common optimization is to use the sendfile system call when sending
  -- files from the disk. This datatype facilitates this optimization; if
  -- 'Left' is returns, the server will send the file from the disk by whatever
  -- means it wishes. If 'Right', it will call the 'Enumerator'.
  , body          :: forall a. Either FilePath (Enumerator a)
  }

type Application = Request -> IO Response

-- | Middleware is a component that sits between the server and application. It can do such tasks as GZIP encoding or response caching. What follows is the general definition of middleware, though a middleware author should feel free to modify this.
--
-- As an example of an alternate type for middleware, suppose you write a
-- function to load up session information. The session information is simply a
-- string map \[(String, String)\]. A logical type signatures for this middleware
-- might be:
--
-- @ loadSession :: ([(String, String)] -> Application) -> Application @
--
-- Here, instead of taking a standard 'Application' as its first argument, the
-- middleware takes a function which consumes the session information as well.
type Middleware = Application -> Application
