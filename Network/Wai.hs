{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|

This module defines a generic web application interface. It is a common
protocol between web servers and web applications.

The overriding design principles here are performance and generality . To
address performance, this library is built on 'Source' for the request body and
'Enumerator' for the response bodies. The advantages of this approach over lazy
IO have been debated elsewhere.

Nonetheless, many people find these data structures difficult to work with. For
that reason, this library includes the "Network.Wai.Enumerator" module to
provide more familiar abstractions, including lazy IO.

Generality is achieved by removing many variables commonly found in similar
projects that are not universal to all servers. The goal is that the 'Request'
object contains only data which is meaningful in all circumstances.

A final note: please remember when using this package that, while your
application may compile without a hitch against many different servers, there
are other considerations to be taken when moving to a new backend. For example,
if you transfer from a CGI application to a FastCGI one, you might suddenly
find you have a memory leak. Conversely, a FastCGI application would be
well served to preload all templates from disk when first starting; this
would kill the performance of a CGI application.

-}
module Network.Wai
    ( -- * Data types

      -- ** Request method
      Method
      -- ** HTTP protocol versions
    , HttpVersion
    , http09
    , http10
    , http11
      -- ** Case-insensitive byte strings
    , CIByteString (..)
    , mkCIByteString
      -- ** Request header names
    , RequestHeader
      -- ** Response header names
    , ResponseHeader
      -- ** Response status code
    , Status (..)
    , status200
    , status301
    , status302
    , status303
    , status400
    , status401
    , status403
    , status404
    , status405
    , status500
      -- ** Response body
    , ResponseBody (..)
      -- * WAI interface
    , Request (..)
    , Response (..)
    , Application
    , Middleware
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Char (toLower)
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Data.Enumerator (Enumerator, Iteratee)
import Blaze.ByteString.Builder (Builder)

-- | HTTP request method. Since the HTTP protocol allows arbitrary request
-- methods, we leave this open as a 'B.ByteString'. Please note the request
-- methods are case-sensitive.
type Method = B.ByteString

-- | Version of HTTP protocol used in current request. The value given here
-- should be everything following the \"HTTP/\" line in a request. In other
-- words, HTTP\/1.1 -> \"1.1\", HTTP\/1.0 -> \"1.0\".
type HttpVersion = B.ByteString

-- | HTTP/0.9
http09 :: HttpVersion
http09 = B8.pack "0.9"

-- | HTTP/1.0
http10 :: HttpVersion
http10 = B8.pack "1.0"

-- | HTTP/1.1
http11 :: HttpVersion
http11 = B8.pack "1.1"

-- | A case insensitive bytestring, where the 'Eq' and 'Ord' instances do
-- comparisons based on the lower-cased version of this string. For efficiency,
-- this datatype contains both the original and lower-case version of the
-- string; this means there is no need to lower-case the bytestring for every
-- comparison.
--
-- Please note that this datatype has an 'IsString' instance, which can allow
-- for very concise code when using the OverloadedStrings language extension.
data CIByteString = CIByteString
    { ciOriginal :: !B.ByteString
    , ciLowerCase :: !B.ByteString
    }

-- | Convert a regular bytestring to a case-insensitive bytestring.
mkCIByteString :: B.ByteString -> CIByteString
mkCIByteString bs = CIByteString bs $ B8.map toLower bs

instance Show CIByteString where
    show = show . ciOriginal
instance Eq CIByteString where
    x == y = ciLowerCase x == ciLowerCase y
instance Ord CIByteString where
    x <= y = ciLowerCase x <= ciLowerCase y
instance IsString CIByteString where
    fromString = mkCIByteString . fromString

-- | Headers sent from the client to the server. Note that this is a
-- case-insensitive string, as the HTTP spec specifies.
type RequestHeader = CIByteString

-- | Headers sent from the server to the client. Note that this is a
-- case-insensitive string, as the HTTP spec specifies.
type ResponseHeader = CIByteString

-- | HTTP status code; a combination of the integral code and a status message.
-- Equality is determined solely on the basis of the integral code.
data Status = Status { statusCode :: Int, statusMessage :: B.ByteString }
    deriving Show

instance Eq Status where
    x == y = statusCode x == statusCode y

-- | OK
status200 :: Status
status200 = Status 200 $ B8.pack "OK"

-- | Moved Permanently
status301 :: Status
status301 = Status 301 $ B8.pack "Moved Permanently"

-- | Found
status302 :: Status
status302 = Status 302 $ B8.pack "Found"

-- | See Other
status303 :: Status
status303 = Status 303 $ B8.pack "See Other"

-- | Bad Request
status400 :: Status
status400 = Status 400 $ B8.pack "Bad Request"

-- | Unauthorized
status401 :: Status
status401 = Status 401 $ B8.pack "Unauthorized"

-- | Forbidden
status403 :: Status
status403 = Status 403 $ B8.pack "Forbidden"

-- | Not Found
status404 :: Status
status404 = Status 404 $ B8.pack "Not Found"

-- | Method Not Allowed
status405 :: Status
status405 = Status 405 $ B8.pack "Method Not Allowed"

-- | Internal Server Error
status500 :: Status
status500 = Status 500 $ B8.pack "Internal Server Error"

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
  -- | If no query string was specified, this should be empty.
  ,  queryString    :: B.ByteString
  ,  serverName     :: B.ByteString
  ,  serverPort     :: Int
  ,  requestHeaders :: [(RequestHeader, B.ByteString)]
  -- ^ Was this request made over an SSL connection?
  ,  isSecure       :: Bool
  ,  requestBody    :: forall a. Enumerator B.ByteString IO a
  -- ^ Log the given line in some method; how this is accomplished is
  -- server-dependant.
  ,  errorHandler   :: String -> IO ()
  -- | The client\'s host information.
  ,  remoteHost     :: B.ByteString
  }
  deriving Typeable

-- | The response body returned to the server from the application. We provide
-- three separate constructors as optimizations:
--
-- * 'ResponseEnumerator' is the most general type, allowing constant-memory
-- production of a response, even in the presence of interleaved I\/O actions.
--
-- * 'ResponseFile' serves a static file from the filesystem. Many servers use
-- a sendfile system call to optimize this type of serving, making this a huge
-- performance gain.
--
-- * 'ResponseLBS'. Often times, we wish to return a response that includes no
-- interleaved I\/O. In this case, we can use Haskell's natural laziness to our
-- advantage, and represent the response as a lazy bytestring.
data ResponseBody
    = ResponseFile FilePath
    | ResponseEnumerator (Iteratee Builder IO () -> IO ())
    | ResponseLBS L.ByteString
  deriving Typeable

data Response = Response
  { status          :: Status
  , responseHeaders :: [(ResponseHeader, B.ByteString)]
  -- | A common optimization is to use the sendfile system call when sending
  -- files from the disk. This datatype facilitates this optimization; if
  -- 'Left' is returned, the server will send the file from the disk by
  -- whatever means it wishes. If 'Right', it will call the 'Enumerator'.
  , responseBody    :: ResponseBody
  }
  deriving Typeable

type Application = Request -> IO Response

-- | Middleware is a component that sits between the server and application. It
-- can do such tasks as GZIP encoding or response caching. What follows is the
-- general definition of middleware, though a middleware author should feel
-- free to modify this.
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
