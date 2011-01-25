{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|

This module defines a generic web application interface. It is a common
protocol between web servers and web applications.

The overriding design principles here are performance and generality . To
address performance, this library is built on top of the enumerator package.
The advantages of this approach over lazy IO have been debated elsewhere.
However, helper functions like 'responseLBS' allow you to continue using lazy
IO if you so desire.

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
    , RequestHeaders
      -- ** Response header names
    , ResponseHeader
    , ResponseHeaders
      -- ** Response status code
    , Status (..)
    , status200, statusOK
    , status201, statusCreated
    , status301, statusMovedPermanently
    , status302, statusFound
    , status303, statusSeeOther
    , status400, statusBadRequest
    , status401, statusUnauthorized
    , status403, statusForbidden
    , status404, statusNotFound
    , status405, statusNotAllowed
    , status500, statusServerError
      -- * WAI interface
    , Request (..)
    , Response (..)
    , ResponseEnumerator
    , responseEnumerator
    , Application
    , Middleware
      -- * Response body smart constructors
    , responseLBS
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Char (toLower)
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Data.Enumerator (Iteratee, ($$), joinI, run_)
import qualified Data.Enumerator as E
import Data.Enumerator.IO (enumFile)
import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)
import Data.Data (Data)
import Network.Socket (SockAddr)

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
    deriving (Data, Typeable)

-- | Convert a regular bytestring to a case-insensitive bytestring.
mkCIByteString :: B.ByteString -> CIByteString
mkCIByteString bs = CIByteString bs $ B8.map toLower bs

instance Show CIByteString where
    show = show . ciOriginal
instance Read CIByteString where
    readsPrec i = map (\(x, y) -> (mkCIByteString x, y)) . readsPrec i
instance Eq CIByteString where
    x == y = ciLowerCase x == ciLowerCase y
instance Ord CIByteString where
    x <= y = ciLowerCase x <= ciLowerCase y
instance IsString CIByteString where
    fromString = mkCIByteString . fromString

-- | Headers sent from the client to the server. Note that this is a
-- case-insensitive string, as the HTTP spec specifies.
type RequestHeader = CIByteString
type RequestHeaders = [(RequestHeader, B.ByteString)]

-- | Headers sent from the server to the client. Note that this is a
-- case-insensitive string, as the HTTP spec specifies.
type ResponseHeader = CIByteString
type ResponseHeaders = [(ResponseHeader, B.ByteString)]

-- | HTTP status code; a combination of the integral code and a status message.
-- Equality is determined solely on the basis of the integral code.
data Status = Status { statusCode :: Int, statusMessage :: B.ByteString }
    deriving Show

instance Eq Status where
    x == y = statusCode x == statusCode y

-- | OK
status200, statusOK :: Status
status200 = Status 200 $ B8.pack "OK"
statusOK = status200

-- | Created
status201, statusCreated :: Status
status201 = Status 200 $ B8.pack "Created"
statusCreated = status201

-- | Moved Permanently
status301, statusMovedPermanently :: Status
status301 = Status 301 $ B8.pack "Moved Permanently"
statusMovedPermanently = status301

-- | Found
status302, statusFound :: Status
status302 = Status 302 $ B8.pack "Found"
statusFound = status302

-- | See Other
status303, statusSeeOther :: Status
status303 = Status 303 $ B8.pack "See Other"
statusSeeOther = status303

-- | Bad Request
status400, statusBadRequest :: Status
status400 = Status 400 $ B8.pack "Bad Request"
statusBadRequest = status400

-- | Unauthorized
status401, statusUnauthorized :: Status
status401 = Status 401 $ B8.pack "Unauthorized"
statusUnauthorized = status401

-- | Forbidden
status403, statusForbidden :: Status
status403 = Status 403 $ B8.pack "Forbidden"
statusForbidden = status403

-- | Not Found
status404, statusNotFound :: Status
status404 = Status 404 $ B8.pack "Not Found"
statusNotFound = status404

-- | Method Not Allowed
status405, statusNotAllowed :: Status
status405 = Status 405 $ B8.pack "Method Not Allowed"
statusNotAllowed = status405

-- | Internal Server Error
status500, statusServerError :: Status
status500 = Status 500 $ B8.pack "Internal Server Error"
statusServerError = status500

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
  -- ^ Log the given line in some method; how this is accomplished is
  -- server-dependant.
  ,  errorHandler   :: String -> IO ()
  -- | The client\'s host information.
  ,  remoteHost     :: SockAddr
  }
  deriving Typeable

data Response
    = ResponseFile Status ResponseHeaders FilePath
    | ResponseBuilder Status ResponseHeaders Builder
    | ResponseEnumerator (forall a. ResponseEnumerator a)
  deriving Typeable

type ResponseEnumerator a =
    (Status -> ResponseHeaders -> Iteratee Builder IO a) -> IO a

responseEnumerator :: Response -> ResponseEnumerator a
responseEnumerator (ResponseEnumerator e) f = e f
responseEnumerator (ResponseFile s h fp) f =
    run_ $ enumFile fp $$ joinI $ E.map fromByteString $$ f s h
responseEnumerator (ResponseBuilder s h b) f = run_ $ do
    E.yield () $ E.Chunks [b]
    f s h

responseLBS :: Status -> ResponseHeaders -> L.ByteString -> Response
responseLBS s h = ResponseBuilder s h . fromLazyByteString

type Application = Request -> Iteratee B.ByteString IO Response

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
