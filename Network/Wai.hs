{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|

This module defines a generic web application interface. It is a common
protocol between web servers and web applications.

The overriding design principles here are performance, generality and type
safety. To address performance, this library is built on 'Source' for the
request body and 'Enumerator' for the response bodies. The advantages of this
approach over lazy IO have been debated elsewhere.

Nonetheless, many people find these data structures difficult to work with. For
that reason, this library includes the "Network.Wai.Enumerator" module to
provide more familiar abstractions, including lazy IO.

Generality is achieved by removing many variables commonly found in similar
projects that are not universal to all servers. The goal is that the 'Request'
object contains only data which is meaningful in all circumstances.

Unlike other approaches, this package declares many data types to assist in
type safety. This feels more inline with the general Haskell spirit.

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
      -- ** Source
    , Source (..)
      -- * Enumerator
    , Enumerator (..)
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

-- $show_read
--
-- For the data types below, you should only use the 'Show' and 'Read'
-- instances for debugging purposes. Each datatype (excepting 'UrlScheme') has
-- associated functions for converting to and from strict 'B.ByteString's;
-- these are approrpiate for generating content.

-- | HTTP request method. This data type is extensible via the Method
-- constructor. Request methods are case-sensitive, and comparison is achieved
-- by converting to a 'B.ByteString' via 'methodToBS'.
type Method = B.ByteString

-- | Version of HTTP protocol used in current request. This data type is
-- extensible via the HttpVersion constructor. Comparison is achieved by
-- converting to a 'B.ByteString' via 'httpVersionToBS'.
type HttpVersion = B.ByteString

http09 :: HttpVersion
http09 = B8.pack "0.9"

http10 :: HttpVersion
http10 = B8.pack "1.0"

http11 :: HttpVersion
http11 = B8.pack "1.1"

data CIByteString = CIByteString
    { ciOriginal :: !B.ByteString
    , ciLowerCase :: !B.ByteString
    }

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

-- | Headers sent from the client to the server. Clearly, this is not a
-- complete list of all possible headers, but rather a selection of common
-- ones. If other headers are required, they can be created with the
-- RequestHeader constructor.
--
-- The naming rules are simple: removing any hyphens from the actual name, and
-- if there is a naming conflict with a 'ResponseHeader', prefix with Req.
--
-- Equality determined by conversion via 'requestHeaderToLower'. Request
-- headers are *not* case sensitive (a change from version 0.0 of WAI).
type RequestHeader = CIByteString

-- | Headers sent from the server to the client. Clearly, this is not a
-- complete list of all possible headers, but rather a selection of common
-- ones. If other headers are required, they can be created with the
-- ResponseHeader constructor.
--
-- if there is a naming conflict with a 'ResponseHeader', prefix with Req.
--
-- Equality determined by conversion via 'responseHeaderToLower'. Response
-- headers are *not* case sensitive (a change from version 0.0 of WAI).
type ResponseHeader = CIByteString

-- | This attempts to provide the most common HTTP status codes, not all of
-- them. Use the Status constructor when you want to create a status code not
-- provided.
--
-- The 'Eq' instance tests equality based only on the numeric status code
-- value. See 'statusCode'.
data Status = Status { statusCode :: Int, statusMessage :: B.ByteString }
    deriving Show

instance Eq Status where
    x == y = statusCode x == statusCode y

status200 :: Status
status200 = Status 200 $ B8.pack "OK"

status301 :: Status
status301 = Status 301 $ B8.pack "Moved Permanently"

status302 :: Status
status302 = Status 302 $ B8.pack "Found"

status303 :: Status
status303 = Status 303 $ B8.pack "See Other"

status400 :: Status
status400 = Status 400 $ B8.pack "Bad Request"

status401 :: Status
status401 = Status 401 $ B8.pack "Unauthorized"

status403 :: Status
status403 = Status 403 $ B8.pack "Forbidden"

status404 :: Status
status404 = Status 404 $ B8.pack "Not Found"

status405 :: Status
status405 = Status 405 $ B8.pack "Method Not Allowed"

status500 :: Status
status500 = Status 500 $ B8.pack "Internal Server Error"

-- | This is a source for 'B.ByteString's. It is a function (wrapped in a
-- newtype) that will return Nothing if the data has been completely consumed,
-- or return the next 'B.ByteString' from the source along with a new 'Source'
-- to continue reading from.
--
-- Be certain not to reuse a 'Source'! It might work fine with some
-- implementations of 'Source', while causing bugs with others.
newtype Source = Source { runSource :: IO (Maybe (B.ByteString, Source)) }

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
-- 'Enumerator's are not required to be resumable. That is to say, the
-- 'Enumerator' may only be called once. While this requirement puts a bit of a
-- strain on the caller in some situations, it saves a large amount of
-- complication- and thus performance- on the producer.
newtype Enumerator = Enumerator { runEnumerator :: forall a.
              (a -> B.ByteString -> IO (Either a a))
                 -> a
                 -> IO (Either a a)
}

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
  ,  isSecure       :: Bool
  ,  requestBody    :: Source
  ,  errorHandler   :: String -> IO ()
  -- | The client\'s host information.
  ,  remoteHost     :: B.ByteString
  }

data ResponseBody = ResponseFile FilePath
                  | ResponseEnumerator Enumerator
                  | ResponseLBS L.ByteString

data Response = Response
  { status          :: Status
  , responseHeaders :: [(ResponseHeader, B.ByteString)]
  -- | A common optimization is to use the sendfile system call when sending
  -- files from the disk. This datatype facilitates this optimization; if
  -- 'Left' is returned, the server will send the file from the disk by
  -- whatever means it wishes. If 'Right', it will call the 'Enumerator'.
  , responseBody    :: ResponseBody
  }

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
