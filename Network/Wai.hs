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
    ( -- * WAI interface
      Request (..)
    , Response (..)
    , ResponseEnumerator
    , responseEnumerator
    , Application
    , Middleware
      -- * Response body smart constructors
    , responseLBS
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Typeable (Typeable)
import Data.Enumerator (Iteratee, ($$), joinI, run_)
import qualified Data.Enumerator as E
import Data.Enumerator.Binary (enumFile)
import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)
import Network.Socket (SockAddr)
import qualified Network.HTTP.Types as H
import Data.Text (Text)

-- | Information on the request sent by the client. This abstracts away the
-- details of the underlying implementation.
data Request = Request
  {  requestMethod  :: H.Method
  ,  httpVersion    :: H.HttpVersion
  -- | Extra path information sent by the client. The meaning varies slightly
  -- depending on backend; in a standalone server setting, this is most likely
  -- all information after the domain name. In a CGI application, this would be
  -- the information following the path to the CGI executable itself.
  ,  rawPathInfo    :: B.ByteString
  -- | If no query string was specified, this should be empty.
  ,  rawQueryString :: B.ByteString
  ,  serverName     :: B.ByteString
  ,  serverPort     :: Int
  ,  requestHeaders :: H.RequestHeaders
  -- ^ Was this request made over an SSL connection?
  ,  isSecure       :: Bool
  -- ^ Log the given line in some method; how this is accomplished is
  -- server-dependant.
  ,  errorHandler   :: String -> IO ()
  -- | The client\'s host information.
  ,  remoteHost     :: SockAddr
  -- | Path info, broken down into individual components.
  ,  pathInfo       :: [Text]
  -- | Parsed query string information
  ,  queryString    :: H.Query
  }
  deriving Typeable

data Response
    = ResponseFile H.Status H.ResponseHeaders FilePath
    | ResponseBuilder H.Status H.ResponseHeaders Builder
    | ResponseEnumerator (forall a. ResponseEnumerator a)
  deriving Typeable

type ResponseEnumerator a =
    (H.Status -> H.ResponseHeaders -> Iteratee Builder IO a) -> IO a

responseEnumerator :: Response -> ResponseEnumerator a
responseEnumerator (ResponseEnumerator e) f = e f
responseEnumerator (ResponseFile s h fp) f =
    run_ $ enumFile fp $$ joinI $ E.map fromByteString $$ f s h
responseEnumerator (ResponseBuilder s h b) f = run_ $ do
    E.yield () $ E.Chunks [b]
    f s h

responseLBS :: H.Status -> H.ResponseHeaders -> L.ByteString -> Response
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
