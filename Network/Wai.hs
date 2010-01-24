{-# LANGUAGE ExistentialQuantification #-}
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

data Request = Request
  {  requestMethod  :: Method
  ,  httpVersion    :: HttpVersion
  ,  pathInfo       :: B.ByteString
  ,  queryString    :: B.ByteString
  ,  serverName     :: B.ByteString
  ,  serverPort     :: Int
  ,  httpHeaders    :: [(B.ByteString, B.ByteString)]
  ,  urlScheme      :: UrlScheme
  ,  requestBody    :: IO (Maybe B.ByteString)
  ,  errorHandler   :: String -> IO ()
  ,  remoteHost     :: String
  }

data Response = Response
  { status        :: Int
  , statusMessage :: B.ByteString
  , headers       :: [(B.ByteString, B.ByteString)]
  , body          :: Either FilePath ((B.ByteString -> IO ()) -> IO ())
  }

type Application = Request -> IO Response

type Middleware = Application -> Application
