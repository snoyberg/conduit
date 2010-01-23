{-# LANGUAGE ExistentialQuantification #-}
module Network.Wai
    ( -- * Data types
      -- ** Request method
      RequestMethod (..)
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
    , ResponseBody (..)
    , ResponseBodyClass (..)
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import System.IO (withBinaryFile, IOMode (ReadMode), hIsEOF)
import Control.Monad (unless)

-- | Please do not use the Show and Read instances for anything other than
-- debugging purposes. Instead, the 'methodFromBS' and 'methodToBS' provide a
-- more appropriate interface.
data RequestMethod =
    OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | OtherMethod B.ByteString
  deriving (Show, Read, Eq)

methodFromBS :: B.ByteString -> RequestMethod
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
        _ -> OtherMethod bs
    | otherwise = OtherMethod bs

methodToBS :: RequestMethod -> B.ByteString
methodToBS OPTIONS = B8.pack "OPTIONS"
methodToBS GET = B8.pack "GET"
methodToBS HEAD = B8.pack "HEAD"
methodToBS POST = B8.pack "POST"
methodToBS PUT = B8.pack "PUT"
methodToBS DELETE = B8.pack "DELETE"
methodToBS TRACE = B8.pack "TRACE"
methodToBS CONNECT = B8.pack "CONNECT"
methodToBS (OtherMethod bs) = bs

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
  {  requestMethod  :: RequestMethod
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
  , body          :: ResponseBody -> IO ()
  }

type Application = Request -> IO Response

type Middleware = Application -> Application

data ResponseBody = forall a. ResponseBodyClass a => ResponseBody a
class ResponseBodyClass a where
    sendByteString :: a -> B.ByteString -> IO ()

    sendLazyByteString :: a -> L.ByteString -> IO ()
    sendLazyByteString a bs = mapM_ (sendByteString a) $ L.toChunks bs

    sendFile :: a -> FilePath -> IO ()
    sendFile a fp = withBinaryFile fp ReadMode helper where
        helper h = do
            eof <- hIsEOF h
            unless eof $ do
                b <- B.hGet h 1024 -- FIXME determine better block size
                sendByteString a b
                helper h
instance ResponseBodyClass ResponseBody where
    sendByteString (ResponseBody a) = sendByteString a
    sendLazyByteString (ResponseBody a) = sendLazyByteString a
    sendFile (ResponseBody a) = sendFile a
