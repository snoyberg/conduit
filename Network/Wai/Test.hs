{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Test
    ( -- * Session
      Session
    , runSession
      -- * Requests
    , request
    , srequest
    , SRequest (..)
    , SResponse (..)
      -- * Assertions
    , assertStatus
    , assertContentType
    , assertBody
    , assertHeader
    , assertNoHeader
    ) where
import Network.Wai
import qualified Test.HUnit.Base as H
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Enumerator (joinI, consume, ($$), run_, enumList)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

type Session = ReaderT Application (StateT ClientState IO)

data ClientState = ClientState
    { clientCookies :: Map ByteString ByteString
    }

initState :: ClientState
initState = ClientState Map.empty

runSession :: Session a -> Application -> IO a
runSession session app = evalStateT (runReaderT session app) initState

data SRequest = SRequest
    { simpleRequest :: Request
    , simpleRequestBody :: L.ByteString
    }
data SResponse = SResponse
    { simpleStatus :: Status
    , simpleHeaders :: [(CIByteString, ByteString)]
    , simpleBody :: L.ByteString
    }
    deriving (Show, Eq)
request :: Request -> Session SResponse
request = srequest . flip SRequest L.empty

srequest :: SRequest -> Session SResponse
srequest (SRequest req bod) = do
    app <- ask
    res <- liftIO $ run_ $ enumList 4 (L.toChunks bod) $$ app req
    sres <- liftIO $ runResponse res
    -- FIXME cookie processing
    return sres

runResponse :: Response -> IO SResponse
runResponse res =
    responseEnumerator res go
  where
    go s h = do
        bss <- joinI $ builderToByteString $$ consume
        return $ SResponse s h $ L.fromChunks bss

assertBool :: String -> Bool -> Session ()
assertBool s b = liftIO $ H.assertBool s b

assertString :: String -> Session ()
assertString s = liftIO $ H.assertString s

assertContentType :: ByteString -> SResponse -> Session ()
assertContentType ct SResponse{simpleHeaders = h} =
    case lookup "content-type" h of
        Nothing -> assertString $ concat
            [ "Expected content type "
            , show ct
            , ", but no content type provided"
            ]
        Just ct' -> assertBool (concat
            [ "Expected content type "
            , show ct
            , ", but received "
            , show ct'
            ]) (go ct == go ct')
  where
    go = S8.takeWhile (/= ';')

assertStatus :: Int -> SResponse -> Session ()
assertStatus i SResponse{simpleStatus = s} = assertBool (concat
    [ "Expected status code "
    , show i
    , ", but received "
    , show sc
    ]) $ i == sc
  where
    sc = statusCode s

assertBody :: L.ByteString -> SResponse -> Session ()
assertBody lbs SResponse{simpleBody = lbs'} = assertBool (concat
    [ "Expected response body "
    , show $ L8.unpack lbs
    , ", but received "
    , show $ L8.unpack lbs'
    ]) $ lbs == lbs'

assertHeader :: CIByteString -> S.ByteString -> SResponse -> Session ()
assertHeader header value SResponse{simpleHeaders = h} =
    case lookup header h of
        Nothing -> assertString $ concat
            [ "Expected header "
            , show header
            , " to be "
            , show value
            , ", but it was not present"
            ]
        Just value' -> assertBool (concat
            [ "Expected header "
            , show header
            , " to be "
            , show value
            , ", but received "
            , show value'
            ]) (value == value')

assertNoHeader :: CIByteString -> SResponse -> Session ()
assertNoHeader header SResponse{simpleHeaders = h} =
    case lookup header h of
        Nothing -> return ()
        Just s -> assertString $ concat
            [ "Unexpected header "
            , show header
            , " containing "
            , show s
            ]
