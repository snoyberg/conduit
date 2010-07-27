module Network.Wai.Handler.Helper
    ( requestBodyHandle
    , requestBodyFunc
    ) where

import System.IO (Handle)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Network.Wai (Source (..))

requestBodyHandle :: Handle -> Int -> Source
requestBodyHandle h =
    requestBodyFunc go
  where
    go = Just `fmap` B.hGet h defaultChunkSize

requestBodyFunc :: IO (Maybe B.ByteString) -> Int -> Source
requestBodyFunc _ 0 = Source $ return Nothing
requestBodyFunc h len = Source $ do
    mbs <- h
    case mbs of
        Nothing -> return Nothing
        Just bs -> do
            let newLen = len - B.length bs
            return $ Just (bs, requestBodyFunc h $ max 0 newLen)
