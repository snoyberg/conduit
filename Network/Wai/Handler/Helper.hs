module Network.Wai.Handler.Helper
    ( requestBodyHandle
    ) where

import System.IO (Handle)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Network.Wai (Source (..))

requestBodyHandle :: Handle -> Int -> Source
requestBodyHandle _ 0 = Source $ return Nothing
requestBodyHandle h len = Source $ do
    bs <- B.hGet h $ min len defaultChunkSize
    let newLen = len - B.length bs
    return $ Just (bs, requestBodyHandle h newLen)
