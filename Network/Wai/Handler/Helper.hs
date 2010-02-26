module Network.Wai.Handler.Helper
    ( requestBodyHandle
    ) where

import System.IO (Handle)
import qualified Data.ByteString as B

requestBodyHandle :: Handle -> Int -> IO (Maybe (B.ByteString, Int))
requestBodyHandle _ 0 = return Nothing
requestBodyHandle h len = do
    let maxChunkSize = 1024
    bs <- B.hGet h $ min len maxChunkSize
    let newLen = len - B.length bs
    return $ Just (bs, newLen)
