module Network.Wai.Handler.Helper
    ( requestBodyHandle
    , requestBodyFunc
    ) where

import System.IO (Handle)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.Enumerator as E
import Data.Enumerator ((>>==))
import Control.Monad.IO.Class (liftIO)

requestBodyHandle :: Handle -> Int -> E.Enumerator B.ByteString IO a
requestBodyHandle h =
    requestBodyFunc go
  where
    go i = Just `fmap` B.hGet h (min i defaultChunkSize)

requestBodyFunc :: (Int -> IO (Maybe B.ByteString))
                -> Int
                -> E.Enumerator B.ByteString IO a
requestBodyFunc _ 0 step = E.returnI step
requestBodyFunc h len (E.Continue k) = do
    mbs <- liftIO $ h len
    case mbs of
        Nothing -> E.continue k
        Just bs -> do
            let newLen = len - B.length bs
            k (E.Chunks [bs]) >>== requestBodyFunc h newLen
requestBodyFunc _ _ step = E.returnI step
