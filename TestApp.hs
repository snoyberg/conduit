{-# LANGUAGE OverloadedStrings #-}
module TestApp (testApp) where

import Network.Wai
import Data.ByteString.Lazy.Char8 (pack)
import Control.Concurrent.MVar

testApp handler = do
    putStrLn "testApp called, this should happen only once per reload"
    var <- newMVar 0
    handler $ const $ do
        counter <- modifyMVar var $ \i -> return (i + 1, i + 1)
        return $ Response status200
            [("Content-Type", "text/plain")]
            $ ResponseLBS $ pack $ "Counter: " ++ show counter
