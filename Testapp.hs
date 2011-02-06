{-# LANGUAGE OverloadedStrings #-}
module Testapp where

import Network.Wai
import Data.ByteString.Lazy.Char8 ()

testapp' _ = return $ responseLBS status200 [("Content-Type", "text/html")] "test"

testapp f = f testapp'
