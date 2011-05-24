{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Launch

main = run $ \_ -> return $ responseLBS status200 [("Content-Type", "text/html; charset=utf8")] "HELLO THERE"
