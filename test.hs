{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Launch
import Network.Wai.Middleware.Gzip

main = run $ gzip True $ \_ -> return $ responseLBS status200 [("Content-Type", "text/html; charset=utf8")] "<html><head></head><body>HELLO THERE"
