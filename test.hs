{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.FastCGI
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Enumerator as E

main = run app

app req
    | pathInfo req == "/" = return $ responseLBS status200 [("Content-Type", "text/html")] "<form method='post' action='/post'><input type='text' name='name'><input type='submit'></form>"
app req = do
    bss <- E.consume
    return $ responseLBS status200 [("Content-Type", "text/plain")] $ L.concat
        [ L8.pack $ show $ requestHeaders req
        , "\n"
        , L.fromChunks bss
        ]
