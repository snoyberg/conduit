{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.SCGI (run)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Enumerator.List as E
import Network.HTTP.Types (status200)

main = run app

app req
    | rawPathInfo req == "/" = return $ responseLBS status200 [("Content-Type", "text/html")] "<form method='post' action='/post'><input type='text' name='name'><input type='submit'></form>"
app req = do
    bss <- E.consume
    return $ responseLBS status200 [("Content-Type", "text/plain")] $ L.concat
        [ L8.pack $ show $ requestHeaders req
        , "\n"
        , L.fromChunks bss
        ]
