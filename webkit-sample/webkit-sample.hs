{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Webkit
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (consume, Iteratee)
import Blaze.ByteString.Builder (fromLazyByteString)

main :: IO ()
main = putStrLn "http://localhost:3000/" >> run "Webkit Sample" app

app :: Application
app req = case pathInfo req of
    "/post/" -> do
        bss <- consume
        postResponse $ L.fromChunks bss
    _ -> indexResponse

indexResponse :: Iteratee ByteString IO Response
indexResponse = return $ ResponseFile
    status200
    [("Content-Type" , "text/html")]
    "index.html"

postResponse :: L.ByteString -> Iteratee ByteString IO Response
postResponse lbs = return $ ResponseBuilder
    status200
    [("Content-Type", "text/plain")]
    (fromLazyByteString lbs)
