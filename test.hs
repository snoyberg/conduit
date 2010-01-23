import Network.Wai
import Network.Wai.Handler.SimpleServer
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B

main :: IO ()
main = putStrLn "http://localhost:3000/" >> run 3000 app

app :: Application
app req = case B8.unpack $ pathInfo req of
    "/post/" -> postResponse $ requestBody req
    _ -> indexResponse

indexResponse :: IO Response
indexResponse = return Response
    { status = 200
    , statusMessage = B8.pack "OK"
    , headers = [(B8.pack "Content-Type", B8.pack "text/html")]
    , body = index
    }

postResponse :: IO (Maybe B.ByteString) -> IO Response
postResponse rb = return Response
    { status = 200
    , statusMessage = B8.pack "OK"
    , headers = [(B8.pack "Content-Type", B8.pack "text/plain")]
    , body = postBody rb
    }

index :: ResponseBody -> IO ()
index rb = sendFile rb "index.html"

postBody :: IO (Maybe B.ByteString) -> ResponseBody -> IO ()
postBody req res = do
    mbs <- req
    case mbs of
        Nothing -> return ()
        Just bs -> sendByteString res bs >> postBody req res
