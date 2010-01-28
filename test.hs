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
    { status = Status200
    , headers = [(ContentType, B8.pack "text/html")]
    , body = index
    }

postResponse :: IO (Maybe B.ByteString) -> IO Response
postResponse rb = return Response
    { status = Status200
    , headers = [(ContentType, B8.pack "text/plain")]
    , body = Right $ postBody rb
    }

index :: Either FilePath a
index = Left "index.html"

postBody :: IO (Maybe B.ByteString) -> Enumerator a
postBody req f a = do
    mbs <- req
    case mbs of
        Nothing -> return a
        Just bs -> do
            res <- f a bs
            case res of
                Left a' -> return a'
                Right a' -> postBody req f a'
