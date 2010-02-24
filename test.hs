{-# LANGUAGE Rank2Types #-}
import Network.Wai
import Network.Wai.Handler.SimpleServer
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = putStrLn "http://localhost:3000/" >> run 3000 app

app :: Application
app req = case B8.unpack $ pathInfo req of
    "/post/" -> postResponse $ requestBody req
    _ -> indexResponse

indexResponse :: IO Response
indexResponse = return Response
    { status = Status200
    , responseHeaders = [(ContentType, B8.pack "text/html")]
    , responseBody = index
    }

postResponse :: Enumerator -> IO Response
postResponse rb = return Response
    { status = Status200
    , responseHeaders = [(ContentType, B8.pack "text/plain")]
    , responseBody = Right rb
    }

index :: Either FilePath a
index = Left "index.html"
