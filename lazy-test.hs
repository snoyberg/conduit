{-# LANGUAGE Rank2Types #-}
import Network.Wai
import Network.Wai.Enumerator
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
    , headers = [(ContentType, B8.pack "text/html")]
    , body = index
    }

postResponse :: (forall a. Enumerator a) -> IO Response
postResponse rb = return Response
    { status = Status200
    , headers = [(ContentType, B8.pack "text/plain")]
    , body = Right $ fromLBS' $ toLBS rb
    }

index :: Either FilePath a
index = Left "index.html"
