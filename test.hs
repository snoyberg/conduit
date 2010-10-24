{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Webkit
import qualified Data.ByteString.Char8 as B8
import qualified Network.Wai.Source as Source

main :: IO ()
main = putStrLn "http://localhost:3000/" >> run "3000" app

app :: Application
app req = case B8.unpack $ pathInfo req of
    "/post/" -> postResponse $ Source.toEnumerator $ requestBody req
    _ -> indexResponse

indexResponse :: IO Response
indexResponse = return Response
    { status = status200
    , responseHeaders = [("Content-Type" , B8.pack "text/html")]
    , responseBody = index
    }

postResponse :: Enumerator -> IO Response
postResponse rb = return Response
    { status = status200
    , responseHeaders = [("Content-Type", B8.pack "text/plain")]
    , responseBody = ResponseEnumerator rb
    }

index = ResponseFile "index.html"
