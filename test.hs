import Network.Wai
import Network.Wai.Handler.SimpleServer
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = putStrLn "http://localhost:3000/" >> run 3000 app

app :: Application
app _ = return $ Response
            { status = 200
            , statusMessage = B8.pack "OK"
            , headers = [(B8.pack "Content-Type", B8.pack "text/html")]
            , body = index
            }

index :: ResponseBody -> IO ()
index rb = sendFile rb "index.html"
