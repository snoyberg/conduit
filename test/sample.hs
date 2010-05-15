import Data.ByteString.Char8 (unpack, pack)
import Data.ByteString.Lazy (fromChunks)
import Network.Wai
import Network.Wai.Enumerator
import Network.Wai.Middleware.Gzip
import Network.Wai.Handler.SimpleServer

app :: Application
app req =
    case unpack $ pathInfo req of
        "/" -> return $ Response Status200 [] $ Right $ fromLBS
                      $ fromChunks $ flip map [1..10000] $ \i -> pack $
                concat
                    [ "<p>Just this same paragraph again. "
                    , show i
                    , "</p>"
                    ]
        _ -> return $ Response Status404 [] $ Left "../LICENSE"

main :: IO ()
main = run 3000 $ gzip app
