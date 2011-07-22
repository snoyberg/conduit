import Data.ByteString.Char8 (unpack, pack)
import Data.ByteString.Lazy (fromChunks)
import Network.Wai
import Network.Wai.Enumerator
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.CleanPath
import Network.Wai.Handler.SimpleServer

app :: [String] -> Application
app [] _ = return $ Response Status200 [] $ Right $ fromLBS
                  $ fromChunks $ flip map [1..10000] $ \i -> pack $
                concat
                    [ "<p>Just this same paragraph again. "
                    , show i
                    , "</p>"
                    ]
app ["test.html"] _ = return $ Response Status200 [] $ Left "test.html"
app ["json"] _ =return $ Response Status200
                         [(ContentType, pack "application/json")]
                       $ Left "json"
app _ _ = return $ Response Status404 [] $ Left "../LICENSE"

main :: IO ()
main = run 3000 $ jsonp $ gzip $ cleanPath app
