{-# LANGUAGE OverloadedStrings #-}
import qualified Network.HTTP.Enumerator as H
import qualified Network.Wai as W
import Network.Wai.Handler.SimpleServer (run)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as S8
import qualified Text.XML.Enumerator.Parse as X
import qualified Text.XML.Enumerator.Render as X
import Data.Enumerator (($$), joinI, run_)

main :: IO ()
main = run 3000 app

app :: W.Application
app wreq = do
    return $ W.Response
        { W.status = W.status200
        , W.responseHeaders =
            [ ("Content-Type", "text/xml")
            ]
        , W.responseBody = W.ResponseEnumerator
            $ \step -> run_ $ H.apply2 (H.http hreq) $ const $ const
            $ joinI $ X.detectUtf
           $$ joinI $ X.parseBytes
           $$ joinI $ X.renderBuilder $$ step
        }
  where
    hreq = fromJust $ H.parseUrl $ S8.unpack $ S8.tail $ W.pathInfo wreq
