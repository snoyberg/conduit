{-# LANGUAGE OverloadedStrings #-}
import qualified Network.HTTP.Enumerator as H
import qualified Network.Wai as W
import Network.Wai.Handler.SimpleServer (run)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 ()
import Data.Enumerator (($$), joinI, run_)
import Blaze.ByteString.Builder (fromByteString)
import qualified Data.Enumerator as E

main :: IO ()
main = run 3000 app

app :: W.Application
app W.Request { W.pathInfo = path } =
    case H.parseUrl $ "http://wiki.yesodweb.com" ++ S8.unpack path of
        Nothing -> return notFound
        Just hreq -> return $ W.ResponseEnumerator $ run_ . H.http hreq . go
  where
    go f s h = joinI $ E.map fromByteString $$ f s $ filter safe h
    safe (x, _) = not $ x `elem` ["Content-Encoding", "Transfer-Encoding"]

notFound :: W.Response
notFound = W.ResponseLBS W.status404 [("Content-Type", "text/plain")] "Not found"
