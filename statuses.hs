{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as S

main = run 3000 app

app req =
    return $ responseLBS (Status s' s) [("Content-Type", "text/plain")]
           $ pack $ concat
        [ "The status code is "
        , S.unpack s
        , ". Have a nice day!"
        ]
  where
    s = S.dropWhile (== '/') $ pathInfo req
    s' = read $ S.unpack s
