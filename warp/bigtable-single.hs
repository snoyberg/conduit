{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromShow)
import Data.Monoid (mappend)
import Network.Wai.Handler.Warp (run)

bigtable :: Builder
bigtable =
    fromByteString "<table>"
    `mappend` foldr mappend (fromByteString "</table>") (replicate 2 row)
  where
    row = fromByteString "<tr>"
          `mappend` foldr go (fromByteString "</tr>") [1..2]
    go i rest = fromByteString "<td>"
                `mappend` fromShow i
                `mappend` fromByteString "</td>"
                `mappend` rest

main = run 3000 app

app _ = return $ ResponseBuilder status200 [("Content-Type", "text/html")] bigtable
