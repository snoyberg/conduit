{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromShow)
import Data.Monoid (mappend)
import Network.Wai.Handler.Warp (run)
import Data.Enumerator (enumList, ($$), run_)

bigtable :: [Builder]
bigtable =
    fromByteString "<table>"
    : foldr (:) [fromByteString "</table>"] (replicate 2 row)
  where
    row = fromByteString "<tr>"
          `mappend` foldr go (fromByteString "</tr>") [1..2]
    go i rest = fromByteString "<td>"
                `mappend` fromShow i
                `mappend` fromByteString "</td>"
                `mappend` rest

main = run 3000 app

app _ = return $ ResponseEnumerator $ \f ->
    run_ $ enumList 4 bigtable $$ f status200 [("Content-Type", "text/html")]
