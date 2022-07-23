{-# LANGUAGE RankNTypes #-}
import Data.Conduit
import qualified Data.Conduit.Text as CT
import qualified OldText as OT
import Criterion.Main
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.List as CL
import Data.Text.StreamDecoding
import Data.Text.Encoding (decodeUtf8)

lengthT :: Monad m => ConduitT T.Text o m Int
lengthT = CL.fold (\x y -> x + T.length y) 0

main :: IO ()
main = do
    bs <- S.readFile "utf8-bench.hs"
    let bss = replicate 1000 bs
        src = mapM_ yield bss
        lbs = L.fromChunks bss
    defaultMain
        [ bench "old conduit" $ whnf (\src' -> runException_ $ src' $$ OT.decode OT.utf8 =$ lengthT) src
        , bench "lazy text" $ whnf (TL.length . TLE.decodeUtf8) lbs
        , bench "new conduit" $ whnf (\src' -> runException_ $ src' $$ CT.decode CT.utf8 =$ lengthT) src
        , bench "stream" $ whnf calcLen bss
        -- , bench "stream fake" $ whnf (calcLen2 bss) 0
        ]

calcLen [] = 0
calcLen (bs0:bss0) =
    loop (streamUtf8 bs0) bss0 0
  where
    loop (DecodeResultSuccess t next) bss total =
        let total' = total + T.length t
         in case bss of
                [] -> total'
                bs:bss' -> total' `seq` loop (next bs) bss' total'
{-

calcLen [] = id
calcLen (bs0:bss0) =
    loop (streamUtf8 bs0) bss0
  where
    loop t bss total =
        let total' = total + T.length t
         in case bss of
                [] -> total'
                bs:bss' -> total' `seq` loop (streamUtf8 bs) bss' total'

calcLen2 [] = id
calcLen2 (bs0:bss0) =
    loop (decodeUtf8 bs0) bss0
  where
    loop t bss total =
        let total' = total + T.length t
         in case bss of
                [] -> total'
                bs:bss' -> total' `seq` loop (decodeUtf8 bs) bss' total'
-}
