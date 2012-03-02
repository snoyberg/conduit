import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()

main :: IO ()
main = hspecX $ do
    describe "zlib" $ do
        prop "idempotent" $ \bss' -> runST $ do
            let bss = map S.pack bss'
                lbs = L.fromChunks bss
                src = mconcat $ map (CL.sourceList . return) bss
            outBss <- src C.$= CZ.gzip C.$= CZ.ungzip C.$$ CL.consume
            return $ lbs == L.fromChunks outBss
        prop "flush" $ \bss' -> runST $ do
            let bss = map S.pack $ filter (not . null) bss'
                bssC = concatMap (\bs -> [C.Chunk bs, C.Flush]) bss
                src = mconcat $ map (CL.sourceList . return) bssC
            outBssC <- src C.$= CZ.compressFlush 5 (CZ.WindowBits 31)
                           C.$= CZ.decompressFlush (CZ.WindowBits 31)
                           C.$$ CL.consume
            return $ bssC == outBssC
