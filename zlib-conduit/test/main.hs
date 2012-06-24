import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Control.Monad.Trans.Resource (runExceptionT_)

main :: IO ()
main = hspec $ do
    describe "zlib" $ do
        prop "idempotent" $ \bss' -> runST $ do
            let bss = map S.pack bss'
                lbs = L.fromChunks bss
                src = mconcat $ map (CL.sourceList . return) bss
            outBss <- runExceptionT_ $ src C.$= CZ.gzip C.$= CZ.ungzip C.$$ CL.consume
            return $ lbs == L.fromChunks outBss
        prop "flush" $ \bss' -> runST $ do
            let bss = map S.pack $ filter (not . null) bss'
                bssC = concatMap (\bs -> [C.Chunk bs, C.Flush]) bss
                src = mconcat $ map (CL.sourceList . return) bssC
            outBssC <- runExceptionT_
                     $ src C.$= CZ.compressFlush 5 (CZ.WindowBits 31)
                           C.$= CZ.decompressFlush (CZ.WindowBits 31)
                           C.$$ CL.consume
            return $ bssC == outBssC
        it "compressFlush large data" $ do
            let content = L.pack $ map (fromIntegral . fromEnum) $ concat $ ["BEGIN"] ++ map show [1..100000 :: Int] ++ ["END"]
                src = CL.sourceList $ map C.Chunk $ L.toChunks content
            bssC <- src C.$$ CZ.compressFlush 5 (CZ.WindowBits 31) C.=$ CL.consume
            let unChunk (C.Chunk x) = [x]
                unChunk C.Flush = []
            bss <- CL.sourceList bssC C.$$ CL.concatMap unChunk C.=$ CZ.ungzip C.=$ CL.consume
            L.fromChunks bss @?= content
