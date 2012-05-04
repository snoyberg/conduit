{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.ByteString.Char8 ()
import Data.Conduit.Blaze (builderToByteString, builderToByteStringFlush)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import Blaze.ByteString.Builder (fromByteString, toLazyByteString, insertLazyByteString, flush)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()

main :: IO ()
main = hspecX $ do
    describe "blaze" $ do
        prop "idempotent to toLazyByteString" $ \bss' -> runST $ do
            let bss = map S.pack bss'
            let builders = map fromByteString bss
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume
            return $ lbs == L.fromChunks outBss

        it "works for large input" $ do
            let builders = replicate 10000 (fromByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume
            lbs @=? L.fromChunks outBss

        it "works for lazy bytestring insertion" $ do
            let builders = replicate 10000 (insertLazyByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume
            lbs @=? L.fromChunks outBss

        it "flush shouldn't bring in empty strings." $ do
            let dat = ["hello", "world"]
                src = CL.sourceList dat C.$= CL.map ((`mappend` flush) . fromByteString)
            out <- src C.$= builderToByteString C.$$ CL.consume
            dat @=? out

        prop "flushing" $ \bss' -> runST $ do
            let bss = concatMap (\bs -> [C.Chunk $ S.pack bs, C.Flush]) $ filter (not . null) bss'
            let src = CL.sourceList $ map (fmap fromByteString) bss
            outBss <- src C.$= builderToByteStringFlush C.$$ CL.consume
            if bss == outBss then return () else error (show (bss, outBss))
            return $ bss == outBss
