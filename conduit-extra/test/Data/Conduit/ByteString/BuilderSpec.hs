{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.ByteString.BuilderSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.ByteString.Char8 ()
import Data.Conduit.ByteString.Builder (builderToByteString, builderToByteStringFlush)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Builder.Internal (lazyByteStringInsert, flush)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()

spec :: Spec
spec =
    describe "Data.Conduit.ByteString.Builder" $ do
        prop "idempotent to toLazyByteString" $ \bss' -> runST $ do
            let bss = map S.pack bss'
            let builders = map byteString bss
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume
            return $ lbs == L.fromChunks outBss

        it "works for large input" $ do
            let builders = replicate 10000 (byteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume
            lbs `shouldBe` L.fromChunks outBss

        it "works for lazy bytestring insertion" $ do
            let builders = replicate 10000 (lazyByteStringInsert "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume
            lbs `shouldBe` L.fromChunks outBss

        it "flush shouldn't bring in empty strings." $ do
            let dat = ["hello", "world"]
                src = CL.sourceList dat C.$= CL.map ((`mappend` flush) . byteString)
            out <- src C.$= builderToByteString C.$$ CL.consume
            dat `shouldBe` out

        prop "flushing" $ \bss' -> runST $ do
            let bss = concatMap (\bs -> [C.Chunk $ S.pack bs, C.Flush]) $ filter (not . null) bss'
            let chunks = map (fmap byteString) bss
            let src = CL.sourceList chunks
            outBss <- src C.$= builderToByteStringFlush C.$$ CL.consume
            if bss == outBss then return () else error (show (bss, outBss))
            return $ bss == outBss
        it "large flush input" $ do
            let lbs = L.pack $ concat $ replicate 100000 [0..255]
            let chunks = map (C.Chunk . byteString) (L.toChunks lbs)
            let src = CL.sourceList chunks
            bss <- src C.$$ builderToByteStringFlush C.=$ CL.consume
            let unFlush (C.Chunk x) = [x]
                unFlush C.Flush = []
            L.fromChunks (concatMap unFlush bss) `shouldBe` lbs
