{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.ZlibSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 ()
import Data.ByteString.Lazy.Char8 ()
import Control.Monad.Trans.Resource (runExceptionT_)
import Control.Monad.Trans.Class
import Control.Monad.Catch.Pure
import Control.Monad.Base
import Control.Monad (replicateM_)

instance MonadBase base m => MonadBase base (CatchT m) where
    liftBase = lift . liftBase

spec :: Spec
spec = describe "Data.Conduit.Zlib" $ do
        prop "idempotent" $ \bss' -> do
            let bss = map S.pack bss'
                lbs = L.fromChunks bss
                src = mconcat $ map (CL.sourceList . return) bss
            outBss <- src C.$= CZ.gzip C.$= CZ.ungzip C.$$ CL.consume
            L.fromChunks outBss `shouldBe` lbs
        prop "flush" $ \bss' -> do
            let bss = map S.pack $ filter (not . null) bss'
                bssC = concatMap (\bs -> [C.Chunk bs, C.Flush]) bss
                src = mconcat $ map (CL.sourceList . return) bssC
            outBssC <- src C.$= CZ.compressFlush 5 (CZ.WindowBits 31)
                           C.$= CZ.decompressFlush (CZ.WindowBits 31)
                           C.$$ CL.consume
            outBssC `shouldBe` bssC
        it "compressFlush large data" $ do
            let content = L.pack $ map (fromIntegral . fromEnum) $ concat $ ["BEGIN"] ++ map show [1..100000 :: Int] ++ ["END"]
                src = CL.sourceList $ map C.Chunk $ L.toChunks content
            bssC <- src C.$$ CZ.compressFlush 5 (CZ.WindowBits 31) C.=$ CL.consume
            let unChunk (C.Chunk x) = [x]
                unChunk C.Flush = []
            bss <- CL.sourceList bssC C.$$ CL.concatMap unChunk C.=$ CZ.ungzip C.=$ CL.consume
            L.fromChunks bss `shouldBe` content

        it "uncompressed after compressed" $ do
            let c = "This data is stored compressed."
                u = "This data isn't."
            let src1 = do
                    C.yield c C.$= CZ.gzip
                    C.yield u
            encoded <- src1 C.$$ CL.consume
            let src2 = mapM_ C.yield encoded
            (c', u') <- src2 C.$$ do
                c' <- CZ.ungzip C.=$ CL.consume
                u' <- CL.consume
                return (S.concat c', S.concat u')
            c' `shouldBe` c
            u' `shouldBe` u

        it "multiple compressed values" $ do
            let s1 = "hello"
                s2 = "world"
                src = do
                    C.yield s1 C.$= CZ.gzip
                    C.yield s2 C.$= CZ.gzip
            actual <- src C.$$ CZ.multiple CZ.ungzip C.=$ CL.consume
            S.concat actual `shouldBe` S.concat [s1, s2]

        it "single compressed, multiple uncompressed chunks" $ do
            let s1 = "hello"
                s2 = "there"
                s3 = "world"
            s1Z <- fmap S.concat $ C.yield s1 C.$= CZ.gzip C.$$ CL.consume
            let src = do
                    C.yield $ S.append s1Z s2
                    C.yield s3
            actual <- src C.$$ do
                x <- fmap S.concat $ CZ.ungzip C.=$ CL.consume
                y <- CL.consume
                return (x, y)
            actual `shouldBe` (s1, [s2, s3])

        it "multiple, over 32k" $ do
            let str = "One line"
                cnt = 30000
                src = replicateM_ cnt $ C.yield str C.$= CZ.gzip
            actual <- fmap S.concat $ src C.$$ CZ.multiple CZ.ungzip C.=$ CL.consume
            let expected = S.concat (replicate cnt str)
            S.length actual `shouldBe` S.length expected
            actual `shouldBe` expected
