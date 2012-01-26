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
import Data.Conduit (runResourceT)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import Blaze.ByteString.Builder (fromByteString, toLazyByteString, insertLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = hspecX $ do
    describe "blaze" $ do
        prop "idempotent to toLazyByteString" $ \bss' -> runST $ runResourceT $ do
            let bss = map S.pack bss'
            let builders = map fromByteString bss
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume
            return $ lbs == L.fromChunks outBss

        it "works for large input" $ runResourceT $ do
            let builders = replicate 10000 (fromByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume :: C.ResourceT IO [S.ByteString]
            liftIO $ lbs @=? L.fromChunks outBss

        it "works for lazy bytestring insertion" $ runResourceT $ do
            let builders = replicate 10000 (insertLazyByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume :: C.ResourceT IO [S.ByteString]
            liftIO $ lbs @=? L.fromChunks outBss

        prop "flushing" $ \bss' -> runST $ runResourceT $ do
            let bss = concatMap (\bs -> [C.Chunk $ S.pack bs, C.Flush]) $ filter (not . null) bss'
            let src = CL.sourceList $ map (fmap fromByteString) bss
            outBss <- src C.$= builderToByteStringFlush C.$$ CL.consume
            if bss == outBss then return () else error (show (bss, outBss))
            return $ bss == outBss
