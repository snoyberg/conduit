{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Lazy as CLazy
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Zlib as CZ
import Data.Conduit.Blaze (builderToByteString)
import Data.Conduit (runResourceT)
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.IORef as I
import Blaze.ByteString.Builder (fromByteString, toLazyByteString, insertLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.Maybe (catMaybes)

main :: IO ()
main = hspecX $ do
    describe "sum" $ do
        it "works for 1..10" $ do
            x <- runResourceT $ CL.fromList [1..10] C.<$$> CL.fold (+) (0 :: Int)
            x @?= sum [1..10]
        prop "is idempotent" $ \list ->
            (unsafePerformIO $ runResourceT $ CL.fromList list C.<$$> CL.fold (+) (0 :: Int))
            == sum list
    describe "Monoid instance for Source" $ do
        it "mappend" $ do
            x <- runResourceT $ (CL.fromList [1..5 :: Int] `mappend` CL.fromList [6..10]) C.<$$> CL.fold (+) 0
            x @?= sum [1..10]
        it "mconcat" $ do
            x <- runResourceT $ mconcat
                [ CL.fromList [1..5 :: Int]
                , CL.fromList [6..10]
                , CL.fromList [11..20]
                ] C.<$$> CL.fold (+) 0
            x @?= sum [1..20]
    describe "file access" $ do
        it "read" $ do
            bs <- S.readFile "conduit.cabal"
            bss <- runResourceT $ CB.sourceFile "conduit.cabal" C.<$$> CL.consume
            bs @=? S.concat bss
        it "write" $ do
            runResourceT $ CB.sourceFile "conduit.cabal" C.<$$> CB.sinkFile "tmp"
            bs1 <- S.readFile "conduit.cabal"
            bs2 <- S.readFile "tmp"
            bs1 @=? bs2
    describe "Monad instance for Sink" $ do
        it "binding" $ do
            x <- runResourceT $ CL.fromList [1..10] C.<$$> do
                _ <- CL.take 5
                CL.fold (+) (0 :: Int)
            x @?= sum [6..10]
    describe "resumable sources" $ do
        it "simple" $ do
            (x, y, z) <- runResourceT $ do
                bs <- C.bsourceM $ CL.fromList [1..10 :: Int]
                x <- bs C.$$ CL.take 5
                y <- bs C.$$ CL.fold (+) 0
                z <- bs C.$$ CL.consume
                return (x, y, z)
            x @?= [1..5] :: IO ()
            y @?= sum [6..10]
            z @?= []
    describe "conduits" $ do
        it "map, left" $ do
            x <- runResourceT $
                CL.fromList [1..10]
                    C.<$=> CL.map (* 2)
                    C.<$$> CL.fold (+) 0
            x @?= 2 * sum [1..10 :: Int]
        it "map, right" $ do
            x <- runResourceT $
                CL.fromList [1..10]
                    C.<$$> CL.map (* 2)
                    C.<=$> CL.fold (+) 0
            x @?= 2 * sum [1..10 :: Int]
        it "concatMap" $ do
            let input = [1, 11, 21]
            x <- runResourceT $ CL.fromList input
                    C.<$$> CL.concatMap (\i -> enumFromTo i (i + 9))
                    C.<=$> CL.fold (+) (0 :: Int)
            x @?= sum [1..30]
    describe "isolate" $ do
        it "bound to resumable source" $ do
            (x, y) <- runResourceT $ do
                bsrc <- C.bsourceM $ CL.fromList [1..10 :: Int]
                bcon <- C.bconduitM $ CL.isolate 5
                x <- bsrc C.$= bcon C.$$ CL.consume
                y <- bsrc C.$$ CL.consume
                return (x, y)
            x @?= [1..5]
            y @?= [6..10]
        it "bound to sink, non-resumable" $ do
            (x, y) <- runResourceT $ do
                CL.fromList [1..10 :: Int] C.<$$> do
                    x <- CL.isolate 5 C.<=$> CL.consume
                    y <- CL.consume
                    return (x, y)
            x @?= [1..5]
            y @?= [6..10]
        it "bound to sink, resumable" $ do
            (x, y) <- runResourceT $ do
                bsrc <- C.bsourceM $ CL.fromList [1..10 :: Int]
                x <- bsrc C.$$ CL.isolate 5 C.<=$> CL.consume
                y <- bsrc C.$$ CL.consume
                return (x, y)
            x @?= [1..5]
            y @?= [6..10]
    describe "lazy" $ do
        it "works inside a ResourceT" $ runResourceT $ do
            counter <- C.liftBase $ I.newIORef 0
            let incr i = C.sourceM
                    (C.liftBase $ I.newIORef $ C.Chunks [i :: Int])
                    (const $ return ())
                    (\istate -> do
                        state <- C.liftBase $ I.atomicModifyIORef istate
                            (\state -> (C.EOF [], state))
                        case state of
                            C.EOF [] -> return ()
                            _ -> do
                                count <- C.liftBase $ I.atomicModifyIORef counter
                                    (\j -> (j + 1, j + 1))
                                C.liftBase $ count @?= i
                        return state
                            )
            nums <- CLazy.lazyConsume $ mconcat $ map incr [1..10]
            C.liftBase $ nums @?= [1..10]
    describe "isolate" $ do
        it "works" $ do
            let sink = do
                    _ <- CL.take 2
                    CL.head
            let conduit = C.sequence sink
            res <- runResourceT $ CL.fromList [1..10 :: Int]
                           C.<$=> conduit
                           C.<$$> CL.consume
            catMaybes res @?= [3, 6, 9]
    describe "zlib" $ do
        prop "idempotent" $ \bss' -> unsafePerformIO $ runResourceT $ do
            let bss = map S.pack bss'
                lbs = L.fromChunks bss
                src = mconcat $ map (CL.fromList . return) bss
            outBss <- src C.<$=> CZ.gzip C.<$=> CZ.ungzip C.<$$> CL.consume
            return $ lbs == L.fromChunks outBss
    describe "binary isolate" $ do
        it "works" $ do
            bss <- runResourceT $ CL.fromList (replicate 1000 "X")
                           C.<$=> CB.isolate 6
                           C.<$$> CL.consume
            S.concat bss @?= "XXXXXX"
    describe "blaze" $ do
        prop "idempotent to toLazyByteString" $ \bss' -> unsafePerformIO $ runResourceT $ do
            let bss = map S.pack bss'
            let builders = map fromByteString bss
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.fromList . return) builders
            outBss <- src C.<$=> builderToByteString C.<$$> CL.consume
            return $ lbs == L.fromChunks outBss
        it "works for large input" $ runResourceT $ do
            let builders = replicate 10000 (fromByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.fromList . return) builders
            outBss <- src C.<$=> builderToByteString C.<$$> CL.consume :: C.ResourceT IO [S.ByteString]
            C.liftBase $ lbs @=? L.fromChunks outBss
        it "works for lazy bytestring insertion" $ runResourceT $ do
            let builders = replicate 10000 (insertLazyByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.fromList . return) builders
            outBss <- src C.<$=> builderToByteString C.<$$> CL.consume :: C.ResourceT IO [S.ByteString]
            C.liftBase $ lbs @=? L.fromChunks outBss
