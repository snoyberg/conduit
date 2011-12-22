{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Lazy as CLazy
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Zlib as CZ
import Data.Conduit.Blaze (builderToByteString)
import Data.Conduit (runResourceT)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.IORef as I
import Blaze.ByteString.Builder (fromByteString, toLazyByteString, insertLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Writer (Writer)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Monad.Trans.Resource (runExceptionT_, withIO, resourceForkIO)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = hspecX $ do
    describe "filter" $ do
        it "even" $ do
            x <- runResourceT $ CL.sourceList [1..10] C.$$ CL.filter even C.=$ CL.consume
            x @?= filter even [1..10 :: Int]
    describe "ResourceT" $ do
        it "resourceForkIO" $ do
            counter <- I.newIORef 0
            let w = withIO
                        (I.atomicModifyIORef counter $ \i ->
                            (i + 1, ()))
                        (const $ I.atomicModifyIORef counter $ \i ->
                            (i - 1, ()))
            runResourceT $ do
                _ <- w
                _ <- resourceForkIO $ return ()
                _ <- resourceForkIO $ return ()
                sequence_ $ replicate 1000 $ do
                    tid <- resourceForkIO $ return ()
                    liftIO $ killThread tid
                _ <- resourceForkIO $ return ()
                _ <- resourceForkIO $ return ()
                return ()

            -- give enough of a chance to the cleanup code to finish
            threadDelay 1000
            res <- I.readIORef counter
            res @?= (0 :: Int)
    describe "sum" $ do
        it "works for 1..10" $ do
            x <- runResourceT $ CL.sourceList [1..10] C.$$ CL.fold (+) (0 :: Int)
            x @?= sum [1..10]
        prop "is idempotent" $ \list ->
            (runST $ runResourceT $ CL.sourceList list C.$$ CL.fold (+) (0 :: Int))
            == sum list
    describe "Monoid instance for Source" $ do
        it "mappend" $ do
            x <- runResourceT $ (CL.sourceList [1..5 :: Int] `mappend` CL.sourceList [6..10]) C.$$ CL.fold (+) 0
            x @?= sum [1..10]
        it "mconcat" $ do
            x <- runResourceT $ mconcat
                [ CL.sourceList [1..5 :: Int]
                , CL.sourceList [6..10]
                , CL.sourceList [11..20]
                ] C.$$ CL.fold (+) 0
            x @?= sum [1..20]
    describe "file access" $ do
        it "read" $ do
            bs <- S.readFile "conduit.cabal"
            bss <- runResourceT $ CB.sourceFile "conduit.cabal" C.$$ CL.consume
            bs @=? S.concat bss
        it "read range" $ do
            S.writeFile "tmp" "0123456789"
            bss <- runResourceT $ CB.sourceFileRange "tmp" (Just 2) (Just 3) C.$$ CL.consume
            S.concat bss @?= "234"
        it "write" $ do
            runResourceT $ CB.sourceFile "conduit.cabal" C.$$ CB.sinkFile "tmp"
            bs1 <- S.readFile "conduit.cabal"
            bs2 <- S.readFile "tmp"
            bs1 @=? bs2
        it "conduit" $ do
            runResourceT $ CB.sourceFile "conduit.cabal"
                C.$= CB.conduitFile "tmp"
                C.$$ CB.sinkFile "tmp2"
            bs1 <- S.readFile "conduit.cabal"
            bs2 <- S.readFile "tmp"
            bs3 <- S.readFile "tmp2"
            bs1 @=? bs2
            bs1 @=? bs3
    describe "Monad instance for Sink" $ do
        it "binding" $ do
            x <- runResourceT $ CL.sourceList [1..10] C.$$ do
                _ <- CL.take 5
                CL.fold (+) (0 :: Int)
            x @?= sum [6..10]
    describe "resumable sources" $ do
        it "simple" $ do
            (x, y, z) <- runResourceT $ do
                bs <- C.bufferSource $ CL.sourceList [1..10 :: Int]
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
                CL.sourceList [1..10]
                    C.$= CL.map (* 2)
                    C.$$ CL.fold (+) 0
            x @?= 2 * sum [1..10 :: Int]
        it "map, right" $ do
            x <- runResourceT $
                CL.sourceList [1..10]
                    C.$$ CL.map (* 2)
                    C.=$ CL.fold (+) 0
            x @?= 2 * sum [1..10 :: Int]
        it "concatMap" $ do
            let input = [1, 11, 21]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.concatMap (\i -> enumFromTo i (i + 9))
                    C.=$ CL.fold (+) (0 :: Int)
            x @?= sum [1..30]
        it "bind together" $ do
            let conduit = CL.map (+ 5) C.=$= CL.map (* 2)
            x <- runResourceT $ CL.sourceList [1..10] C.$= conduit C.$$ CL.fold (+) 0
            x @?= sum (map (* 2) $ map (+ 5) [1..10 :: Int])
#if !FAST
    describe "isolate" $ do
        it "bound to resumable source" $ do
            (x, y) <- runResourceT $ do
                bsrc <- C.bufferSource $ CL.sourceList [1..10 :: Int]
                x <- bsrc C.$= CL.isolate 5 C.$$ CL.consume
                y <- bsrc C.$$ CL.consume
                return (x, y)
            x @?= [1..5]
            y @?= [6..10]
        it "bound to sink, non-resumable" $ do
            (x, y) <- runResourceT $ do
                CL.sourceList [1..10 :: Int] C.$$ do
                    x <- CL.isolate 5 C.=$ CL.consume
                    y <- CL.consume
                    return (x, y)
            x @?= [1..5]
            y @?= [6..10]
        it "bound to sink, resumable" $ do
            (x, y) <- runResourceT $ do
                bsrc <- C.bufferSource $ CL.sourceList [1..10 :: Int]
                x <- bsrc C.$$ CL.isolate 5 C.=$ CL.consume
                y <- bsrc C.$$ CL.consume
                return (x, y)
            x @?= [1..5]
            y @?= [6..10]
    describe "lazy" $ do
        it' "works inside a ResourceT" $ runResourceT $ do
            counter <- C.liftBase $ I.newIORef 0
            let incr i = C.sourceIO
                    (C.liftBase $ I.newIORef $ C.SourceResult C.Open [i :: Int])
                    (const $ return ())
                    (\istate -> do
                        state@(C.SourceResult sstate _) <- C.liftBase $ I.atomicModifyIORef istate
                            (\state -> (C.SourceResult C.Closed [], state))
                        case sstate of
                            C.Closed -> return ()
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
            res <- runResourceT $ CL.sourceList [1..10 :: Int]
                           C.$= conduit
                           C.$$ CL.consume
            catMaybes res @?= [3, 6, 9]
#endif
    describe "peek" $ do
        it "works" $ do
            (a, b) <- runResourceT $ CL.sourceList [1..10 :: Int] C.$$ do
                a <- CL.peek
                b <- CL.consume
                return (a, b)
            (a, b) @?= (Just 1, [1..10])
    describe "zlib" $ do
        prop "idempotent" $ \bss' -> runST $ runResourceT $ do
            let bss = map S.pack bss'
                lbs = L.fromChunks bss
                src = mconcat $ map (CL.sourceList . return) bss
            outBss <- src C.$= CZ.gzip C.$= CZ.ungzip C.$$ CL.consume
            return $ lbs == L.fromChunks outBss
    describe "text" $ do
        let go enc tenc cenc = do
                prop (enc ++ " single chunk") $ \chars -> runST $ runExceptionT_ $ runResourceT $ do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = CL.sourceList $ L.toChunks lbs
                    ts <- src C.$= CT.decode cenc C.$$ CL.consume
                    return $ TL.fromChunks ts == tl
                prop (enc ++ " many chunks") $ \chars -> runST $ runExceptionT_ $ runResourceT $ do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = mconcat $ map (CL.sourceList . return . S.singleton) $ L.unpack lbs
                    ts <- src C.$= CT.decode cenc C.$$ CL.consume
                    return $ TL.fromChunks ts == tl
                prop (enc ++ " encoding") $ \chars -> runST $ runExceptionT_ $ runResourceT $ do
                    let tss = map T.pack chars
                        lbs = tenc $ TL.fromChunks tss
                        src = mconcat $ map (CL.sourceList . return) tss
                    bss <- src C.$= CT.encode cenc C.$$ CL.consume
                    return $ L.fromChunks bss == lbs
        go "utf8" TLE.encodeUtf8 CT.utf8
        go "utf16_le" TLE.encodeUtf16LE CT.utf16_le
        go "utf16_be" TLE.encodeUtf16BE CT.utf16_be
        go "utf32_le" TLE.encodeUtf32LE CT.utf32_le
        go "utf32_be" TLE.encodeUtf32BE CT.utf32_be
    describe "binary isolate" $ do
        it "works" $ do
            bss <- runResourceT $ CL.sourceList (replicate 1000 "X")
                           C.$= CB.isolate 6
                           C.$$ CL.consume
            S.concat bss @?= "XXXXXX"
#if !FAST
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
            C.liftBase $ lbs @=? L.fromChunks outBss
        it "works for lazy bytestring insertion" $ runResourceT $ do
            let builders = replicate 10000 (insertLazyByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            let src = mconcat $ map (CL.sourceList . return) builders
            outBss <- src C.$= builderToByteString C.$$ CL.consume :: C.ResourceT IO [S.ByteString]
            C.liftBase $ lbs @=? L.fromChunks outBss
#endif

it' :: String -> IO () -> Writer [Spec] ()
it' = it
