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
import Data.Conduit (runResourceT)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.IORef as I
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Control.Monad.Trans.Writer (Writer)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Monad.Trans.Resource (runExceptionT_, withIO, resourceForkIO)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (pure, (<$>), (<*>))

main :: IO ()
main = hspecX $ do
    describe "data loss rules" $ do
        {- FIXME
        it "sink yield" $ do
            x <- runResourceT $ CL.sourceList (map return [1..10 :: Int]) C.$$ do
                CL.drop 5
                C.yield [11..15] ()
                return ()
                CL.consume
            x @?= [11..15] ++ [6..10]
        -}

        it "consumes the source to quickly" $ do
            x <- runResourceT $ CL.sourceList [1..10 :: Int] C.$$ do
                  strings <- CL.map show C.=$ CL.take 5
                  liftIO $ putStr $ unlines strings
                  CL.fold (+) 0
            40 @?= x

        it "correctly consumes a chunked resource" $ do
            x <- runResourceT $ (CL.sourceList [1..5 :: Int] `mappend` CL.sourceList [6..10]) C.$$ do
                strings <- CL.map show C.=$ CL.take 5
                liftIO $ putStr $ unlines strings
                CL.fold (+) 0
            40 @?= x

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

    describe "Applicative instance for Sink" $ do
        it "<$> and <*>" $ do
            x <- runResourceT $ CL.sourceList [1..10] C.$$
                (+) <$> pure 5 <*> CL.fold (+) (0 :: Int)
            x @?= sum [1..10] + 5

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

        it "consumes all data" $ do
            x <- runResourceT $ CL.sourceList [1..10 :: Int] C.$$ do
                CL.isolate 5 C.=$ CL.sinkNull
                CL.consume
            x @?= [6..10]

    describe "lazy" $ do
        it' "works inside a ResourceT" $ runResourceT $ do
            counter <- liftIO $ I.newIORef 0
            let incr i = C.sourceIO
                    (liftIO $ I.newIORef $ C.Open (i :: Int))
                    (const $ return ())
                    (\istate -> do
                        res <- liftIO $ I.atomicModifyIORef istate
                            (\state -> (C.Closed, state))
                        case res of
                            C.Closed -> return ()
                            _ -> do
                                count <- liftIO $ I.atomicModifyIORef counter
                                    (\j -> (j + 1, j + 1))
                                liftIO $ count @?= i
                        return res
                            )
            nums <- CLazy.lazyConsume $ mconcat $ map incr [1..10]
            liftIO $ nums @?= [1..10]

    describe "sequenceSink" $ do
        it "simple sink" $ do
            let sink () = do
                    _ <- CL.drop 2
                    x <- CL.head
                    return $ C.Emit () $ maybe [] return x
            let conduit = C.sequenceSink () sink
            res <- runResourceT $ CL.sourceList [1..10 :: Int]
                           C.$= conduit
                           C.$$ CL.consume
            res @?= [3, 6, 9]
        it "finishes on new state" $ do
            let sink () = do
                x <- CL.head
                return $ C.Emit () $ maybe [] return x
            let conduit = C.sequenceSink () sink
            res <- runResourceT $ CL.sourceList [1..10 :: Int]
                        C.$= conduit C.$$ CL.consume
            res @?= [1..10]
        it "switch to a conduit" $ do
            let sink () = do
                _ <- CL.drop 4
                return $ C.StartConduit $ CL.filter even
            let conduit = C.sequenceSink () sink
            res <- runResourceT $ CL.sourceList [1..10 :: Int]
                            C.$= conduit
                            C.$$ CL.consume
            res @?= [6, 8, 10]
#endif

    describe "peek" $ do
        it "works" $ do
            (a, b) <- runResourceT $ CL.sourceList [1..10 :: Int] C.$$ do
                a <- CL.peek
                b <- CL.consume
                return (a, b)
            (a, b) @?= (Just 1, [1..10])

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

it' :: String -> IO () -> Writer [Spec] ()
it' = it
