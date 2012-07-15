{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

import qualified Data.Conduit as C
import qualified Data.Conduit.Util as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Lazy as CLazy
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Data.Conduit (runResourceT)
import Data.Maybe (fromMaybe)
import qualified Data.List as DL
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.IORef as I
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Monad.Trans.Resource (runExceptionT_, allocate, resourceForkIO)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (pure, (<$>), (<*>))
import Data.Functor.Identity (runIdentity)
import Control.Monad (forever)
import Data.Void (Void)

main :: IO ()
main = hspec $ do
    describe "data loss rules" $ do
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
            let w = allocate
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
            (runST $ CL.sourceList list C.$$ CL.fold (+) (0 :: Int))
            == sum list

    describe "unfold" $ do
        it "works" $ do
            let f 0 = Nothing
                f i = Just (show i, i - 1)
                seed = 10 :: Int
            x <- CL.unfold f seed C.$$ CL.consume
            let y = DL.unfoldr f seed
            x @?= y

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

    describe "zipping" $ do
        it "zipping two small lists" $ do
            res <- runResourceT $ C.zip (CL.sourceList [1..10]) (CL.sourceList [11..12]) C.$$ CL.consume
            res @=? zip [1..10 :: Int] [11..12 :: Int]

    describe "zipping sinks" $ do
        it "take all" $ do
            res <- runResourceT $ CL.sourceList [1..10] C.$$ C.zipSinks CL.consume CL.consume
            res @=? ([1..10 :: Int], [1..10 :: Int])
        it "take fewer on left" $ do
            res <- runResourceT $ CL.sourceList [1..10] C.$$ C.zipSinks (CL.take 4) CL.consume
            res @=? ([1..4 :: Int], [1..10 :: Int])
        it "take fewer on right" $ do
            res <- runResourceT $ CL.sourceList [1..10] C.$$ C.zipSinks CL.consume (CL.take 4)
            res @=? ([1..10 :: Int], [1..4 :: Int])

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
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 C.$$+ CL.take 5
                (src3, y) <- src2 C.$$++ CL.fold (+) 0
                z <- src3 C.$$+- CL.consume
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

        it "groupBy" $ do
            let input = [1::Int, 1, 2, 3, 3, 3, 4, 5, 5]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.groupBy (==)
                    C.=$ CL.consume
            x @?= DL.groupBy (==) input

        it "groupBy (nondup begin/end)" $ do
            let input = [1::Int, 2, 3, 3, 3, 4, 5]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.groupBy (==)
                    C.=$ CL.consume
            x @?= DL.groupBy (==) input

        it "mapMaybe" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.mapMaybe ((+2) <$>)
                    C.=$ CL.consume
            x @?= [3, 4, 5]

        it "mapMaybeM" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.mapMaybeM (return . ((+2) <$>))
                    C.=$ CL.consume
            x @?= [3, 4, 5]

        it "catMaybes" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.catMaybes
                    C.=$ CL.consume
            x @?= [1, 2, 3]

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
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 C.$= CL.isolate 5 C.$$+ CL.consume
                y <- src2 C.$$+- CL.consume
                return (x, y)
            x @?= [1..5]
            y @?= []

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
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 C.$$+ CL.isolate 5 C.=$ CL.consume
                y <- src2 C.$$+- CL.consume
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
                    (liftIO $ I.newIORef $ C.IOOpen (i :: Int))
                    (const $ return ())
                    (\istate -> do
                        res <- liftIO $ I.atomicModifyIORef istate
                            (\state -> (C.IOClosed, state))
                        case res of
                            C.IOClosed -> return ()
                            _ -> do
                                count <- liftIO $ I.atomicModifyIORef counter
                                    (\j -> (j + 1, j + 1))
                                liftIO $ count @?= i
                        return res
                            )
            nums <- CLazy.lazyConsume $ mconcat $ map incr [1..10]
            liftIO $ nums @?= [1..10]

        it' "returns nothing outside ResourceT" $ do
            bss <- runResourceT $ CLazy.lazyConsume $ CB.sourceFile "test/main.hs"
            bss @?= []

    describe "sequence" $ do
        it "simple sink" $ do
            let sumSink = do
                    ma <- CL.head
                    case ma of
                        Nothing -> return 0
                        Just a  -> (+a) . fromMaybe 0 <$> CL.head

            res <- runResourceT $ CL.sourceList [1..11 :: Int]
                             C.$= CL.sequence sumSink
                             C.$$ CL.consume
            res @?= [3, 7, 11, 15, 19, 11]

        it "sink with unpull behaviour" $ do
            let sumSink = do
                    ma <- CL.head
                    case ma of
                        Nothing -> return 0
                        Just a  -> (+a) . fromMaybe 0 <$> CL.peek

            res <- runResourceT $ CL.sourceList [1..11 :: Int]
                             C.$= CL.sequence sumSink
                             C.$$ CL.consume
            res @?= [3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 11]


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
                prop (enc ++ " single chunk") $ \chars -> runST $ runExceptionT_ $ do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = CL.sourceList $ L.toChunks lbs
                    ts <- src C.$= CT.decode cenc C.$$ CL.consume
                    return $ TL.fromChunks ts == tl
                prop (enc ++ " many chunks") $ \chars -> runIdentity $ runExceptionT_ $ do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = mconcat $ map (CL.sourceList . return . S.singleton) $ L.unpack lbs
                    ts <- src C.$= CT.decode cenc C.$$ CL.consume
                    return $ TL.fromChunks ts == tl
                prop (enc ++ " encoding") $ \chars -> runIdentity $ runExceptionT_ $ do
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

    describe "text lines" $ do
        it "works across split lines" $
            (CL.sourceList [T.pack "abc", T.pack "d\nef"] C.$= CT.lines C.$$ CL.consume) ==
                [[T.pack "abcd", T.pack "ef"]]
        it "works with multiple lines in an item" $
            (CL.sourceList [T.pack "ab\ncd\ne"] C.$= CT.lines C.$$ CL.consume) ==
                [[T.pack "ab", T.pack "cd", T.pack "e"]]
        it "works with ending on a newline" $
            (CL.sourceList [T.pack "ab\n"] C.$= CT.lines C.$$ CL.consume) ==
                [[T.pack "ab"]]
        it "works with ending a middle item on a newline" $
            (CL.sourceList [T.pack "ab\n", T.pack "cd\ne"] C.$= CT.lines C.$$ CL.consume) ==
                [[T.pack "ab", T.pack "cd", T.pack "e"]]
        it "is not too eager" $ do
            x <- CL.sourceList ["foobarbaz", error "ignore me"] C.$$ CT.decode CT.utf8 C.=$ CL.head
            x @?= Just "foobarbaz"

    describe "binary isolate" $ do
        it "works" $ do
            bss <- runResourceT $ CL.sourceList (replicate 1000 "X")
                           C.$= CB.isolate 6
                           C.$$ CL.consume
            S.concat bss @?= "XXXXXX"
    describe "unbuffering" $ do
        it "works" $ do
            x <- runResourceT $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, ()) <- src1 C.$$+ CL.drop 5
                src2 C.$$+- CL.fold (+) 0
            x @?= sum [6..10]

    describe "operators" $ do
        it "only use =$=" $
            runIdentity
            (    CL.sourceList [1..10 :: Int]
              C.$$ CL.map (+ 1)
             C.=$  CL.map (subtract 1)
             C.=$  CL.mapM (return . (* 2))
             C.=$  CL.map (`div` 2)
             C.=$  CL.fold (+) 0
            ) @?= sum [1..10]
        it "only use =$" $
            runIdentity
            (    CL.sourceList [1..10 :: Int]
              C.$$ CL.map (+ 1)
              C.=$ CL.map (subtract 1)
              C.=$ CL.map (* 2)
              C.=$ CL.map (`div` 2)
              C.=$ CL.fold (+) 0
            ) @?= sum [1..10]
        it "chain" $ do
            x <-      CL.sourceList [1..10 :: Int]
                C.$=  CL.map (+ 1)
                C.$= CL.map (+ 1)
                C.$=  CL.map (+ 1)
                C.$= CL.map (subtract 3)
                C.$= CL.map (* 2)
                C.$$  CL.map (`div` 2)
                C.=$  CL.map (+ 1)
                C.=$  CL.map (+ 1)
                C.=$  CL.map (+ 1)
                C.=$  CL.map (subtract 3)
                C.=$  CL.fold (+) 0
            x @?= sum [1..10]


    describe "properly using binary file reading" $ do
        it "sourceFile" $ do
            x <- runResourceT $ CB.sourceFile "test/random" C.$$ CL.consume
            lbs <- L.readFile "test/random"
            L.fromChunks x @?= lbs

    describe "binary head" $ do
        let go lbs = do
                x <- CB.head
                case (x, L.uncons lbs) of
                    (Nothing, Nothing) -> return True
                    (Just y, Just (z, lbs'))
                        | y == z -> go lbs'
                    _ -> return False

        prop "works" $ \bss' ->
            let bss = map S.pack bss'
             in runIdentity $
                CL.sourceList bss C.$$ go (L.fromChunks bss)
    describe "binary takeWhile" $ do
        prop "works" $ \bss' ->
            let bss = map S.pack bss'
             in runIdentity $ do
                bss2 <- CL.sourceList bss C.$$ CB.takeWhile (>= 5) C.=$ CL.consume
                return $ L.fromChunks bss2 == L.takeWhile (>= 5) (L.fromChunks bss)
        prop "leftovers present" $ \bss' ->
            let bss = map S.pack bss'
             in runIdentity $ do
                result <- CL.sourceList bss C.$$ do
                    x <- CB.takeWhile (>= 5) C.=$ CL.consume
                    y <- CL.consume
                    return (S.concat x, S.concat y)
                let expected = S.span (>= 5) $ S.concat bss
                if result == expected
                    then return True
                    else error $ show (S.concat bss, result, expected)

    describe "binary dropWhile" $ do
        prop "works" $ \bss' ->
            let bss = map S.pack bss'
             in runIdentity $ do
                bss2 <- CL.sourceList bss C.$$ do
                    CB.dropWhile (< 5)
                    CL.consume
                return $ L.fromChunks bss2 == L.dropWhile (< 5) (L.fromChunks bss)

    describe "binary take" $ do
      let go n l = CL.sourceList l C.$$ do
          a <- CB.take n
          b <- CL.consume
          return (a, b)

      -- Taking nothing should result in an empty Bytestring
      it "nothing" $ do
        (a, b) <- runResourceT $ go 0 ["abc", "defg"]
        a              @?= L.empty
        L.fromChunks b @?= "abcdefg"

      it "normal" $ do
        (a, b) <- runResourceT $ go 4 ["abc", "defg"]
        a              @?= "abcd"
        L.fromChunks b @?= "efg"

      -- Taking exactly the data that is available should result in no
      -- leftover.
      it "all" $ do
        (a, b) <- runResourceT $ go 7 ["abc", "defg"]
        a @?= "abcdefg"
        b @?= []

      -- Take as much as possible.
      it "more" $ do
        (a, b) <- runResourceT $ go 10 ["abc", "defg"]
        a @?= "abcdefg"
        b @?= []

    describe "normalFuseLeft" $ do
        it "does not double close conduit" $ do
            x <- runResourceT $ do
                let src = CL.sourceList ["foobarbazbin"]
                src C.$= CB.isolate 10 C.$$ CL.head
            x @?= Just "foobarbazb"

    describe "binary" $ do
        prop "lines" $ \bss' -> runIdentity $ do
            let bss = map S.pack bss'
                bs = S.concat bss
                src = CL.sourceList bss
            res <- src C.$$ CB.lines C.=$ CL.consume
            return $ S8.lines bs == res

    describe "termination" $ do
        it "terminates early" $ do
            let src = forever $ CI.yield ()
            x <- src C.$$ CL.head
            x @?= Just ()
        it "bracket" $ do
            ref <- I.newIORef (0 :: Int)
            let src = CI.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ CI.yield (1 :: Int))
            val <- C.runResourceT $ src C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            val @?= 10
            i <- I.readIORef ref
            i @?= 3
        it "bracket skipped if not needed" $ do
            ref <- I.newIORef (0 :: Int)
            let src = CI.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ CI.yield (1 :: Int))
                src' = CL.sourceList $ repeat 1
            val <- C.runResourceT $ (src' >> src) C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            val @?= 10
            i <- I.readIORef ref
            i @?= 0
        it "bracket + toPipe" $ do
            ref <- I.newIORef (0 :: Int)
            let src = CI.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ CI.yield (1 :: Int))
            val <- C.runResourceT $ src C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            val @?= 10
            i <- I.readIORef ref
            i @?= 3
        it "bracket skipped if not needed" $ do
            ref <- I.newIORef (0 :: Int)
            let src = CI.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ CI.yield (1 :: Int))
                src' = CL.sourceList $ repeat 1
            val <- C.runResourceT $ (src' >> src) C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            val @?= 10
            i <- I.readIORef ref
            i @?= 0

    describe "invariant violations" $ do
        it "leftovers without input" $ do
            ref <- I.newIORef []
            let add x = I.modifyIORef ref (x:)
                adder = CI.NeedInput (\a -> liftIO (add a) >> adder) return
                residue x = CI.Leftover (CI.Done ()) x

            _ <- CI.yield 1 C.$$ adder
            x <- I.readIORef ref
            x @?= [1 :: Int]
            I.writeIORef ref []

            _ <- CI.yield 1 C.$$ (residue 2 >> residue 3) >> adder
            y <- I.readIORef ref
            y @?= [1, 2, 3]
            I.writeIORef ref []

            _ <- CI.yield 1 C.$$ residue 2 >> (residue 3 >> adder)
            z <- I.readIORef ref
            z @?= [1, 2, 3]
            I.writeIORef ref []

    describe "sane yield/await'" $ do
        it' "yield terminates" $ do
            let is = [1..10] ++ undefined
                src [] = return ()
                src (x:xs) = CI.yield x >> src xs
            x <- src is C.$$ CL.take 10
            x @?= [1..10 :: Int]
        it' "yield terminates (2)" $ do
            let is = [1..10] ++ undefined
            x <- mapM_ CI.yield is C.$$ CL.take 10
            x @?= [1..10 :: Int]
        it' "yieldOr finalizer called" $ do
            iref <- I.newIORef (0 :: Int)
            let src = mapM_ (\i -> C.yieldOr i $ I.writeIORef iref i) [1..]
            src C.$$ CL.isolate 10 C.=$ CL.sinkNull
            x <- I.readIORef iref
            x @?= 10

    describe "upstream results" $ do
        it' "works" $ do
            let foldUp :: (b -> a -> b) -> b -> C.Pipe l a Void u IO (u, b)
                foldUp f b = C.awaitE >>= either (\u -> return (u, b)) (\a -> let b' = f b a in b' `seq` foldUp f b')
                passFold :: (b -> a -> b) -> b -> C.Pipe l a a () IO b
                passFold f b = C.await >>= maybe (return b) (\a -> let b' = f b a in b' `seq` C.yield a >> passFold f b')
            (x, y) <- CI.runPipe $ CL.sourceList [1..10 :: Int] C.>+> passFold (+) 0 C.>+>  foldUp (*) 1
            (x, y) @?= (sum [1..10], product [1..10])

    describe "input/output mapping" $ do
        it' "mapOutput" $ do
            x <- CI.mapOutput (+ 1) (CL.sourceList [1..10 :: Int]) C.$$ CL.fold (+) 0
            x @?= sum [2..11]
        it' "mapOutputMaybe" $ do
            x <- CI.mapOutputMaybe (\i -> if even i then Just i else Nothing) (CL.sourceList [1..10 :: Int]) C.$$ CL.fold (+) 0
            x @?= sum [2, 4..10]
        it' "mapInput" $ do
            xyz <- (CL.sourceList $ map show [1..10 :: Int]) C.$$ do
                (x, y) <- CI.mapInput read (Just . show) $ ((do
                    x <- CL.isolate 5 C.=$ CL.fold (+) 0
                    y <- CL.peek
                    return (x :: Int, y :: Maybe Int)) :: C.Sink Int IO (Int, Maybe Int))
                z <- CL.consume
                return (x, y, concat z)

            xyz @?= (sum [1..5], Just 6, "678910")

    describe "left/right identity" $ do
        it' "left identity" $ do
            x <- CL.sourceList [1..10 :: Int] C.$$ CI.idP C.=$ CL.fold (+) 0
            y <- CL.sourceList [1..10 :: Int] C.$$ CL.fold (+) 0
            x @?= y
        it' "right identity" $ do
            x <- CI.runPipe $ CL.sourceList [1..10 :: Int] C.>+> CL.fold (+) 0 C.>+> CI.idP
            y <- CI.runPipe $ CL.sourceList [1..10 :: Int] C.>+> CL.fold (+) 0
            x @?= y

    describe "generalizing" $ do
        it' "works" $ do
            x <-     C.runPipe
                   $ CI.sourceToPipe  (CL.sourceList [1..10 :: Int])
               C.>+> CI.conduitToPipe (CL.map (+ 1))
               C.>+> CI.sinkToPipe    (CL.fold (+) 0)
            x @?= sum [2..11]

    describe "withUpstream" $ do
        it' "works" $ do
            let src = CL.sourceList [1..10 :: Int] >> return True
                sink = C.withUpstream $ CL.fold (+) 0
            res <- C.runPipe $ src C.>+> sink
            res @?= (True, sum [1..10])

    describe "iterate" $ do
        it' "works" $ do
            res <- CL.iterate (+ 1) (1 :: Int) C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            res @?= sum [1..10]

    describe "unwrapResumable" $ do
        it' "works" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
                    C.yieldOr () $ I.writeIORef ref 3
            (rsrc0, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            x0 @?= 0

            (_, final) <- C.unwrapResumable rsrc0

            x1 <- I.readIORef ref
            x1 @?= 0

            final

            x2 <- I.readIORef ref
            x2 @?= 1

        it' "isn't called twice" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
            (rsrc0, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            x0 @?= 0

            (src1, final) <- C.unwrapResumable rsrc0

            x1 <- I.readIORef ref
            x1 @?= 0

            Just () <- src1 C.$$ CL.head

            x2 <- I.readIORef ref
            x2 @?= 2

            final

            x3 <- I.readIORef ref
            x3 @?= 2

        it' "source isn't used" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
            (rsrc0, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            x0 @?= 0

            (src1, final) <- C.unwrapResumable rsrc0

            x1 <- I.readIORef ref
            x1 @?= 0

            () <- src1 C.$$ return ()

            x2 <- I.readIORef ref
            x2 @?= 0

            final

            x3 <- I.readIORef ref
            x3 @?= 1
    describe "injectLeftovers" $ do
        it "works" $ do
            let src = CL.sourceList [1..10 :: Int]
                conduit = C.awaitForever $ \i -> do
                    js <- CL.take 2
                    mapM_ C.leftover $ reverse js
                    C.yield i
            res <- (src C.>+> C.injectLeftovers conduit) C.$$ CL.consume
            res @?= [1..10]

it' :: String -> IO () -> Spec
it' = it
