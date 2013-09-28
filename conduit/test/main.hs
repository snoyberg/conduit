{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Data.Conduit as C
import qualified Data.Conduit.Util as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Lazy as CLazy
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Data.Conduit (runResourceT, Pipe)
import Data.Maybe   (fromMaybe,catMaybes)
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
import Control.Monad.Trans.Resource (runExceptionT, runExceptionT_, allocate, resourceForkIO)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (execWriter, tell, runWriterT, runWriter, Writer)
import Control.Monad.Trans.State (evalStateT, get, put)
import Control.Applicative (pure, (<$>), (<*>))
import Data.Functor.Identity (Identity,runIdentity)
import Control.Monad (forever, void)
import qualified Control.Concurrent.MVar as M
import Control.Monad.Error (catchError, throwError, Error)
import Data.Void (Void)
import Control.Monad.Morph (hoist)

(@=?) :: (Eq a, Show a) => a -> a -> IO ()
(@=?) = flip shouldBe

-- Quickcheck property for testing equivalence of list processing
-- functions and their conduit counterparts
equivToList :: Eq b => ([a] -> [b]) -> C.Conduit a Identity b -> [a] -> Bool
equivToList f conduit xs =
  f xs == runIdentity (CL.sourceList xs C.$$ conduit C.=$ CL.consume)


main :: IO ()
main = hspec $ do
{-
    describe "data loss rules" $ do
        it "consumes the source to quickly" $ do
            x <- runResourceT $ CL.sourceList [1..10 :: Int] C.$$ do
                  strings <- CL.map show C.=$ CL.take 5
                  liftIO $ putStr $ unlines strings
                  CL.fold (+) 0
            40 `shouldBe` x

        it "correctly consumes a chunked resource" $ do
            x <- runResourceT $ (CL.sourceList [1..5 :: Int] `mappend` CL.sourceList [6..10]) C.$$ do
                strings <- CL.map show C.=$ CL.take 5
                liftIO $ putStr $ unlines strings
                CL.fold (+) 0
            40 `shouldBe` x

    describe "filter" $ do
        it "even" $ do
            x <- runResourceT $ CL.sourceList [1..10] C.$$ CL.filter even C.=$ CL.consume
            x `shouldBe` filter even [1..10 :: Int]

    prop "concat" $ equivToList (concat :: [[Int]]->[Int]) CL.concat

    describe "mapFoldable" $ do
        prop "list" $
            equivToList (concatMap (:[]) :: [Int]->[Int]) (CL.mapFoldable  (:[]))
        let f x = if odd x then Just x else Nothing
        prop "Maybe" $
            equivToList (catMaybes . map f :: [Int]->[Int]) (CL.mapFoldable f)

    prop "scanl" $ equivToList (tail . scanl (+) 0 :: [Int]->[Int]) (CL.scanl (\a s -> (a+s,a+s)) 0)

    -- mapFoldableM and scanlM are fully polymorphic in type of monad
    -- so it suffice to check only with Identity.
    describe "mapFoldableM" $ do
        prop "list" $
            equivToList (concatMap (:[]) :: [Int]->[Int]) (CL.mapFoldableM (return . (:[])))
        let f x = if odd x then Just x else Nothing
        prop "Maybe" $
            equivToList (catMaybes . map f :: [Int]->[Int]) (CL.mapFoldableM (return . f))

    prop "scanl" $ equivToList (tail . scanl (+) 0 :: [Int]->[Int]) (CL.scanlM (\a s -> return (a+s,a+s)) 0)

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
            res `shouldBe` (0 :: Int)

    describe "sum" $ do
        it "works for 1..10" $ do
            x <- runResourceT $ CL.sourceList [1..10] C.$$ CL.fold (+) (0 :: Int)
            x `shouldBe` sum [1..10]
        prop "is idempotent" $ \list ->
            (runST $ CL.sourceList list C.$$ CL.fold (+) (0 :: Int))
            == sum list

    describe "foldMap" $ do
        it "sums 1..10" $ do
            Sum x <- CL.sourceList [1..(10 :: Int)] C.$$ CL.foldMap Sum
            x `shouldBe` sum [1..10]

        it "preserves order" $ do
            x <- CL.sourceList [[4],[2],[3],[1]] C.$$ CL.foldMap (++[(9 :: Int)])
            x `shouldBe` [4,9,2,9,3,9,1,9]

    describe "foldMapM" $ do
        it "sums 1..10" $ do
            Sum x <- CL.sourceList [1..(10 :: Int)] C.$$ CL.foldMapM (return . Sum)
            x `shouldBe` sum [1..10]

        it "preserves order" $ do
            x <- CL.sourceList [[4],[2],[3],[1]] C.$$ CL.foldMapM (return . (++[(9 :: Int)]))
            x `shouldBe` [4,9,2,9,3,9,1,9]

    describe "unfold" $ do
        it "works" $ do
            let f 0 = Nothing
                f i = Just (show i, i - 1)
                seed = 10 :: Int
            x <- CL.unfold f seed C.$$ CL.consume
            let y = DL.unfoldr f seed
            x `shouldBe` y

    describe "Monoid instance for Source" $ do
        it "mappend" $ do
            x <- runResourceT $ (CL.sourceList [1..5 :: Int] `mappend` CL.sourceList [6..10]) C.$$ CL.fold (+) 0
            x `shouldBe` sum [1..10]
        it "mconcat" $ do
            x <- runResourceT $ mconcat
                [ CL.sourceList [1..5 :: Int]
                , CL.sourceList [6..10]
                , CL.sourceList [11..20]
                ] C.$$ CL.fold (+) 0
            x `shouldBe` sum [1..20]

    describe "file access" $ do
        it "read" $ do
            bs <- S.readFile "conduit.cabal"
            bss <- runResourceT $ CB.sourceFile "conduit.cabal" C.$$ CL.consume
            bs @=? S.concat bss

        it "read range" $ do
            S.writeFile "tmp" "0123456789"
            bss <- runResourceT $ CB.sourceFileRange "tmp" (Just 2) (Just 3) C.$$ CL.consume
            S.concat bss `shouldBe` "234"

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
            x `shouldBe` sum [6..10]

    describe "Applicative instance for Sink" $ do
        it "<$> and <*>" $ do
            x <- runResourceT $ CL.sourceList [1..10] C.$$
                (+) <$> pure 5 <*> CL.fold (+) (0 :: Int)
            x `shouldBe` sum [1..10] + 5

    describe "resumable sources" $ do
        it "simple" $ do
            (x, y, z) <- runResourceT $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 C.$$+ CL.take 5
                (src3, y) <- src2 C.$$+ CL.fold (+) 0
                z <- src3 C.$$ CL.consume
                return (x, y, z)
            x `shouldBe` [1..5] :: IO ()
            y `shouldBe` sum [6..10]
            z `shouldBe` []

    describe "conduits" $ do
        it "map, left" $ do
            x <- runResourceT $
                CL.sourceList [1..10]
                    C.$= CL.map (* 2)
                    C.$$ CL.fold (+) 0
            x `shouldBe` 2 * sum [1..10 :: Int]

        it "map, left >+>" $ do
            x <- runResourceT $
                    (CL.sourceList [1..10]
                    CI.>+> (CL.map (* 2)))
                    C.$$ CL.fold (+) 0
            x `shouldBe` 2 * sum [1..10 :: Int]

        it "map, right" $ do
            x <- runResourceT $
                CL.sourceList [1..10]
                    C.$$ CL.map (* 2)
                    C.=$ CL.fold (+) 0
            x `shouldBe` 2 * sum [1..10 :: Int]

        it "groupBy" $ do
            let input = [1::Int, 1, 2, 3, 3, 3, 4, 5, 5]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.groupBy (==)
                    C.=$ CL.consume
            x `shouldBe` DL.groupBy (==) input

        it "groupBy (nondup begin/end)" $ do
            let input = [1::Int, 2, 3, 3, 3, 4, 5]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.groupBy (==)
                    C.=$ CL.consume
            x `shouldBe` DL.groupBy (==) input

        it "mapMaybe" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.mapMaybe ((+2) <$>)
                    C.=$ CL.consume
            x `shouldBe` [3, 4, 5]

        it "mapMaybeM" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.mapMaybeM (return . ((+2) <$>))
                    C.=$ CL.consume
            x `shouldBe` [3, 4, 5]

        it "catMaybes" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.catMaybes
                    C.=$ CL.consume
            x `shouldBe` [1, 2, 3]

        it "concatMap" $ do
            let input = [1, 11, 21]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.concatMap (\i -> enumFromTo i (i + 9))
                    C.=$ CL.fold (+) (0 :: Int)
            x `shouldBe` sum [1..30]

        it "bind together" $ do
            let conduit = CL.map (+ 5) C.=$= CL.map (* 2)
            x <- runResourceT $ CL.sourceList [1..10] C.$= conduit C.$$ CL.fold (+) 0
            x `shouldBe` sum (map (* 2) $ map (+ 5) [1..10 :: Int])

#if !FAST
    describe "isolate" $ do
        it "bound to resumable source" $ do
            (x, y) <- runResourceT $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 C.$= CL.isolate 5 C.$$+ CL.consume
                y <- src2 C.$$ CL.consume
                return (x, y)
            x `shouldBe` [1..5]
            y `shouldBe` []

        it "bound to sink, non-resumable" $ do
            (x, y) <- runResourceT $ do
                CL.sourceList [1..10 :: Int] C.$$ do
                    x <- CL.isolate 5 C.=$ CL.consume
                    y <- CL.consume
                    return (x, y)
            x `shouldBe` [1..5]
            y `shouldBe` [6..10]

        it "bound to sink, resumable" $ do
            (x, y) <- runResourceT $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 C.$$+ CL.isolate 5 C.=$ CL.consume
                y <- src2 C.$$ CL.consume
                return (x, y)
            x `shouldBe` [1..5]
            y `shouldBe` [6..10]

        it "consumes all data" $ do
            x <- runResourceT $ CL.sourceList [1..10 :: Int] C.$$ do
                CL.isolate 5 C.=$ CL.sinkNull
                CL.consume
            x `shouldBe` [6..10]

    describe "lazy" $ do
        it' "works inside a ResourceT" $ runResourceT $ do
            counter <- liftIO $ I.newIORef 0
            let incr i = do
                    istate <- liftIO $ I.newIORef $ Just (i :: Int)
                    let loop = do
                            res <- liftIO $ I.atomicModifyIORef istate ((,) Nothing)
                            case res of
                                Nothing -> return ()
                                Just x -> do
                                    count <- liftIO $ I.atomicModifyIORef counter
                                        (\j -> (j + 1, j + 1))
                                    liftIO $ count `shouldBe` i
                                    C.yield x
                                    loop
                    loop
            nums <- CLazy.lazyConsume $ mconcat $ map incr [1..10]
            liftIO $ nums `shouldBe` [1..10]

        it' "returns nothing outside ResourceT" $ do
            bss <- runResourceT $ CLazy.lazyConsume $ CB.sourceFile "test/main.hs"
            bss `shouldBe` []

        it' "works with pure sources" $ do
            nums <- CLazy.lazyConsume $ forever $ C.yield 1
            take 100 nums `shouldBe` replicate 100 (1 :: Int)

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
            res `shouldBe` [3, 7, 11, 15, 19, 11]

        it "sink with unpull behaviour" $ do
            let sumSink = do
                    ma <- CL.head
                    case ma of
                        Nothing -> return 0
                        Just a  -> (+a) . fromMaybe 0 <$> CL.peek

            res <- runResourceT $ CL.sourceList [1..11 :: Int]
                             C.$= CL.sequence sumSink
                             C.$$ CL.consume
            res `shouldBe` [3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 11]

#endif

    describe "peek" $ do
        it "works" $ do
            (a, b) <- runResourceT $ CL.sourceList [1..10 :: Int] C.$$ do
                a <- CL.peek
                b <- CL.consume
                return (a, b)
            (a, b) `shouldBe` (Just 1, [1..10])

    describe "text" $ do
        let go enc tenc tdec cenc = do
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

                -- Check whether raw bytes are decoded correctly, in
                -- particular that Text decoding produces an error if
                -- and only if Conduit does.
                prop (enc ++ " raw bytes") $ \bytes ->
                    let lbs = L.pack bytes
                        src = CL.sourceList $ L.toChunks lbs
                        etl = C.runException $ src C.$= CT.decode cenc C.$$ CL.consume
                        tl' = tdec lbs
                    in  case etl of
                          (Left _) -> (return $! TL.toStrict tl') `shouldThrow` anyException
                          (Right tl) -> TL.fromChunks tl `shouldBe` tl'
                prop (enc ++ " encoding") $ \chars -> runIdentity $ runExceptionT_ $ do
                    let tss = map T.pack chars
                        lbs = tenc $ TL.fromChunks tss
                        src = mconcat $ map (CL.sourceList . return) tss
                    bss <- src C.$= CT.encode cenc C.$$ CL.consume
                    return $ L.fromChunks bss == lbs
        go "utf8" TLE.encodeUtf8 TLE.decodeUtf8 CT.utf8
        go "utf16_le" TLE.encodeUtf16LE TLE.decodeUtf16LE CT.utf16_le
        go "utf16_be" TLE.encodeUtf16BE TLE.decodeUtf16BE CT.utf16_be
        go "utf32_le" TLE.encodeUtf32LE TLE.decodeUtf32LE CT.utf32_le
        go "utf32_be" TLE.encodeUtf32BE TLE.decodeUtf32BE CT.utf32_be

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
            x `shouldBe` Just "foobarbaz"

    describe "text lines bounded" $ do
        it "works across split lines" $
            (CL.sourceList [T.pack "abc", T.pack "d\nef"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[T.pack "abcd", T.pack "ef"]]
        it "works with multiple lines in an item" $
            (CL.sourceList [T.pack "ab\ncd\ne"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[T.pack "ab", T.pack "cd", T.pack "e"]]
        it "works with ending on a newline" $
            (CL.sourceList [T.pack "ab\n"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[T.pack "ab"]]
        it "works with ending a middle item on a newline" $
            (CL.sourceList [T.pack "ab\n", T.pack "cd\ne"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[T.pack "ab", T.pack "cd", T.pack "e"]]
        it "is not too eager" $ do
            x <- CL.sourceList ["foobarbaz", error "ignore me"] C.$$ CT.decode CT.utf8 C.=$ CL.head
            x `shouldBe` Just "foobarbaz"
        it "throws an exception when lines are too long" $ do
            x <- C.runExceptionT $ CL.sourceList ["hello\nworld"] C.$$ CT.linesBounded 4 C.=$ CL.consume
            show x `shouldBe` show (Left $ CT.LengthExceeded 4 :: Either CT.TextException ())

    describe "binary isolate" $ do
        it "works" $ do
            bss <- runResourceT $ CL.sourceList (replicate 1000 "X")
                           C.$= CB.isolate 6
                           C.$$ CL.consume
            S.concat bss `shouldBe` "XXXXXX"
    describe "unbuffering" $ do
        it "works" $ do
            x <- runResourceT $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, ()) <- src1 C.$$+ CL.drop 5
                src2 C.$$ CL.fold (+) 0
            x `shouldBe` sum [6..10]

    describe "operators" $ do
        it "only use =$=" $
            runIdentity
            (    CL.sourceList [1..10 :: Int]
              C.$$ CL.map (+ 1)
             C.=$  CL.map (subtract 1)
             C.=$  CL.mapM (return . (* 2))
             C.=$  CL.map (`div` 2)
             C.=$  CL.fold (+) 0
            ) `shouldBe` sum [1..10]
        it "only use =$" $
            runIdentity
            (    CL.sourceList [1..10 :: Int]
              C.$$ CL.map (+ 1)
              C.=$ CL.map (subtract 1)
              C.=$ CL.map (* 2)
              C.=$ CL.map (`div` 2)
              C.=$ CL.fold (+) 0
            ) `shouldBe` sum [1..10]
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
            x `shouldBe` sum [1..10]


    describe "properly using binary file reading" $ do
        it "sourceFile" $ do
            x <- runResourceT $ CB.sourceFile "test/random" C.$$ CL.consume
            lbs <- L.readFile "test/random"
            L.fromChunks x `shouldBe` lbs

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
        a              `shouldBe` L.empty
        L.fromChunks b `shouldBe` "abcdefg"

      it "normal" $ do
        (a, b) <- runResourceT $ go 4 ["abc", "defg"]
        a              `shouldBe` "abcd"
        L.fromChunks b `shouldBe` "efg"

      -- Taking exactly the data that is available should result in no
      -- leftover.
      it "all" $ do
        (a, b) <- runResourceT $ go 7 ["abc", "defg"]
        a `shouldBe` "abcdefg"
        b `shouldBe` []

      -- Take as much as possible.
      it "more" $ do
        (a, b) <- runResourceT $ go 10 ["abc", "defg"]
        a `shouldBe` "abcdefg"
        b `shouldBe` []

    describe "normalFuseLeft" $ do
        it "does not double close conduit" $ do
            x <- runResourceT $ do
                let src = CL.sourceList ["foobarbazbin"]
                src C.$= CB.isolate 10 C.$$ CL.head
            x `shouldBe` Just "foobarbazb"

    describe "binary" $ do
        prop "lines" $ \bss' -> runIdentity $ do
            let bss = map S.pack bss'
                bs = S.concat bss
                src = CL.sourceList bss
            res <- src C.$$ CB.lines C.=$ CL.consume
            return $ S8.lines bs == res

    describe "termination" $ do
        it "terminates early" $ do
            let src = forever $ C.yield ()
            x <- src C.$$ CL.head
            x `shouldBe` Just ()
        it "bracket" $ do
            ref <- I.newIORef (0 :: Int)
            let src = C.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ C.yield (1 :: Int))
            val <- C.runResourceT $ src C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            val `shouldBe` 10
            i <- I.readIORef ref
            i `shouldBe` 3
        it "bracket skipped if not needed" $ do
            ref <- I.newIORef (0 :: Int)
            let src = C.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ C.yield (1 :: Int))
                src' = CL.sourceList $ repeat 1
            val <- C.runResourceT $ (src' >> src) C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            val `shouldBe` 10
            i <- I.readIORef ref
            i `shouldBe` 0
        it "bracket + toPipe" $ do
            ref <- I.newIORef (0 :: Int)
            let src = C.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ C.yield (1 :: Int))
            val <- C.runResourceT $ src C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            val `shouldBe` 10
            i <- I.readIORef ref
            i `shouldBe` 3
        it "bracket skipped if not needed" $ do
            ref <- I.newIORef (0 :: Int)
            let src = C.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ C.yield (1 :: Int))
                src' = CL.sourceList $ repeat 1
            val <- C.runResourceT $ (src' >> src) C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            val `shouldBe` 10
            i <- I.readIORef ref
            i `shouldBe` 0

    describe "invariant violations" $ do
        it "leftovers without input" $ do
            ref <- I.newIORef []
            let add x = I.modifyIORef ref (x:)
                adder' = CI.await >>= maybe (return ()) (\a -> liftIO (add a) >> adder')
                adder = adder'
                residue x = CI.leftover x

            _ <- C.yield 1 C.$$ adder
            x <- I.readIORef ref
            x `shouldBe` [1 :: Int]
            I.writeIORef ref []

            _ <- C.yield 1 C.$$ (residue 2 >> residue 3) >> adder
            y <- I.readIORef ref
            y `shouldBe` [1, 2, 3]
            I.writeIORef ref []

            _ <- C.yield 1 C.$$ residue 2 >> (residue 3 >> adder)
            z <- I.readIORef ref
            z `shouldBe` [1, 2, 3]
            I.writeIORef ref []

    describe "sane yield/await'" $ do
        it' "yield terminates" $ do
            let is = [1..10] ++ undefined
                src [] = return ()
                src (x:xs) = C.yield x >> src xs
            x <- src is C.$$ CL.take 10
            x `shouldBe` [1..10 :: Int]
        it' "yield terminates (2)" $ do
            let is = [1..10] ++ undefined
            x <- mapM_ C.yield is C.$$ CL.take 10
            x `shouldBe` [1..10 :: Int]
        it' "yieldOr finalizer called" $ do
            iref <- I.newIORef (0 :: Int)
            let src = mapM_ (\i -> C.yieldOr i $ I.writeIORef iref i) [1..]
            src C.$$ CL.isolate 10 C.=$ CL.sinkNull
            x <- I.readIORef iref
            x `shouldBe` 10

    describe "input/output mapping" $ do
        it' "mapOutput" $ do
            x <- C.mapOutput (+ 1) (CL.sourceList [1..10 :: Int]) C.$$ CL.fold (+) 0
            x `shouldBe` sum [2..11]
        it' "mapOutputMaybe" $ do
            x <- C.mapOutputMaybe (\i -> if even i then Just i else Nothing) (CL.sourceList [1..10 :: Int]) C.$$ CL.fold (+) 0
            x `shouldBe` sum [2, 4..10]
        it' "mapInput" $ do
            xyz <- (CL.sourceList $ map show [1..10 :: Int]) C.$$ do
                (x, y) <- C.mapInput read (Just . show) $ ((do
                    x <- CL.isolate 5 C.=$ CL.fold (+) 0
                    y <- CL.peek
                    return (x :: Int, y :: Maybe Int)) :: C.Sink Int IO (Int, Maybe Int))
                z <- CL.consume
                return (x, y, concat z)

            xyz `shouldBe` (sum [1..5], Just 6, "678910")

    describe "left/right identity" $ do
        it' "left identity" $ do
            x <- CL.sourceList [1..10 :: Int] C.$$ CI.idP C.=$ CL.fold (+) 0
            y <- CL.sourceList [1..10 :: Int] C.$$ CL.fold (+) 0
            x `shouldBe` y
        it' "right identity" $ do
            x <- CI.runPipe $ mapM_ CI.yield [1..10 :: Int] CI.>+> (CL.fold (+) 0) CI.>+> CI.idP
            y <- CI.runPipe $ mapM_ CI.yield [1..10 :: Int] CI.>+> (CL.fold (+) 0)
            x `shouldBe` y

    describe "generalizing" $ do
        it' "works" $ do
            x <-     CI.runPipe
                   $ (CL.sourceList [1..10 :: Int])
               CI.>+> (CL.map (+ 1))
               CI.>+> (CL.fold (+) 0)
            x `shouldBe` sum [2..11]

    describe "iterate" $ do
        it' "works" $ do
            res <- CL.iterate (+ 1) (1 :: Int) C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            res `shouldBe` sum [1..10]

    describe "unwrapResumable" $ do
        it' "works" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
                    C.yieldOr () $ I.writeIORef ref 3
            (rsrc0, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            ('a', x0) `shouldBe` ('a', 0)

            x1 <- I.readIORef ref
            ('b', x1) `shouldBe` ('b', 0)

            rsrc0 C.$$ return ()

            x2 <- I.readIORef ref
            ('c', x2) `shouldBe` ('c', 1)

        it' "isn't called twice" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
            (src1, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            x0 `shouldBe` 0

            x1 <- I.readIORef ref
            x1 `shouldBe` 0

            Just () <- src1 C.$$ CL.head

            x2 <- I.readIORef ref
            x2 `shouldBe` 2

            --final

            x3 <- I.readIORef ref
            x3 `shouldBe` 2

        it' "source isn't used" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
            (src1, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            ('a', x0) `shouldBe` ('a', 0)

            x1 <- I.readIORef ref
            ('b', x1) `shouldBe` ('b', 0)

            () <- src1 C.$$ return ()

            x2 <- I.readIORef ref
            ('c', x2) `shouldBe` ('c', 1)

    describe "setFinalizer" $ do
        it' "works" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    CI.addCleanup (\_ -> I.writeIORef ref 2) $ CI.yield ()
            () <- src0 C.$$ return ()

            x0 <- I.readIORef ref
            ('a', x0) `shouldBe` ('a', 2)

        it' "actions have no effect" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    liftIO $ I.writeIORef ref (1 :: Int)
                    CI.addCleanup (\_ -> do
                        x <- I.readIORef ref
                        ('b', x) `shouldBe` ('b', 1)
                        I.writeIORef ref 2) $ CI.yield ()

            () <- src0 C.$$ return ()

            x0 <- I.readIORef ref
            ('a', x0) `shouldBe` ('a', 2)

-}
    describe "injectLeftovers" $ do
        it "works" $ do
            let src = mapM_ CI.yield [1..10 :: Int]
                conduit = C.awaitForever $ \i -> do
                    js <- CL.take 2
                    mapM_ C.leftover $ reverse js
                    C.yield i
            res <- CI.runPipe $ (CI.fromDown src CI.>+> CI.fromDown conduit) C.>+> CL.consume
            res `shouldBe` [1..10]
    describe "up-upstream finalizers" $ do
        it "pipe" $ do
            let p1 = CI.await >>= maybe (return ()) CI.yield
                p2 = idMsg "p2-final"
                p3 = idMsg "p3-final"
                idMsg msg = CI.addCleanup (const $ tell [msg]) $ CI.awaitForever CI.yield
                printer = CI.awaitForever $ lift . tell . return . show
                src = mapM_ CI.yield [1 :: Int ..]
            let run' p = execWriter $ CI.runPipe $ printer CI.<+< p CI.<+< src
            run' (p1 CI.<+< (p2 CI.<+< p3)) `shouldBe` run' ((p1 CI.<+< p2) CI.<+< p3)
        it "conduit" $ do
            let p1 = C.await >>= maybe (return ()) C.yield
                p2 = idMsg "p2-final"
                p3 = idMsg "p3-final"
                idMsg msg = C.addCleanup (const $ tell [msg]) $ C.awaitForever C.yield
                printer = C.awaitForever $ lift . tell . return . show
                src = CL.sourceList [1 :: Int ..]
            let run' :: C.Conduit Int (Writer [String]) Int
                     -> [String]
                run' p = execWriter $ src C.$$ p C.=$ printer
            run' ((p3 C.=$= p2) C.=$= p1) `shouldBe` run' (p3 C.=$= (p2 C.=$= p1))
    describe "monad transformer laws" $ do
        it "transConduitM" $ do
            let source = CL.sourceList $ replicate 10 ()
            let tell' x = tell [x :: Int]

            let replaceNum1 = C.awaitForever $ \() -> do
                    i <- lift get
                    lift $ (put $ i + 1) >> (get >>= lift . tell')
                    C.yield i

            let replaceNum2 = C.awaitForever $ \() -> do
                    i <- lift get
                    lift $ put $ i + 1
                    lift $ get >>= lift . tell'
                    C.yield i

            x <- runWriterT $ source C.$$ hoist (`evalStateT` 1) replaceNum1 C.=$ CL.consume
            y <- runWriterT $ source C.$$ hoist (`evalStateT` 1) replaceNum2 C.=$ CL.consume
            x `shouldBe` y
    describe "text decode" $ do
        it' "doesn't throw runtime exceptions" $ do
            let x = runIdentity $ runExceptionT $ C.yield "\x89\x243" C.$$ CT.decode CT.utf8 C.=$ CL.consume
            case x of
                Left _ -> return ()
                Right t -> error $ "This should have failed: " ++ show t
    describe "iterM" $ do
        prop "behavior" $ \l -> monadicIO $ do
            let counter ref = CL.iterM (const $ liftIO $ M.modifyMVar_ ref (\i -> return $! i + 1))
            v <- run $ do
                ref <- M.newMVar 0
                CL.sourceList l C.$= counter ref C.$$ CL.mapM_ (const $ return ())
                M.readMVar ref

            assert $ v == length (l :: [Int])
            {-
        prop "mapM_ equivalence" $ \l -> monadicIO $ do
            let runTest h = run $ do
                    ref <- M.newMVar (0 :: Int)
                    let f = action ref
                    s <- CL.sourceList (l :: [Int]) C.$= h f C.$$ CL.fold (+) 0
                    c <- M.readMVar ref

                    return (c, s)

                action ref = const $ liftIO $ M.modifyMVar_ ref (\i -> return $! i + 1)
            (c1, s1) <- runTest CL.iterM
            (c2, s2) <- runTest (\f -> CL.mapM (\a -> f a >>= \() -> return a))

            assert $ c1 == c2
            assert $ s1 == s2
            -}

{-
    describe "generalizing" $ do
        it "works" $ do
            let src :: Int -> C.Source IO Int
                src i = CL.sourceList [1..i]
                sink :: C.Sink Int IO Int
                sink = CL.fold (+) 0
            res <- C.yield 10
              C.$$ C.awaitForever (C.toProducer src)
              C.=$ (C.toConsumer sink >>= C.yield)
              C.=$ C.await
            res `shouldBe` Just (sum [1..10])
            -}

    describe "sinkCacheLength" $ do
        it' "works" $ C.runResourceT $ do
            lbs <- liftIO $ L.readFile "test/main.hs"
            -- FIXME boy this is ugly
            (len, src) <- CI.runPipe $ (CI.fromDown (CB.sourceLbs lbs)) CI.>+> CB.sinkCacheLength
            lbs' <- CI.runPipe $ (CI.fromDown src) CI.>+> CB.sinkLbs
            liftIO $ do
                fromIntegral len `shouldBe` L.length lbs
                lbs' `shouldBe` lbs
                fromIntegral len `shouldBe` L.length lbs'

    describe "mtl instances" $ do
        it "ErrorT" $ do
            let src = flip catchError (const $ C.yield 4) $ do
                    lift $ return ()
                    C.yield 1
                    lift $ return ()
                    C.yield 2
                    lift $ return ()
                    () <- throwError DummyError
                    lift $ return ()
                    C.yield 3
                    lift $ return ()
            (src C.$$ CL.consume) `shouldBe` Right [1, 2, 4 :: Int]

    describe "inject approach test suite" $ do
        let (>->) = CI.pipe

            say :: String -> Writer [String] ()
            say = tell . return

            consume :: Monad m => CI.Pipe i o d t m [i]
            consume = CL.consume
            yield = CI.yield
            leftover = CI.leftover
            idC = CI.idP
            foldM = CL.foldM
            await = CI.await
            runConduit = CI.runPipe

            runConduitI :: Pipe () Void () r Identity r -> r
            runConduitI = runIdentity . runConduit

            runConduitW :: Monoid w => Pipe () Void () r (Writer w) r -> (r, w)
            runConduitW = runWriter . runConduit

            takeExactly :: Monad m => Int -> Pipe i i d t m ()
            takeExactly =
                loop
              where
                loop 0 = return ()
                loop c = do
                    mi <- await
                    case mi of
                        Nothing -> return ()
                        Just i -> do
                            x <- CI.tryYield i
                            case x of
                                Nothing -> loop (c - 1)
                                Just _ -> CL.drop (c - 1)

        describe "basic ops" $ do
            it "consume" $
                runIdentity (mapM_ yield [1..10] C.$$ consume) `shouldBe` [1..10 :: Int]
            it "foldM" $
                runIdentity (mapM_ yield [1..10] C.$$ (foldM (\x y -> return (x + y)) 0)) `shouldBe` (sum [1..10] :: Int)
            it "consume + leftover" $
                runConduitI
                    ((CI.fromDown (mapM_ yield [2..10])) >-> do
                        leftover (1 :: Int)
                        consume) `shouldBe` [1..10]
        describe "identity without leftovers" $ do
            it "front" $
                runConduitI (idC >-> CI.fromDown (mapM_ yield [1..10]) >-> consume) `shouldBe` [1..10 :: Int]
            it "middle" $
                runConduitI ((CI.fromDown (mapM_ yield [1..10])) >-> idC >-> consume) `shouldBe` [1..10 :: Int]
        describe "identity with leftovers" $ do
            it "single" $
                runIdentity (mapM_ yield [2..10] C.$$ do
                    idC >-> leftover (1 :: Int)
                    consume) `shouldBe` [1..10]
            it "multiple, separate blocks" $
                runIdentity (mapM_ yield [3..10] C.$$ do
                    idC `CI.pipe` leftover (2 :: Int)
                    idC >-> leftover (1 :: Int)
                    consume) `shouldBe` [1..10]
            it "multiple, single block" $
                runIdentity (mapM_ yield [3..10] C.$$ do
                    idC `CI.pipe` do
                        leftover (2 :: Int)
                        leftover (1 :: Int)
                    consume) `shouldBe` [1..10]
        describe "cleanup" $ do
            describe "takeExactly" $ do
                it "undrained" $
                    runIdentity (mapM_ yield [1..10 :: Int] C.$$ do
                        takeExactly 5 C.=$ return ()
                        consume) `shouldBe` [6..10]
                it "drained" $
                    runIdentity (mapM_ yield [1..10 :: Int] C.$$ do
                        void $ takeExactly 5 C.=$ consume
                        consume) `shouldBe` [6..10]
        describe "finalizers" $ do
            it "left grouping" $ do
                runConduitW (
                    ((CI.addCleanup (const $ say "first") $ yield () >> lift (say "not called")) C.=$=
                    (CI.addCleanup (const $ say "second") $ yield () >> lift (say "not called"))) C.=$=
                    return ()) `shouldBe` ((), ["second", "first"])
            it "right grouping" $ do
                runConduitW (
                    (CI.addCleanup (const $ say "first") $ yield () >> lift (say "not called")) >->
                    ((CI.addCleanup (const $ say "second") $ yield () >> lift (say "not called")) >->
                    return ())) `shouldBe` ((), ["second", "first"])
            it "promptness" $ do
                imsgs <- I.newIORef []
                let add x = liftIO $ do
                        msgs <- I.readIORef imsgs
                        I.writeIORef imsgs $ msgs ++ [x]
                    src' = C.bracketP
                        (add "acquire")
                        (const $ add "release")
                        (const $ mapM_ (flip CI.yieldOr (add "inside")) [1..5])
                    src = do
                        src' C.$= CL.isolate 4
                        add "computation"
                    sink = CL.mapM (\x -> add (show x) >> return x) C.=$ CL.consume

                res <- C.runResourceT $ runConduit $ src C.$$ sink

                msgs <- I.readIORef imsgs
                msgs `shouldBe` words "acquire 1 2 3 4 inside release computation"

                res `shouldBe` [1..4 :: Int]

    describe "finalizers" $ do
        it "promptness" $ do
            imsgs <- I.newIORef []
            let add x = liftIO $ do
                    msgs <- I.readIORef imsgs
                    I.writeIORef imsgs $ msgs ++ [x]
                src' = C.bracketP
                    (add "acquire")
                    (const $ add "release")
                    (const $ C.addCleanup (const $ add "inside") (mapM_ C.yield [1..5]))
                src = do
                    src' C.$= CL.isolate 4
                    add "computation"
                sink = CL.mapM (\x -> add (show x) >> return x) C.=$ CL.consume

            res <- C.runResourceT $ src C.$$ sink

            msgs <- I.readIORef imsgs
            msgs `shouldBe` words "acquire 1 2 3 4 inside release computation"

            res `shouldBe` [1..4 :: Int]

        it "left associative" $ do
            imsgs <- I.newIORef []
            let add x = liftIO $ do
                    msgs <- I.readIORef imsgs
                    I.writeIORef imsgs $ msgs ++ [x]
                p1 = C.bracketP (add "start1") (const $ add "stop1") (const $ add "inside1" >> C.yield ())
                p2 = C.bracketP (add "start2") (const $ add "stop2") (const $ add "inside2" >> C.await >>= maybe (return ()) C.yield)
                p3 = C.bracketP (add "start3") (const $ add "stop3") (const $ add "inside3" >> C.await)

            res <- C.runResourceT $ (p1 C.$= p2) C.$$ p3
            res `shouldBe` Just ()

            msgs <- I.readIORef imsgs
            msgs `shouldBe` words "start3 inside3 start2 inside2 start1 inside1 stop3 stop2 stop1"

        it "right associative" $ do
            imsgs <- I.newIORef []
            let add x = liftIO $ do
                    msgs <- I.readIORef imsgs
                    I.writeIORef imsgs $ msgs ++ [x]
                p1 = C.bracketP (add "start1") (const $ add "stop1") (const $ add "inside1" >> C.yield ())
                p2 = C.bracketP (add "start2") (const $ add "stop2") (const $ add "inside2" >> C.await >>= maybe (return ()) C.yield)
                p3 = C.bracketP (add "start3") (const $ add "stop3") (const $ add "inside3" >> C.await)

            res <- C.runResourceT $ p1 C.$$ (p2 C.=$ p3)
            res `shouldBe` Just ()

            msgs <- I.readIORef imsgs
            msgs `shouldBe` words "start3 inside3 start2 inside2 start1 inside1 stop3 stop2 stop1"

        it "no double releases" $ do
            let tellLn = tell . (++ "\n")
                finallyP fin = CI.addCleanup (const fin)
                printer = CI.awaitForever $ lift . tellLn . show
                idMsg msg = finallyP (tellLn msg) CI.idP
                takeP 0 = return ()
                takeP n = CI.await >>= \ex -> case ex of
                  Nothing -> return ()
                  Just i -> CI.yield i >> takeP (pred n)

                testPipe p = execWriter $ runPipe $ printer <+< p <+< CI.sourceList ([1..] :: [Int])

                (<+<) = (CI.<+<)
                runPipe = CI.runPipe

                p1 = takeP (2 :: Int)
                p2 = idMsg "foo"
                p3 = idMsg "bar"

                test2L = testPipe $ (p2 <+< p1) <+< p3

            test2L `shouldBe` "1\n2\nfoo\nbar\n"

        describe "dan burton's associative tests" $ do
            let tellLn = tell . (++ "\n")
                finallyP fin = CI.addCleanup (const fin)
                printer = CI.awaitForever $ lift . tellLn . show
                idMsg msg = finallyP (tellLn msg) CI.idP
                takeP 0 = return ()
                takeP n = CI.await >>= \ex -> case ex of
                  Nothing -> return ()
                  Just i -> CI.yield i >> takeP (pred n)

                testPipe p = execWriter $ runPipe $ printer <+< p <+< CI.sourceList ([1..] :: [Int])

                p1 = takeP (1 :: Int)
                p2 = idMsg "foo"
                p3 = idMsg "bar"

                (<+<) = (CI.<+<)
                runPipe = CI.runPipe

                test1L = testPipe $ (p1 <+< p2) <+< p3
                test1R = testPipe $ p1 <+< (p2 <+< p3)

                test2L = testPipe $ (p2 <+< p1) <+< p3
                test2R = testPipe $ p2 <+< (p1 <+< p3)

                test3L = testPipe $ (p2 <+< p3) <+< p1
                test3R = testPipe $ p2 <+< (p3 <+< p1)

                verify testL testR p1' p2' p3'
                  | testL == testR = return () :: IO ()
                  | otherwise = error $ unlines
                    [ "FAILURE"
                    , ""
                    , "(" ++ p1' ++ " <+< " ++ p2' ++ ") <+< " ++ p3'
                    , "------------------"
                    , testL
                    , ""
                    , p1' ++ " <+< (" ++ p2' ++ " <+< " ++ p3' ++ ")"
                    , "------------------"
                    , testR
                    ]

            it "test1" $ verify test1L test1R "p1" "p2" "p3"
            it "test2" $ verify test2L test2R "p2" "p1" "p3"
            it "test3" $ verify test3L test3R "p2" "p3" "p1"

it' :: String -> IO () -> Spec
it' = it

data DummyError = DummyError
    deriving (Show, Eq)
instance Error DummyError
