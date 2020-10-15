{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (getPositive)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Data.Conduit (runConduit, (.|), ConduitT, runConduitPure, runConduitRes)
import qualified Data.Conduit as C
import qualified Data.Conduit.Lift as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import Data.Typeable (Typeable)
import Control.Exception (throw, evaluate)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.State.Strict (modify)
import Data.Maybe   (fromMaybe,catMaybes,fromJust)
import qualified Data.List as DL
import qualified Data.List.Split as DLS (chunksOf)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.IORef as I
import Data.Tuple (swap)
import Control.Monad.Trans.Resource (allocate, resourceForkIO)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (execWriter, tell, runWriterT)
import Control.Monad.Trans.State (evalStateT, get, put)
import qualified Control.Monad.Writer as W
import Control.Applicative (pure, (<$>), (<*>))
import qualified Control.Monad.Catch as Catch
import Data.Functor.Identity (Identity,runIdentity)
import Control.Monad (forever, void)
import Data.Void (Void)
import qualified Control.Concurrent.MVar as M
import Control.Monad.Except (catchError, throwError)
import qualified Data.Map as Map
import qualified Data.Conduit.Extra.ZipConduitSpec as ZipConduit
import qualified Data.Conduit.StreamSpec as Stream
import qualified Spec

(@=?) :: (Eq a, Show a) => a -> a -> IO ()
(@=?) = flip shouldBe

-- Quickcheck property for testing equivalence of list processing
-- functions and their conduit counterparts
equivToList :: Eq b => ([a] -> [b]) -> ConduitT a b Identity () -> [a] -> Bool
equivToList f conduit xs =
  f xs == runConduitPure (CL.sourceList xs .| conduit .| CL.consume)

-- | Check that two conduits produce the same outputs and return the same result.
bisimilarTo :: (Eq a, Eq r) => ConduitT () a Identity r -> ConduitT () a Identity r -> Bool
left `bisimilarTo` right =
    C.runConduitPure (toListRes left) == C.runConduitPure (toListRes right)
  where
    -- | Sink a conduit into a list and return it alongside the result.
    -- So it is, essentially, @sinkList@ plus result.
    toListRes :: Monad m => ConduitT () a m r -> ConduitT () Void m ([a], r)
    toListRes cond = swap <$> C.fuseBoth cond CL.consume


main :: IO ()
main = hspec $ do
    describe "Combinators" Spec.spec
    describe "data loss rules" $ do
        it "consumes the source to quickly" $ do
            x <- runConduitRes $ CL.sourceList [1..10 :: Int] .| do
                  strings <- CL.map show .| CL.take 5
                  liftIO $ putStr $ unlines strings
                  CL.fold (+) 0
            40 `shouldBe` x

        it "correctly consumes a chunked resource" $ do
            x <- runConduitRes $ (CL.sourceList [1..5 :: Int] `mappend` CL.sourceList [6..10]) .| do
                strings <- CL.map show .| CL.take 5
                liftIO $ putStr $ unlines strings
                CL.fold (+) 0
            40 `shouldBe` x

    describe "filter" $ do
        it "even" $ do
            x <- runConduitRes $ CL.sourceList [1..10] .| CL.filter even .| CL.consume
            x `shouldBe` filter even [1..10 :: Int]

    prop "concat" $ equivToList (concat :: [[Int]]->[Int]) CL.concat

    describe "mapFoldable" $ do
        prop "list" $
            equivToList (concatMap (:[]) :: [Int]->[Int]) (CL.mapFoldable  (:[]))
        let f x = if odd x then Just x else Nothing
        prop "Maybe" $
            equivToList (catMaybes . map f :: [Int]->[Int]) (CL.mapFoldable f)

    prop "scan" $ equivToList (tail . scanl (+) 0 :: [Int]->[Int]) (void $ CL.scan (+) 0)

    -- mapFoldableM and scanlM are fully polymorphic in type of monad
    -- so it suffice to check only with Identity.
    describe "mapFoldableM" $ do
        prop "list" $
            equivToList (concatMap (:[]) :: [Int]->[Int]) (CL.mapFoldableM (return . (:[])))
        let f x = if odd x then Just x else Nothing
        prop "Maybe" $
            equivToList (catMaybes . map f :: [Int]->[Int]) (CL.mapFoldableM (return . f))

    prop "scanM" $ equivToList (tail . scanl (+) 0) (void $ CL.scanM (\a s -> return $ a + s) (0 :: Int))

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
            x <- runConduitRes $ CL.sourceList [1..10] .| CL.fold (+) (0 :: Int)
            x `shouldBe` sum [1..10]
        prop "is idempotent" $ \list ->
            (runST $ runConduit $ CL.sourceList list .| CL.fold (+) (0 :: Int))
            == sum list

    describe "foldMap" $ do
        it "sums 1..10" $ do
            Sum x <- runConduit $ CL.sourceList [1..(10 :: Int)] .| CL.foldMap Sum
            x `shouldBe` sum [1..10]

        it "preserves order" $ do
            x <- runConduit $ CL.sourceList [[4],[2],[3],[1]] .| CL.foldMap (++[(9 :: Int)])
            x `shouldBe` [4,9,2,9,3,9,1,9]

    describe "foldMapM" $ do
        it "sums 1..10" $ do
            Sum x <- runConduit $ CL.sourceList [1..(10 :: Int)] .| CL.foldMapM (return . Sum)
            x `shouldBe` sum [1..10]

        it "preserves order" $ do
            x <- runConduit $ CL.sourceList [[4],[2],[3],[1]] .| CL.foldMapM (return . (++[(9 :: Int)]))
            x `shouldBe` [4,9,2,9,3,9,1,9]

    describe "unfold" $ do
        it "works" $ do
            let f 0 = Nothing
                f i = Just (show i, i - 1)
                seed = 10 :: Int
            x <- runConduit $ CL.unfold f seed .| CL.consume
            let y = DL.unfoldr f seed
            x `shouldBe` y

    describe "unfoldM" $ do
        it "works" $ do
            let f 0 = Nothing
                f i = Just (show i, i - 1)
                seed = 10 :: Int
            x <- runConduit $ CL.unfoldM (return . f) seed .| CL.consume
            let y = DL.unfoldr f seed
            x `shouldBe` y

    describe "uncons" $ do
        prop "folds to list" $ \xs ->
          let src = C.sealConduitT $ CL.sourceList xs in
          (xs :: [Int]) == DL.unfoldr CL.uncons src

        prop "works with unfold" $ \xs ->
          let src = CL.sourceList xs in
          CL.unfold CL.uncons (C.sealConduitT src) `bisimilarTo` (src :: ConduitT () Int Identity ())

    describe "unconsEither" $ do
        let
          eitherToMaybe :: Either l a -> Maybe a
          eitherToMaybe (Left _) = Nothing
          eitherToMaybe (Right a) = Just a
        prop "folds outputs to list" $ \xs ->
          let src = C.sealConduitT $ CL.sourceList xs in
          (xs :: [Int]) == DL.unfoldr (eitherToMaybe . CL.unconsEither) src

        prop "works with unfoldEither" $ \(xs, r) ->
          let src = CL.sourceList xs *> pure r in
          CL.unfoldEither CL.unconsEither (C.sealConduitT src) `bisimilarTo` (src :: ConduitT () Int Identity Int)

    describe "Monoid instance for Source" $ do
        it "mappend" $ do
            x <- runConduitRes $ (CL.sourceList [1..5 :: Int] `mappend` CL.sourceList [6..10]) .| CL.fold (+) 0
            x `shouldBe` sum [1..10]
        it "mconcat" $ do
            x <- runConduitRes $ mconcat
                [ CL.sourceList [1..5 :: Int]
                , CL.sourceList [6..10]
                , CL.sourceList [11..20]
                ] .| CL.fold (+) 0
            x `shouldBe` sum [1..20]

    describe "zipping" $ do
        it "zipping two small lists" $ do
            res <- runConduitRes $ CI.zipSources (CL.sourceList [1..10]) (CL.sourceList [11..12]) .| CL.consume
            res @=? zip [1..10 :: Int] [11..12 :: Int]

    describe "zipping sinks" $ do
        it "take all" $ do
            res <- runConduitRes $ CL.sourceList [1..10] .| CI.zipSinks CL.consume CL.consume
            res @=? ([1..10 :: Int], [1..10 :: Int])
        it "take fewer on left" $ do
            res <- runConduitRes $ CL.sourceList [1..10] .| CI.zipSinks (CL.take 4) CL.consume
            res @=? ([1..4 :: Int], [1..10 :: Int])
        it "take fewer on right" $ do
            res <- runConduitRes $ CL.sourceList [1..10] .| CI.zipSinks CL.consume (CL.take 4)
            res @=? ([1..10 :: Int], [1..4 :: Int])

    describe "Monad instance for Sink" $ do
        it "binding" $ do
            x <- runConduitRes $ CL.sourceList [1..10] .| do
                _ <- CL.take 5
                CL.fold (+) (0 :: Int)
            x `shouldBe` sum [6..10]

    describe "Applicative instance for Sink" $ do
        it "<$> and <*>" $ do
            x <- runConduitRes $ CL.sourceList [1..10] .|
                (+) <$> pure 5 <*> CL.fold (+) (0 :: Int)
            x `shouldBe` sum [1..10] + 5

    describe "resumable sources" $ do
        it "simple" $ do
            (x, y, z) <- runConduitRes $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 C.$$+ CL.take 5
                (src3, y) <- src2 C.$$++ CL.fold (+) 0
                z <- src3 C.$$+- CL.consume
                return (x, y, z)
            x `shouldBe` [1..5] :: IO ()
            y `shouldBe` sum [6..10]
            z `shouldBe` []

    describe "conduits" $ do
        it "map, left" $ do
            x <- runConduitRes $
                CL.sourceList [1..10]
                    .| CL.map (* 2)
                    .| CL.fold (+) 0
            x `shouldBe` 2 * sum [1..10 :: Int]

        it "map, left >+>" $ do
            x <- runConduitRes $
                CI.ConduitT
                    ((CI.unConduitT (CL.sourceList [1..10]) CI.Done
                    CI.>+> CI.injectLeftovers (flip CI.unConduitT CI.Done $ CL.map (* 2))) >>=)
                    .| CL.fold (+) 0
            x `shouldBe` 2 * sum [1..10 :: Int]

        it "map, right" $ do
            x <- runConduitRes $
                CL.sourceList [1..10]
                    .| CL.map (* 2)
                    .| CL.fold (+) 0
            x `shouldBe` 2 * sum [1..10 :: Int]

        prop "chunksOf" $ \(positive, xs) ->
            let p = getPositive positive
                conduit = CL.sourceList xs .| CL.chunksOf p .| CL.consume
            in DLS.chunksOf p (xs :: [Int]) == runConduitPure conduit

        it "chunksOf (zero)" $
            let conduit = return () .| CL.chunksOf 0 .| CL.consume
            in evaluate (runConduitPure conduit) `shouldThrow` anyException

        it "chunksOf (negative)" $
            let conduit = return () .| CL.chunksOf (-5) .| CL.consume
            in evaluate (runConduitPure conduit) `shouldThrow` anyException

        it "groupBy" $ do
            let input = [1::Int, 1, 2, 3, 3, 3, 4, 5, 5]
            x <- runConduitRes $ CL.sourceList input
                    .| CL.groupBy (==)
                    .| CL.consume
            x `shouldBe` DL.groupBy (==) input

        it "groupBy (nondup begin/end)" $ do
            let input = [1::Int, 2, 3, 3, 3, 4, 5]
            x <- runConduitRes $ CL.sourceList input
                    .| CL.groupBy (==)
                    .| CL.consume
            x `shouldBe` DL.groupBy (==) input

        it "groupOn1" $ do
            let input = [1::Int, 1, 2, 3, 3, 3, 4, 5, 5]
            x <- runConduitRes $ CL.sourceList input
                    .| CL.groupOn1 id
                    .| CL.consume
            x `shouldBe` [(1,[1]), (2, []), (3,[3,3]), (4,[]), (5, [5])]

        it "groupOn1 (nondup begin/end)" $ do
            let input = [1::Int, 2, 3, 3, 3, 4, 5]
            x <- runConduitRes $ CL.sourceList input
                    .| CL.groupOn1 id
                    .| CL.consume
            x `shouldBe` [(1,[]), (2, []), (3,[3,3]), (4,[]), (5, [])]


        it "mapMaybe" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runConduitRes $ CL.sourceList input
                    .| CL.mapMaybe ((+2) <$>)
                    .| CL.consume
            x `shouldBe` [3, 4, 5]

        it "mapMaybeM" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runConduitRes $ CL.sourceList input
                    .| CL.mapMaybeM (return . ((+2) <$>))
                    .| CL.consume
            x `shouldBe` [3, 4, 5]

        it "catMaybes" $ do
            let input = [Just (1::Int), Nothing, Just 2, Nothing, Just 3]
            x <- runConduitRes $ CL.sourceList input
                    .| CL.catMaybes
                    .| CL.consume
            x `shouldBe` [1, 2, 3]

        it "concatMap" $ do
            let input = [1, 11, 21]
            x <- runConduitRes $ CL.sourceList input
                    .| CL.concatMap (\i -> enumFromTo i (i + 9))
                    .| CL.fold (+) (0 :: Int)
            x `shouldBe` sum [1..30]

        it "bind together" $ do
            let conduit = CL.map (+ 5) .| CL.map (* 2)
            x <- runConduitRes $ CL.sourceList [1..10] .| conduit .| CL.fold (+) 0
            x `shouldBe` sum (map (* 2) $ map (+ 5) [1..10 :: Int])

#if !FAST
    describe "isolate" $ do
        it "bound to resumable source" $ do
            (x, y) <- runConduitRes $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 .| CL.isolate 5 C.$$+ CL.consume
                y <- src2 C.$$+- CL.consume
                return (x, y)
            x `shouldBe` [1..5]
            y `shouldBe` []

        it "bound to sink, non-resumable" $ do
            (x, y) <- runConduitRes $ do
                CL.sourceList [1..10 :: Int] .| do
                    x <- CL.isolate 5 .| CL.consume
                    y <- CL.consume
                    return (x, y)
            x `shouldBe` [1..5]
            y `shouldBe` [6..10]

        it "bound to sink, resumable" $ do
            (x, y) <- runConduitRes $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, x) <- src1 C.$$+ CL.isolate 5 .| CL.consume
                y <- src2 C.$$+- CL.consume
                return (x, y)
            x `shouldBe` [1..5]
            y `shouldBe` [6..10]

        it "consumes all data" $ do
            x <- runConduitRes $ CL.sourceList [1..10 :: Int] .| do
                CL.isolate 5 .| CL.sinkNull
                CL.consume
            x `shouldBe` [6..10]

    describe "sequence" $ do
        it "simple sink" $ do
            let sumSink = do
                    ma <- CL.head
                    case ma of
                        Nothing -> return 0
                        Just a  -> (+a) . fromMaybe 0 <$> CL.head

            res <- runConduitRes $ CL.sourceList [1..11 :: Int]
                             .| CL.sequence sumSink
                             .| CL.consume
            res `shouldBe` [3, 7, 11, 15, 19, 11]

        it "sink with unpull behaviour" $ do
            let sumSink = do
                    ma <- CL.head
                    case ma of
                        Nothing -> return 0
                        Just a  -> (+a) . fromMaybe 0 <$> CL.peek

            res <- runConduitRes $ CL.sourceList [1..11 :: Int]
                             .| CL.sequence sumSink
                             .| CL.consume
            res `shouldBe` [3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 11]

#endif

    describe "peek" $ do
        it "works" $ do
            (a, b) <- runConduitRes $ CL.sourceList [1..10 :: Int] .| do
                a <- CL.peek
                b <- CL.consume
                return (a, b)
            (a, b) `shouldBe` (Just 1, [1..10])

    describe "unbuffering" $ do
        it "works" $ do
            x <- runConduitRes $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, ()) <- src1 C.$$+ CL.drop 5
                src2 C.$$+- CL.fold (+) 0
            x `shouldBe` sum [6..10]

    describe "operators" $ do
        it "only use .|" $
            runConduitPure
            (    CL.sourceList [1..10 :: Int]
              .| CL.map (+ 1)
             .|  CL.map (subtract 1)
             .|  CL.mapM (return . (* 2))
             .|  CL.map (`div` 2)
             .|  CL.fold (+) 0
            ) `shouldBe` sum [1..10]
        it "only use =$" $
            runConduitPure
            (    CL.sourceList [1..10 :: Int]
              .| CL.map (+ 1)
              .| CL.map (subtract 1)
              .| CL.map (* 2)
              .| CL.map (`div` 2)
              .| CL.fold (+) 0
            ) `shouldBe` sum [1..10]
        it "chain" $ do
            x <-    runConduit
                 $ CL.sourceList [1..10 :: Int]
                .| CL.map (+ 1)
                .| CL.map (+ 1)
                .| CL.map (+ 1)
                .| CL.map (subtract 3)
                .| CL.map (* 2)
                .| CL.map (`div` 2)
                .| CL.map (+ 1)
                .| CL.map (+ 1)
                .| CL.map (+ 1)
                .| CL.map (subtract 3)
                .| CL.fold (+) 0
            x `shouldBe` sum [1..10]


    describe "termination" $ do
        it "terminates early" $ do
            let src = forever $ C.yield ()
            x <- runConduit $ src .| CL.head
            x `shouldBe` Just ()
        it "bracket" $ do
            ref <- I.newIORef (0 :: Int)
            let src = C.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ C.yield (1 :: Int))
            val <- runConduitRes $ src .| CL.isolate 10 .| CL.fold (+) 0
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
            val <- runConduitRes $ (src' >> src) .| CL.isolate 10 .| CL.fold (+) 0
            val `shouldBe` 10
            i <- I.readIORef ref
            i `shouldBe` 0
        it "bracket + toPipe" $ do
            ref <- I.newIORef (0 :: Int)
            let src = C.bracketP
                    (I.modifyIORef ref (+ 1))
                    (\() -> I.modifyIORef ref (+ 2))
                    (\() -> forever $ C.yield (1 :: Int))
            val <- runConduitRes $ src .| CL.isolate 10 .| CL.fold (+) 0
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
            val <- runConduitRes $ (src' >> src) .| CL.isolate 10 .| CL.fold (+) 0
            val `shouldBe` 10
            i <- I.readIORef ref
            i `shouldBe` 0

    describe "invariant violations" $ do
        it "leftovers without input" $ do
            ref <- I.newIORef []
            let add x = I.modifyIORef ref (x:)
                adder' = CI.NeedInput (\a -> liftIO (add a) >> adder') return
                adder = CI.ConduitT (adder' >>=)
                residue x = CI.ConduitT $ \rest -> CI.Leftover (rest ()) x

            _ <- runConduit $ C.yield 1 .| adder
            x <- I.readIORef ref
            x `shouldBe` [1 :: Int]
            I.writeIORef ref []

            _ <- runConduit $ C.yield 1 .| ((residue 2 >> residue 3) >> adder)
            y <- I.readIORef ref
            y `shouldBe` [1, 2, 3]
            I.writeIORef ref []

            _ <- runConduit $ C.yield 1 .| (residue 2 >> (residue 3 >> adder))
            z <- I.readIORef ref
            z `shouldBe` [1, 2, 3]
            I.writeIORef ref []

    describe "sane yield/await'" $ do
        it' "yield terminates" $ do
            let is = [1..10] ++ undefined
                src [] = return ()
                src (x:xs) = C.yield x >> src xs
            x <- runConduit $ src is .| CL.take 10
            x `shouldBe` [1..10 :: Int]
        it' "yield terminates (2)" $ do
            let is = [1..10] ++ undefined
            x <- runConduit $ mapM_ C.yield is .| CL.take 10
            x `shouldBe` [1..10 :: Int]

    describe "upstream results" $ do
        it' "works" $ do
            let foldUp :: (b -> a -> b) -> b -> CI.Pipe l a Void u IO (u, b)
                foldUp f b = CI.awaitE >>= either (\u -> return (u, b)) (\a -> let b' = f b a in b' `seq` foldUp f b')
                passFold :: (b -> a -> b) -> b -> CI.Pipe l a a () IO b
                passFold f b = CI.await >>= maybe (return b) (\a -> let b' = f b a in b' `seq` CI.yield a >> passFold f b')
            (x, y) <- CI.runPipe $ mapM_ CI.yield [1..10 :: Int] CI.>+> passFold (+) 0 CI.>+>  foldUp (*) 1
            (x, y) `shouldBe` (sum [1..10], product [1..10])

    describe "input/output mapping" $ do
        it' "mapOutput" $ do
            x <- runConduit $ C.mapOutput (+ 1) (CL.sourceList [1..10 :: Int]) .| CL.fold (+) 0
            x `shouldBe` sum [2..11]
        it' "mapOutputMaybe" $ do
            x <- runConduit $ C.mapOutputMaybe (\i -> if even i then Just i else Nothing) (CL.sourceList [1..10 :: Int]) .| CL.fold (+) 0
            x `shouldBe` sum [2, 4..10]
        it' "mapInput" $ do
            xyz <- runConduit $ (CL.sourceList $ map show [1..10 :: Int]) .| do
                (x, y) <- C.mapInput read (Just . show) $ ((do
                    x <- CL.isolate 5 .| CL.fold (+) 0
                    y <- CL.peek
                    return (x :: Int, y :: Maybe Int)) :: ConduitT Int Void IO (Int, Maybe Int))
                z <- CL.consume
                return (x, y, concat z)

            xyz `shouldBe` (sum [1..5], Just 6, "678910")

    describe "left/right identity" $ do
        it' "left identity" $ do
            x <- runConduit $ CL.sourceList [1..10 :: Int] .| CI.ConduitT (CI.idP >>=) .| CL.fold (+) 0
            y <- runConduit $ CL.sourceList [1..10 :: Int] .| CL.fold (+) 0
            x `shouldBe` y
        it' "right identity" $ do
            x <- CI.runPipe $ mapM_ CI.yield [1..10 :: Int] CI.>+> (CI.injectLeftovers $ flip CI.unConduitT CI.Done $ CL.fold (+) 0) CI.>+> CI.idP
            y <- CI.runPipe $ mapM_ CI.yield [1..10 :: Int] CI.>+> (CI.injectLeftovers $ flip CI.unConduitT CI.Done $ CL.fold (+) 0)
            x `shouldBe` y

    describe "generalizing" $ do
        it' "works" $ do
            x <-     CI.runPipe
                   $ CI.sourceToPipe  (CL.sourceList [1..10 :: Int])
               CI.>+> CI.conduitToPipe (CL.map (+ 1))
               CI.>+> CI.sinkToPipe    (CL.fold (+) 0)
            x `shouldBe` sum [2..11]

    describe "withUpstream" $ do
        it' "works" $ do
            let src = mapM_ CI.yield [1..10 :: Int] >> return True
                fold f =
                    loop
                  where
                    loop accum =
                        CI.await >>= maybe (return accum) go
                      where
                        go a =
                            let accum' = f accum a
                             in accum' `seq` loop accum'
                sink = CI.withUpstream $ fold (+) 0
            res <- CI.runPipe $ src CI.>+> sink
            res `shouldBe` (True, sum [1..10])

    describe "iterate" $ do
        it' "works" $ do
            res <- runConduit $ CL.iterate (+ 1) (1 :: Int) .| CL.isolate 10 .| CL.fold (+) 0
            res `shouldBe` sum [1..10]

    prop "replicate" $ \cnt' -> do
        let cnt = min cnt' 100
        res <- runConduit $ CL.replicate cnt () .| CL.consume
        res `shouldBe` replicate cnt ()

    prop "replicateM" $ \cnt' -> do
        ref <- I.newIORef 0
        let cnt = min cnt' 100
        res <- runConduit $ CL.replicateM cnt (I.modifyIORef ref (+ 1)) .| CL.consume
        res `shouldBe` replicate cnt ()

        ref' <- I.readIORef ref
        ref' `shouldBe` (if cnt' <= 0 then 0 else cnt)

    describe "injectLeftovers" $ do
        it "works" $ do
            let src = mapM_ CI.yield [1..10 :: Int]
                conduit = CI.injectLeftovers $ flip CI.unConduitT CI.Done $ C.awaitForever $ \i -> do
                    js <- CL.take 2
                    mapM_ C.leftover $ reverse js
                    C.yield i
            res <- runConduit $ CI.ConduitT ((src CI.>+> CI.injectLeftovers conduit) >>=) .| CL.consume
            res `shouldBe` [1..10]
    describe "monad transformer laws" $ do
        it "transPipe" $ do
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

            x <- runWriterT $ runConduit $ source .| C.transPipe (`evalStateT` 1) replaceNum1 .| CL.consume
            y <- runWriterT $ runConduit $ source .| C.transPipe (`evalStateT` 1) replaceNum2 .| CL.consume
            x `shouldBe` y
    describe "iterM" $ do
        prop "behavior" $ \l -> monadicIO $ do
            let counter ref = CL.iterM (const $ liftIO $ M.modifyMVar_ ref (\i -> return $! i + 1))
            v <- run $ do
                ref <- M.newMVar 0
                runConduit $ CL.sourceList l .| counter ref .| CL.mapM_ (const $ return ())
                M.readMVar ref

            assert $ v == length (l :: [Int])
        prop "mapM_ equivalence" $ \l -> monadicIO $ do
            let runTest h = run $ do
                    ref <- M.newMVar (0 :: Int)
                    let f = action ref
                    s <- runConduit $ CL.sourceList (l :: [Int]) .| h f .| CL.fold (+) 0
                    c <- M.readMVar ref

                    return (c, s)

                action ref = const $ liftIO $ M.modifyMVar_ ref (\i -> return $! i + 1)
            (c1, s1) <- runTest CL.iterM
            (c2, s2) <- runTest (\f -> CL.mapM (\a -> f a >>= \() -> return a))

            assert $ c1 == c2
            assert $ s1 == s2

    describe "generalizing" $ do
        it "works" $ do
            let src :: Int -> ConduitT () Int IO ()
                src i = CL.sourceList [1..i]
                sink :: ConduitT Int Void IO Int
                sink = CL.fold (+) 0
            res <- runConduit $ C.yield 10 .| C.awaitForever (C.toProducer . src) .| (C.toConsumer sink >>= C.yield) .| C.await
            res `shouldBe` Just (sum [1..10])

    describe "mergeSource" $ do
        it "works" $ do
            let src :: ConduitT () String IO ()
                src = CL.sourceList ["A", "B", "C"]
                withIndex :: ConduitT String (Integer, String) IO ()
                withIndex = CI.mergeSource (CL.sourceList [1..])
            output <- runConduit $ src .| withIndex .| CL.consume
            output `shouldBe` [(1, "A"), (2, "B"), (3, "C")]
        it "does stop processing when the source exhausted" $ do
            let src :: ConduitT () Integer IO ()
                src = CL.sourceList [1..]
                withShortAlphaIndex :: ConduitT Integer (String, Integer) IO ()
                withShortAlphaIndex = CI.mergeSource (CL.sourceList ["A", "B", "C"])
            output <- runConduit $ src .| withShortAlphaIndex .| CL.consume
            output `shouldBe` [("A", 1), ("B", 2), ("C", 3)]

    describe "passthroughSink" $ do
        it "works" $ do
            ref <- I.newIORef (-1)
            let sink = CL.fold (+) (0 :: Int)
                conduit = C.passthroughSink sink (I.writeIORef ref)
                input = [1..10]
            output <- runConduit $ mapM_ C.yield input .| conduit .| CL.consume
            output `shouldBe` input
            x <- I.readIORef ref
            x `shouldBe` sum input
        it "does nothing when downstream does nothing" $ do
            ref <- I.newIORef (-1)
            let sink = CL.fold (+) (0 :: Int)
                conduit = C.passthroughSink sink (I.writeIORef ref)
                input = [undefined]
            runConduit $ mapM_ C.yield input .| conduit .| return ()
            x <- I.readIORef ref
            x `shouldBe` (-1)

        it "handles the last input correctly #304" $ do
            ref <- I.newIORef (-1 :: Int)
            let sink = CL.mapM_ (I.writeIORef ref)
                conduit = C.passthroughSink sink (const (return ()))
            res <- runConduit $ mapM_ C.yield [1..] .| conduit .| CL.take 5
            res `shouldBe` [1..5]
            x <- I.readIORef ref
            x `shouldBe` 5

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
            runConduit (src .| CL.consume) `shouldBe` Right [1, 2, 4 :: Int]
        describe "WriterT" $
            it "pass" $
                let writer = W.pass $ do
                      W.tell [1 :: Int]
                      pure ((), (2:))
                in execWriter (runConduit writer) `shouldBe` [2, 1]

    describe "Data.Conduit.Lift" $ do
        it "execStateC" $ do
            let sink = C.execStateLC 0 $ CL.mapM_ $ modify . (+)
                src = mapM_ C.yield [1..10 :: Int]
            res <- runConduit $ src .| sink
            res `shouldBe` sum [1..10]

        it "execWriterC" $ do
            let sink = C.execWriterLC $ CL.mapM_ $ tell . return
                src = mapM_ C.yield [1..10 :: Int]
            res <- runConduit $ src .| sink
            res `shouldBe` [1..10]

        it "runExceptC" $ do
            let sink = C.runExceptC $ do
                    x <- C.catchExceptC (lift $ throwError "foo") return
                    return $ x ++ "bar"
            res <- runConduit $ return () .| sink
            res `shouldBe` Right ("foobar" :: String)

        it "runMaybeC" $ do
            let src = void $ C.runMaybeC $ do
                    C.yield 1
                    () <- lift $ MaybeT $ return Nothing
                    C.yield 2
                sink = CL.consume
            res <- runConduit $ src .| sink
            res `shouldBe` [1 :: Int]

    describe "sequenceSources" $ do
        it "works" $ do
            let src1 = mapM_ C.yield [1, 2, 3 :: Int]
                src2 = mapM_ C.yield [3, 2, 1]
                src3 = mapM_ C.yield $ repeat 2
                srcs = C.sequenceSources $ Map.fromList
                    [ (1 :: Int, src1)
                    , (2, src2)
                    , (3, src3)
                    ]
            res <- runConduit $ srcs .| CL.consume
            res `shouldBe`
                [ Map.fromList [(1, 1), (2, 3), (3, 2)]
                , Map.fromList [(1, 2), (2, 2), (3, 2)]
                , Map.fromList [(1, 3), (2, 1), (3, 2)]
                ]
    describe "zipSink" $ do
        it "zip equal-sized" $ do
            x <- runConduitRes $
                    CL.sourceList [1..100] .|
                    C.sequenceSinks [ CL.fold (+) 0,
                                   (`mod` 101) <$> CL.fold (*) 1 ]
            x `shouldBe` [5050, 100 :: Integer]

        it "zip distinct sizes" $ do
            let sink = C.getZipSink $
                        (*) <$> C.ZipSink (CL.fold (+) 0)
                            <*> C.ZipSink (Data.Maybe.fromJust <$> C.await)
            x <- runConduitRes $ CL.sourceList [100,99..1] .| sink
            x `shouldBe` (505000 :: Integer)

    describe "upstream results" $ do
        it "fuseBoth" $ do
            let upstream = do
                    C.yield ("hello" :: String)
                    CL.isolate 5 .| CL.fold (+) 0
                downstream = C.fuseBoth upstream CL.consume
            res <- runConduit $ CL.sourceList [1..10 :: Int] .| do
                (x, y) <- downstream
                z <- CL.consume
                return (x, y, z)
            res `shouldBe` (sum [1..5], ["hello"], [6..10])

        it "fuseBothMaybe with no result" $ do
            let src = mapM_ C.yield [1 :: Int ..]
                sink = CL.isolate 5 .| CL.fold (+) 0
            (mup, down) <- runConduit $ C.fuseBothMaybe src sink
            mup `shouldBe` (Nothing :: Maybe ())
            down `shouldBe` sum [1..5]

        it "fuseBothMaybe with result" $ do
            let src = mapM_ C.yield [1 :: Int .. 5]
                sink = CL.isolate 6 .| CL.fold (+) 0
            (mup, down) <- runConduit $ C.fuseBothMaybe src sink
            mup `shouldBe` Just ()
            down `shouldBe` sum [1..5]

        it "fuseBothMaybe with almost result" $ do
            let src = mapM_ C.yield [1 :: Int .. 5]
                sink = CL.isolate 5 .| CL.fold (+) 0
            (mup, down) <- runConduit $ C.fuseBothMaybe src sink
            mup `shouldBe` (Nothing :: Maybe ())
            down `shouldBe` sum [1..5]

    describe "catching exceptions" $ do
        it "works" $ do
            let src = do
                    C.yield 1
                    () <- Catch.throwM DummyError
                    C.yield 2
                src' = do
                    CI.catchC src (\DummyError -> C.yield (3 :: Int))
            res <- runConduit $ src' .| CL.consume
            res `shouldBe` [1, 3]

    describe "sourceToList" $ do
        it "works lazily in Identity" $ do
            let src = C.yield 1 >> C.yield 2 >> throw DummyError
            let res = runIdentity $ C.sourceToList src
            take 2 res `shouldBe` [1, 2 :: Int]
        it "is not lazy in IO" $ do
            let src = C.yield 1 >> C.yield (2 :: Int) >> throw DummyError
            C.sourceToList src `shouldThrow` (==DummyError)

    ZipConduit.spec
    Stream.spec

it' :: String -> IO () -> Spec
it' = it

data DummyError = DummyError
    deriving (Show, Eq, Typeable)
instance Catch.Exception DummyError
