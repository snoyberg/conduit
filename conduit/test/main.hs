{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Data.Conduit as C
import qualified Data.Conduit.Lift as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import Data.Typeable (Typeable)
import Control.Exception (throw)
import Control.Monad.Trans.Resource as C (runResourceT)
import Data.Maybe   (fromMaybe,catMaybes,fromJust)
import qualified Data.List as DL
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.IORef as I
import Control.Monad.Trans.Resource (allocate, resourceForkIO)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (execWriter, tell, runWriterT)
import Control.Monad.Trans.State (evalStateT, get, put, modify)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Applicative (pure, (<$>), (<*>))
import qualified Control.Monad.Catch as Catch
import Data.Functor.Identity (Identity,runIdentity)
import Control.Monad (forever, void)
import Data.Void (Void)
import qualified Control.Concurrent.MVar as M
import Control.Monad.Error (catchError, throwError, Error)
import qualified Data.Map as Map
import qualified Data.Conduit.Extra.ZipConduitSpec as ZipConduit
import qualified Data.Conduit.StreamSpec as Stream

(@=?) :: (Eq a, Show a) => a -> a -> IO ()
(@=?) = flip shouldBe

-- Quickcheck property for testing equivalence of list processing
-- functions and their conduit counterparts
equivToList :: Eq b => ([a] -> [b]) -> CI.Conduit a Identity b -> [a] -> Bool
equivToList f conduit xs =
  f xs == runIdentity (CL.sourceList xs C.$$ conduit C.=$= CL.consume)


main :: IO ()
main = hspec $ do
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

    describe "unfoldM" $ do
        it "works" $ do
            let f 0 = Nothing
                f i = Just (show i, i - 1)
                seed = 10 :: Int
            x <- CL.unfoldM (return . f) seed C.$$ CL.consume
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

    describe "zipping" $ do
        it "zipping two small lists" $ do
            res <- runResourceT $ CI.zipSources (CL.sourceList [1..10]) (CL.sourceList [11..12]) C.$$ CL.consume
            res @=? zip [1..10 :: Int] [11..12 :: Int]

    describe "zipping sinks" $ do
        it "take all" $ do
            res <- runResourceT $ CL.sourceList [1..10] C.$$ CI.zipSinks CL.consume CL.consume
            res @=? ([1..10 :: Int], [1..10 :: Int])
        it "take fewer on left" $ do
            res <- runResourceT $ CL.sourceList [1..10] C.$$ CI.zipSinks (CL.take 4) CL.consume
            res @=? ([1..4 :: Int], [1..10 :: Int])
        it "take fewer on right" $ do
            res <- runResourceT $ CL.sourceList [1..10] C.$$ CI.zipSinks CL.consume (CL.take 4)
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
                (src3, y) <- src2 C.$$++ CL.fold (+) 0
                z <- src3 C.$$+- CL.consume
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
                CI.ConduitM
                    ((CI.unConduitM (CL.sourceList [1..10]) CI.Done
                    CI.>+> CI.injectLeftovers (flip CI.unConduitM CI.Done $ CL.map (* 2))) >>=)
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

        it "groupOn1" $ do
            let input = [1::Int, 1, 2, 3, 3, 3, 4, 5, 5]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.groupOn1 id
                    C.=$ CL.consume
            x `shouldBe` [(1,[1]), (2, []), (3,[3,3]), (4,[]), (5, [5])]

        it "groupOn1 (nondup begin/end)" $ do
            let input = [1::Int, 2, 3, 3, 3, 4, 5]
            x <- runResourceT $ CL.sourceList input
                    C.$$ CL.groupOn1 id
                    C.=$ CL.consume
            x `shouldBe` [(1,[]), (2, []), (3,[3,3]), (4,[]), (5, [])]


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
                y <- src2 C.$$+- CL.consume
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
                y <- src2 C.$$+- CL.consume
                return (x, y)
            x `shouldBe` [1..5]
            y `shouldBe` [6..10]

        it "consumes all data" $ do
            x <- runResourceT $ CL.sourceList [1..10 :: Int] C.$$ do
                CL.isolate 5 C.=$ CL.sinkNull
                CL.consume
            x `shouldBe` [6..10]

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

    describe "unbuffering" $ do
        it "works" $ do
            x <- runResourceT $ do
                let src1 = CL.sourceList [1..10 :: Int]
                (src2, ()) <- src1 C.$$+ CL.drop 5
                src2 C.$$+- CL.fold (+) 0
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
                adder' = CI.NeedInput (\a -> liftIO (add a) >> adder') return
                adder = CI.ConduitM (adder' >>=)
                residue x = CI.ConduitM $ \rest -> CI.Leftover (rest ()) x

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
            x <- CL.sourceList [1..10 :: Int] C.$$ CI.ConduitM (CI.idP >>=) C.=$ CL.fold (+) 0
            y <- CL.sourceList [1..10 :: Int] C.$$ CL.fold (+) 0
            x `shouldBe` y
        it' "right identity" $ do
            x <- CI.runPipe $ mapM_ CI.yield [1..10 :: Int] CI.>+> (CI.injectLeftovers $ flip CI.unConduitM CI.Done $ CL.fold (+) 0) CI.>+> CI.idP
            y <- CI.runPipe $ mapM_ CI.yield [1..10 :: Int] CI.>+> (CI.injectLeftovers $ flip CI.unConduitM CI.Done $ CL.fold (+) 0)
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
            res <- CL.iterate (+ 1) (1 :: Int) C.$$ CL.isolate 10 C.=$ CL.fold (+) 0
            res `shouldBe` sum [1..10]

    prop "replicate" $ \cnt' -> do
        let cnt = min cnt' 100
        res <- CL.replicate cnt () C.$$ CL.consume
        res `shouldBe` replicate cnt ()

    prop "replicateM" $ \cnt' -> do
        ref <- I.newIORef 0
        let cnt = min cnt' 100
        res <- CL.replicateM cnt (I.modifyIORef ref (+ 1)) C.$$ CL.consume
        res `shouldBe` replicate cnt ()

        ref' <- I.readIORef ref
        ref' `shouldBe` (if cnt' <= 0 then 0 else cnt)

    describe "unwrapResumable" $ do
        it' "works" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
                    C.yieldOr () $ I.writeIORef ref 3
            (rsrc0, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            x0 `shouldBe` 0

            (_, final) <- C.unwrapResumable rsrc0

            x1 <- I.readIORef ref
            x1 `shouldBe` 0

            final

            x2 <- I.readIORef ref
            x2 `shouldBe` 1

        it' "isn't called twice" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
            (rsrc0, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            x0 `shouldBe` 0

            (src1, final) <- C.unwrapResumable rsrc0

            x1 <- I.readIORef ref
            x1 `shouldBe` 0

            Just () <- src1 C.$$ CL.head

            x2 <- I.readIORef ref
            x2 `shouldBe` 2

            final

            x3 <- I.readIORef ref
            x3 `shouldBe` 2

        it' "source isn't used" $ do
            ref <- I.newIORef (0 :: Int)
            let src0 = do
                    C.yieldOr () $ I.writeIORef ref 1
                    C.yieldOr () $ I.writeIORef ref 2
            (rsrc0, Just ()) <- src0 C.$$+ CL.head

            x0 <- I.readIORef ref
            x0 `shouldBe` 0

            (src1, final) <- C.unwrapResumable rsrc0

            x1 <- I.readIORef ref
            x1 `shouldBe` 0

            () <- src1 C.$$ return ()

            x2 <- I.readIORef ref
            x2 `shouldBe` 0

            final

            x3 <- I.readIORef ref
            x3 `shouldBe` 1
    describe "injectLeftovers" $ do
        it "works" $ do
            let src = mapM_ CI.yield [1..10 :: Int]
                conduit = CI.injectLeftovers $ flip CI.unConduitM CI.Done $ C.awaitForever $ \i -> do
                    js <- CL.take 2
                    mapM_ C.leftover $ reverse js
                    C.yield i
            res <- CI.ConduitM ((src CI.>+> CI.injectLeftovers conduit) >>=) C.$$ CL.consume
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
            let run' p = execWriter $ src C.$$ p C.=$ printer
            run' ((p3 C.=$= p2) C.=$= p1) `shouldBe` run' (p3 C.=$= (p2 C.=$= p1))
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

            x <- runWriterT $ source C.$$ C.transPipe (`evalStateT` 1) replaceNum1 C.=$ CL.consume
            y <- runWriterT $ source C.$$ C.transPipe (`evalStateT` 1) replaceNum2 C.=$ CL.consume
            x `shouldBe` y
    describe "iterM" $ do
        prop "behavior" $ \l -> monadicIO $ do
            let counter ref = CL.iterM (const $ liftIO $ M.modifyMVar_ ref (\i -> return $! i + 1))
            v <- run $ do
                ref <- M.newMVar 0
                CL.sourceList l C.$= counter ref C.$$ CL.mapM_ (const $ return ())
                M.readMVar ref

            assert $ v == length (l :: [Int])
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

    describe "generalizing" $ do
        it "works" $ do
            let src :: Int -> C.Source IO Int
                src i = CL.sourceList [1..i]
                sink :: C.Sink Int IO Int
                sink = CL.fold (+) 0
            res <- C.yield 10 C.$$ C.awaitForever (C.toProducer . src) C.=$ (C.toConsumer sink >>= C.yield) C.=$ C.await
            res `shouldBe` Just (sum [1..10])

    describe "mergeSource" $ do
        it "works" $ do
            let src :: C.Source IO String
                src = CL.sourceList ["A", "B", "C"]
                withIndex :: C.Conduit String IO (Integer, String)
                withIndex = CI.mergeSource (CL.sourceList [1..])
            output <- src C.$= withIndex C.$$ CL.consume
            output `shouldBe` [(1, "A"), (2, "B"), (3, "C")]
        it "does stop processing when the source exhausted" $ do
            let src :: C.Source IO Integer
                src = CL.sourceList [1..]
                withShortAlphaIndex :: C.Conduit Integer IO (String, Integer)
                withShortAlphaIndex = CI.mergeSource (CL.sourceList ["A", "B", "C"])
            output <- src C.$= withShortAlphaIndex C.$$ CL.consume
            output `shouldBe` [("A", 1), ("B", 2), ("C", 3)]

        let modFlag ref cur next = do
                prev <- I.atomicModifyIORef ref $ (,) next
                prev `shouldBe` cur
            flagShouldBe ref expect = do
                cur <- I.readIORef ref
                cur `shouldBe` expect
        it "properly run the finalizer - When the main Conduit is fully consumed" $ do
            called <- I.newIORef ("RawC" :: String)
            let src :: MonadIO m => C.Source m String
                src = CL.sourceList ["A", "B", "C"]
                withIndex :: MonadIO m => C.Conduit String m (Integer, String)
                withIndex = C.addCleanup (\f -> liftIO $ modFlag called "AllocC-3" ("FinalC:" ++ show f)) . CI.mergeSource $ do
                    liftIO $ modFlag called "RawC" "AllocC-1"
                    C.yield 1
                    liftIO $ modFlag called "AllocC-1" "AllocC-2"
                    C.yield 2
                    liftIO $ modFlag called "AllocC-2" "AllocC-3"
                    C.yield 3
                    liftIO $ modFlag called "AllocC-3" "AllocC-4"
                    C.yield 4
            output <- src C.$= withIndex C.$$ CL.consume
            output `shouldBe` [(1, "A"), (2, "B"), (3, "C")]
            called `flagShouldBe` "FinalC:True"
        it "properly run the finalizer - When the branch Source is fully consumed" $ do
            called <- I.newIORef ("RawS" :: String)
            let src :: MonadIO m => C.Source m Integer
                src = CL.sourceList [1..]
                withIndex :: MonadIO m => C.Conduit Integer m (String, Integer)
                withIndex = C.addCleanup (\f -> liftIO $ modFlag called "AllocS-C" ("FinalS:" ++ show f)) . CI.mergeSource $ do
                    liftIO $ modFlag called "RawS" "AllocS-A"
                    C.yield "A"
                    liftIO $ modFlag called "AllocS-A" "AllocS-B"
                    C.yield "B"
                    liftIO $ modFlag called "AllocS-B" "AllocS-C"
                    C.yield "C"
            output <- src C.$= withIndex C.$$ CL.consume
            output `shouldBe` [("A", 1), ("B", 2), ("C", 3)]
            called `flagShouldBe` "FinalS:True"
        it "properly DO NOT run the finalizer - When nothing consumed" $ do
            called <- I.newIORef ("Raw0" :: String)
            let src :: MonadIO m => C.Source m String
                src = CL.sourceList ["A", "B", "C"]
                withIndex :: MonadIO m => C.Conduit String m (Integer, String)
                withIndex = C.addCleanup (\f -> liftIO $ modFlag called "WONT CALLED" ("Final0:" ++ show f)) . CI.mergeSource $ do
                    liftIO $ modFlag called "Raw0" "Alloc0-1"
                    C.yield 1
            output <- src C.$= withIndex C.$$ return ()
            output `shouldBe` ()
            called `flagShouldBe` "Raw0"
        it "properly run the finalizer - When only one item consumed" $ do
            called <- I.newIORef ("Raw1" :: String)
            let src :: MonadIO m => C.Source m Integer
                src = CL.sourceList [1..]
                withIndex :: MonadIO m => C.Conduit Integer m (String, Integer)
                withIndex = C.addCleanup (\f -> liftIO $ modFlag called "Alloc1-A" ("Final1:" ++ show f)) . CI.mergeSource $ do
                    liftIO $ modFlag called "Raw1" "Alloc1-A"
                    C.yield "A"
                    liftIO $ modFlag called "Alloc1-A" "Alloc1-B"
                    C.yield "B"
                    liftIO $ modFlag called "Alloc1-B" "Alloc1-C"
                    C.yield "C"
            output <- src C.$= withIndex C.$= CL.isolate 1 C.$$ CL.consume
            output `shouldBe` [("A", 1)]
            called `flagShouldBe` "Final1:False"

        it "handles finalizers" $ do
            ref <- I.newIORef (0 :: Int)
            let src1 = C.addCleanup
                    (const $ I.modifyIORef ref (+1))
                    (mapM_ C.yield [1 :: Int ..])
                src2 = mapM_ C.yield ("hi" :: String)
            res1 <- src1 C.$$ C.mergeSource src2 C.=$ CL.consume
            res1 `shouldBe` [('h', 1), ('i', 2)]
            i1 <- I.readIORef ref
            i1 `shouldBe` 1

            res2 <- src2 C.$$ C.mergeSource src1 C.=$ CL.consume
            res2 `shouldBe` [(1, 'h'), (2, 'i')]
            i2 <- I.readIORef ref
            i2 `shouldBe` 2

    describe "passthroughSink" $ do
        it "works" $ do
            ref <- I.newIORef (-1)
            let sink = CL.fold (+) (0 :: Int)
                conduit = C.passthroughSink sink (I.writeIORef ref)
                input = [1..10]
            output <- mapM_ C.yield input C.$$ conduit C.=$ CL.consume
            output `shouldBe` input
            x <- I.readIORef ref
            x `shouldBe` sum input
        it "does nothing when downstream does nothing" $ do
            ref <- I.newIORef (-1)
            let sink = CL.fold (+) (0 :: Int)
                conduit = C.passthroughSink sink (I.writeIORef ref)
                input = [undefined]
            mapM_ C.yield input C.$$ conduit C.=$ return ()
            x <- I.readIORef ref
            x `shouldBe` (-1)

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
            -- FIXME this would be better msgs `shouldBe` words "acquire 1 2 3 4 inside release computation"
            msgs `shouldBe` words "acquire 1 2 3 4 release inside computation"

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

        describe "dan burton's associative tests" $ do
            let tellLn = tell . (++ "\n")
                finallyP fin = CI.addCleanup (const fin)
                printer = CI.awaitForever $ lift . tellLn . show
                idMsg msg = finallyP (tellLn msg) CI.idP
                takeP 0 = return ()
                takeP n = CI.awaitE >>= \ex -> case ex of
                  Left _u -> return ()
                  Right i -> CI.yield i >> takeP (pred n)

                testPipe p = execWriter $ runPipe $ printer <+< p <+< CI.sourceList ([1..] :: [Int])

                p1 = takeP (1 :: Int)
                p2 = idMsg "foo"
                p3 = idMsg "bar"

                (<+<) = (CI.<+<)
                runPipe = CI.runPipe

                test1L = testPipe $ (p1 <+< p2) <+< p3
                test1R = testPipe $ p1 <+< (p2 <+< p3)

                _test2L = testPipe $ (p2 <+< p1) <+< p3
                _test2R = testPipe $ p2 <+< (p1 <+< p3)

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
            -- FIXME this is broken it "test2" $ verify test2L test2R "p2" "p1" "p3"
            it "test3" $ verify test3L test3R "p2" "p3" "p1"

    describe "Data.Conduit.Lift" $ do
        it "execStateC" $ do
            let sink = C.execStateLC 0 $ CL.mapM_ $ modify . (+)
                src = mapM_ C.yield [1..10 :: Int]
            res <- src C.$$ sink
            res `shouldBe` sum [1..10]

        it "execWriterC" $ do
            let sink = C.execWriterLC $ CL.mapM_ $ tell . return
                src = mapM_ C.yield [1..10 :: Int]
            res <- src C.$$ sink
            res `shouldBe` [1..10]

        it "runErrorC" $ do
            let sink = C.runErrorC $ do
                    x <- C.catchErrorC (lift $ throwError "foo") return
                    return $ x ++ "bar"
            res <- return () C.$$ sink
            res `shouldBe` Right ("foobar" :: String)

        it "runMaybeC" $ do
            let src = void $ C.runMaybeC $ do
                    C.yield 1
                    () <- lift $ MaybeT $ return Nothing
                    C.yield 2
                sink = CL.consume
            res <- src C.$$ sink
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
            res <- srcs C.$$ CL.consume
            res `shouldBe`
                [ Map.fromList [(1, 1), (2, 3), (3, 2)]
                , Map.fromList [(1, 2), (2, 2), (3, 2)]
                , Map.fromList [(1, 3), (2, 1), (3, 2)]
                ]
    describe "zipSink" $ do
        it "zip equal-sized" $ do
            x <- runResourceT $
                    CL.sourceList [1..100] C.$$
                    C.sequenceSinks [ CL.fold (+) 0,
                                   (`mod` 101) <$> CL.fold (*) 1 ]
            x `shouldBe` [5050, 100 :: Integer]

        it "zip distinct sizes" $ do
            let sink = C.getZipSink $
                        (*) <$> C.ZipSink (CL.fold (+) 0)
                            <*> C.ZipSink (Data.Maybe.fromJust <$> C.await)
            x <- C.runResourceT $ CL.sourceList [100,99..1] C.$$ sink
            x `shouldBe` (505000 :: Integer)

    describe "upstream results" $ do
        it "fuseBoth" $ do
            let upstream = do
                    C.yield ("hello" :: String)
                    CL.isolate 5 C.=$= CL.fold (+) 0
                downstream = C.fuseBoth upstream CL.consume
            res <- CL.sourceList [1..10 :: Int] C.$$ do
                (x, y) <- downstream
                z <- CL.consume
                return (x, y, z)
            res `shouldBe` (sum [1..5], ["hello"], [6..10])

        it "fuseBothMaybe with no result" $ do
            let src = mapM_ C.yield [1 :: Int ..]
                sink = CL.isolate 5 C.=$= CL.fold (+) 0
            (mup, down) <- C.runConduit $ C.fuseBothMaybe src sink
            mup `shouldBe` (Nothing :: Maybe ())
            down `shouldBe` sum [1..5]

        it "fuseBothMaybe with result" $ do
            let src = mapM_ C.yield [1 :: Int .. 5]
                sink = CL.isolate 6 C.=$= CL.fold (+) 0
            (mup, down) <- C.runConduit $ C.fuseBothMaybe src sink
            mup `shouldBe` Just ()
            down `shouldBe` sum [1..5]

        it "fuseBothMaybe with almost result" $ do
            let src = mapM_ C.yield [1 :: Int .. 5]
                sink = CL.isolate 5 C.=$= CL.fold (+) 0
            (mup, down) <- C.runConduit $ C.fuseBothMaybe src sink
            mup `shouldBe` (Nothing :: Maybe ())
            down `shouldBe` sum [1..5]

    describe "catching exceptions" $ do
        it "works" $ do
            let src = do
                    C.yield 1
                    () <- Catch.throwM DummyError
                    C.yield 2
                src' = do
                    Catch.catch src (\DummyError -> C.yield (3 :: Int))
            res <- src' C.$$ CL.consume
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
instance Error DummyError
instance Catch.Exception DummyError
