{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Spec (spec) where

import Conduit
import Prelude hiding (FilePath)
import Data.Maybe (listToMaybe)
import Data.Conduit.Combinators (slidingWindow, chunksOfE, chunksOfExactlyE)
import Data.List (intersperse, sort, find, mapAccumL)
import Safe (tailSafe)
import System.FilePath (takeExtension, (</>))
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Control.Monad (liftM)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Writer
import qualified System.IO as IO
#if ! MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid (..))
import Control.Applicative ((<$>), (<*>))
#endif
#if MIN_VERSION_mono_traversable(1,0,0)
import Data.Sequences (LazySequence (..), Utf8 (..))
#else
import Data.Sequences.Lazy
import Data.Textual.Encoding
#endif
import qualified Data.NonNull as NN
import System.IO.Silently (hCapture)
import GHC.IO.Handle (hDuplicateTo)
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified StreamSpec
import GHC.IO (catchAny)
import Control.Exception (fromException)
import UnliftIO.Exception (pureTry, stringException)
import Control.Concurrent.Async (ExceptionInLinkedThread (..))
import Control.Concurrent.MVar

spec :: Spec
spec = do
    describe "yieldMany" $ do
        it "list" $
            runConduitPure (yieldMany [1..10] .| sinkList)
            `shouldBe` [1..10]
        it "Text" $
            runConduitPure (yieldMany ("Hello World" :: T.Text) .| sinkList)
            `shouldBe` "Hello World"
    it "unfold" $
        let f 11 = Nothing
            f i = Just (show i, i + 1)
         in runConduitPure (unfoldC f 1 .| sinkList)
            `shouldBe` map show [1..10]
    it "enumFromTo" $
        runConduitPure (enumFromToC 1 10 .| sinkList) `shouldBe` [1..10]
    it "iterate" $
        let f i = i + 1
            src = iterateC f seed
            seed = 1
            count = 10
            res = runConduitPure $ src .| takeC count .| sinkList
         in res `shouldBe` take count (iterate f seed)
    it "repeat" $
        let src = repeatC seed
            seed = 1
            count = 10
            res = runConduitPure $ src .| takeC count .| sinkList
         in res `shouldBe` take count (repeat seed)
    it "replicate" $
        let src = replicateC count seed
            seed = 1
            count = 10
            res = runConduitPure $ src .| sinkList
         in res `shouldBe` replicate count seed
    it "sourceLazy" $
        let tss = ["foo", "bar", "baz"]
            tl = TL.fromChunks tss
            res = runConduitPure $ sourceLazy tl .| sinkList
         in res `shouldBe` tss
    it "repeatM" $
        let src = repeatMC (return seed)
            seed = 1
            count = 10
            res = runConduitPure $ src .| takeC count .| sinkList
         in res `shouldBe` take count (repeat seed)
    it "repeatWhileM" $ do
        ref <- newIORef 0
        let f = atomicModifyIORef ref $ \i -> (succ i, succ i)
            src = repeatWhileMC f (< 11)
        res <- runConduit $ src .| sinkList
        res `shouldBe` [1..10]
    it "replicateM" $ do
        ref <- newIORef 0
        let f = atomicModifyIORef ref $ \i -> (succ i, succ i)
            src = replicateMC 10 f
        res <- runConduit $ src .| sinkList
        res `shouldBe` [1..10]
    it "sourceFile" $ do
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        res <- runConduitRes $ sourceFile fp .| sinkLazy
        nocrBL res `shouldBe` TL.encodeUtf8 (TL.pack contents)
    it "sourceHandle" $ do
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        res <- IO.withBinaryFile "tmp" IO.ReadMode $ \h ->
          runConduit $ sourceHandle h .| sinkLazy
        nocrBL res `shouldBe` TL.encodeUtf8 (TL.pack contents)
    it "sourceIOHandle" $ do
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        let open = IO.openBinaryFile "tmp" IO.ReadMode
        res <- runConduitRes $ sourceIOHandle open .| sinkLazy
        nocrBL res `shouldBe` TL.encodeUtf8 (TL.pack contents)
    prop "stdin" $ \(S.pack -> content) -> do
        S.writeFile "tmp" content
        IO.withBinaryFile "tmp" IO.ReadMode $ \h -> do
            hDuplicateTo h IO.stdin
            x <- runConduit $ stdinC .| foldC
            x `shouldBe` content
    let hasExtension' ext fp = takeExtension fp == ext
    it "sourceDirectory" $ do
        res <- runConduitRes
             $ sourceDirectory "test" .| filterC (not . hasExtension' ".swp") .| sinkList
        sort res `shouldBe`
          [ "test" </> "Data"
          , "test" </> "Spec.hs"
          , "test" </> "StreamSpec.hs"
          , "test" </> "doctests.hs"
          , "test" </> "main.hs"
          , "test" </> "subdir"
          ]
    it "sourceDirectoryDeep" $ do
        res1 <- runConduitRes
              $ sourceDirectoryDeep False "test" .| filterC (not . hasExtension' ".swp") .| sinkList
        res2 <- runConduitRes
              $ sourceDirectoryDeep True "test" .| filterC (not . hasExtension' ".swp") .| sinkList
        sort res1 `shouldBe`
          [ "test" </> "Data" </> "Conduit" </> "Extra" </> "ZipConduitSpec.hs"
          , "test" </> "Data" </> "Conduit" </> "StreamSpec.hs"
          , "test" </> "Spec.hs"
          , "test" </> "StreamSpec.hs"
          , "test" </> "doctests.hs"
          , "test" </> "main.hs"
          , "test" </> "subdir" </> "dummyfile.txt"
          ]
        sort res1 `shouldBe` sort res2
    prop "drop" $ \(T.pack -> input) count ->
        runConduitPure (yieldMany input .| (dropC count >>= \() -> sinkList))
        `shouldBe` T.unpack (T.drop count input)
    prop "dropE" $ \(T.pack -> input) ->
        runConduitPure (yield input .| (dropCE 5 >>= \() -> foldC))
        `shouldBe` T.drop 5 input
    prop "dropWhile" $ \(T.pack -> input) sep ->
        runConduitPure (yieldMany input .| (dropWhileC (<= sep) >>= \() -> sinkList))
        `shouldBe` T.unpack (T.dropWhile (<= sep) input)
    prop "dropWhileE" $ \(T.pack -> input) sep ->
        runConduitPure (yield input .| (dropWhileCE (<= sep) >>= \() -> foldC))
        `shouldBe` T.dropWhile (<= sep) input
    it "fold" $
        let list = [[1..10], [11..20]]
            src = yieldMany list
            res = runConduitPure $ src .| foldC
         in res `shouldBe` concat list
    it "foldE" $
        let list = [[1..10], [11..20]]
            src = yieldMany $ Identity list
            res = runConduitPure $ src .| foldCE
         in res `shouldBe` concat list
    it "foldl" $
        let res = runConduitPure $ yieldMany [1..10] .| foldlC (+) 0
         in res `shouldBe` sum [1..10]
    it "foldlE" $
        let res = runConduitPure $ yield [1..10] .| foldlCE (+) 0
         in res `shouldBe` sum [1..10]
    it "foldMap" $
        let src = yieldMany [1..10]
            res = runConduitPure $ src .| foldMapC return
         in res `shouldBe` [1..10]
    it "foldMapE" $
        let src = yield [1..10]
            res = runConduitPure $ src .| foldMapCE return
         in res `shouldBe` [1..10]
    prop "all" $ \ (input :: [Int]) -> runConduitPure (yieldMany input .| allC even) `shouldBe` all evenInt input
    prop "allE" $ \ (input :: [Int]) -> runConduitPure (yield input .| allCE even) `shouldBe` all evenInt input
    prop "any" $ \ (input :: [Int]) -> runConduitPure (yieldMany input .| anyC even) `shouldBe` any evenInt input
    prop "anyE" $ \ (input :: [Int]) -> runConduitPure (yield input .| anyCE even) `shouldBe` any evenInt input
    prop "and" $ \ (input :: [Bool]) -> runConduitPure (yieldMany input .| andC) `shouldBe` and input
    prop "andE" $ \ (input :: [Bool]) -> runConduitPure (yield input .| andCE) `shouldBe` and input
    prop "or" $ \ (input :: [Bool]) -> runConduitPure (yieldMany input .| orC) `shouldBe` or input
    prop "orE" $ \ (input :: [Bool]) -> runConduitPure (yield input .| orCE) `shouldBe` or input
    prop "elem" $ \x xs -> runConduitPure (yieldMany xs .| elemC x) `shouldBe` elemInt x xs
    prop "elemE" $ \x xs -> runConduitPure (yield xs .| elemCE x) `shouldBe` elemInt x xs
    prop "notElem" $ \x xs -> runConduitPure (yieldMany xs .| notElemC x) `shouldBe` notElemInt x xs
    prop "notElemE" $ \x xs -> runConduitPure (yield xs .| notElemCE x) `shouldBe` notElemInt x xs
    prop "sinkVector regular" $ \xs -> do
        res <- runConduit $ yieldMany xs .| sinkVector
        res `shouldBe` V.fromList (xs :: [Int])
    prop "sinkVector unboxed" $ \xs -> do
        res <- runConduit $ yieldMany xs .| sinkVector
        res `shouldBe` VU.fromList (xs :: [Int])
    prop "sinkVector storable" $ \xs -> do
        res <- runConduit $ yieldMany xs .| sinkVector
        res `shouldBe` VS.fromList (xs :: [Int])
    prop "sinkVectorN regular" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- runConduit $ yieldMany xs' .| sinkVectorN maxSize
        res `shouldBe` V.fromList (xs :: [Int])
    prop "sinkVectorN unboxed" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- runConduit $ yieldMany xs' .| sinkVectorN maxSize
        res `shouldBe` VU.fromList (xs :: [Int])
    prop "sinkVectorN storable" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- runConduit $ yieldMany xs' .| sinkVectorN maxSize
        res `shouldBe` VS.fromList (xs :: [Int])
    prop "sinkBuilder" $ \(map S.pack -> inputs) ->
        let builder = runConduitPure $ yieldMany inputs .| foldMapC byteString
            ltext = toLazyByteString builder
         in ltext `shouldBe` fromChunks inputs
    prop "sinkLazyBuilder" $ \(map S.pack -> inputs) ->
        let lbs = runConduitPure (yieldMany (map byteString inputs) .| sinkLazyBuilder)
         in lbs `shouldBe` fromChunks inputs
    prop "sinkNull" $ \xs toSkip -> do
        res <- runConduit $ yieldMany xs .| do
            takeC toSkip .| sinkNull
            sinkList
        res `shouldBe` drop toSkip (xs :: [Int])
    prop "awaitNonNull" $ \xs ->
        fmap NN.toNullable (runConduitPure $ yieldMany xs .| awaitNonNull)
        `shouldBe` listToMaybe (filter (not . null) (xs :: [[Int]]))
    prop "headE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| ((,) <$> headCE <*> foldC))
        `shouldBe` (listToMaybe $ concat xs, drop 1 $ concat xs)
    prop "peek" $ \xs ->
        runConduitPure (yieldMany xs .| ((,) <$> peekC <*> sinkList))
        `shouldBe` (listToMaybe xs, xs :: [Int])
    prop "peekE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| ((,) <$> peekCE <*> foldC))
        `shouldBe` (listToMaybe $ concat xs, concat xs)
    prop "last" $ \xs ->
        runConduitPure (yieldMany xs .| lastC)
        `shouldBe` listToMaybe (reverse (xs :: [Int]))
    prop "lastE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| lastCE)
        `shouldBe` listToMaybe (reverse (concat xs))
    prop "length" $ \xs ->
        runConduitPure (yieldMany xs .| lengthC)
        `shouldBe` length (xs :: [Int])
    prop "lengthE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| lengthCE)
        `shouldBe` length (concat xs)
    prop "lengthIf" $ \x xs ->
        runConduitPure (yieldMany xs .| lengthIfC (< x))
        `shouldBe` length (filter (< x) xs :: [Int])
    prop "lengthIfE" $ \x (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| lengthIfCE (< x))
        `shouldBe` length (filter (< x) (concat xs))
    prop "maximum" $ \xs ->
        runConduitPure (yieldMany xs .| maximumC)
        `shouldBe` (if null (xs :: [Int]) then Nothing else Just (maximum xs))
    prop "maximumE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| maximumCE)
        `shouldBe` (if null (concat xs) then Nothing else Just (maximum $ concat xs))
    prop "minimum" $ \xs ->
        runConduitPure (yieldMany xs .| minimumC)
        `shouldBe` (if null (xs :: [Int]) then Nothing else Just (minimum xs))
    prop "minimumE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| minimumCE)
        `shouldBe` (if null (concat xs) then Nothing else Just (minimum $ concat xs))
    prop "null" $ \xs ->
        runConduitPure (yieldMany xs .| nullC)
        `shouldBe` null (xs :: [Int])
    prop "nullE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| ((,) <$> nullCE <*> foldC))
        `shouldBe` (null (concat xs), concat xs)
    prop "sum" $ \xs ->
        runConduitPure (yieldMany xs .| sumC)
        `shouldBe` sum (xs :: [Int])
    prop "sumE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| sumCE)
        `shouldBe` sum (concat xs)
    prop "product" $ \xs ->
        runConduitPure (yieldMany xs .| productC)
        `shouldBe` product (xs :: [Int])
    prop "productE" $ \ (xs :: [[Int]]) ->
        runConduitPure (yieldMany xs .| productCE)
        `shouldBe` product (concat xs)
    prop "find" $ \x xs ->
        runConduitPure (yieldMany xs .| findC (< x))
        `shouldBe` find (< x) (xs :: [Int])
    prop "mapM_" $ \xs ->
        let res = execWriter $ runConduit $ yieldMany xs .| mapM_C (tell . return)
         in res `shouldBe` (xs :: [Int])
    prop "mapM_E" $ \xs ->
        let res = execWriter $ runConduit $ yield xs .| mapM_CE (tell . return)
         in res `shouldBe` (xs :: [Int])
    prop "foldM" $ \ (xs :: [Int]) -> do
        res <- runConduit $ yieldMany xs .| foldMC addM 0
        res `shouldBe` sum xs
    prop "foldME" $ \ (xs :: [Int]) -> do
        res <- runConduit $ yield xs .| foldMCE addM 0
        res `shouldBe` sum xs
    it "foldMapM" $
        let src = yieldMany [1..10]
            res = runConduitPure $ src .| foldMapMC (return . return)
         in res `shouldBe` [1..10]
    it "foldMapME" $
        let src = yield [1..10]
            res = runConduitPure $ src .| foldMapMCE (return . return)
         in res `shouldBe` [1..10]
    it "sinkFile" $ do
        let contents = mconcat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
        runConduitRes $ yield contents .| sinkFile fp
        res <- S.readFile fp
        res `shouldBe` contents
    it "sinkHandle" $ do
        let contents = mconcat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
        IO.withBinaryFile "tmp" IO.WriteMode $ \h -> runConduit $ yield contents .| sinkHandle h
        res <- S.readFile fp
        res `shouldBe` contents
    it "sinkIOHandle" $ do
        let contents = mconcat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
            open = IO.openBinaryFile "tmp" IO.WriteMode
        runConduitRes $ yield contents .| sinkIOHandle open
        res <- S.readFile fp
        res `shouldBe` contents
    prop "print" $ \vals -> do
        let expected = Prelude.unlines $ map showInt vals
        (actual, ()) <- hCapture [IO.stdout] $ runConduit $ yieldMany vals .| printC
        actual `shouldBe` expected
#ifndef WINDOWS
    prop "stdout" $ \ (vals :: [String]) -> do
        let expected = concat vals
        (actual, ()) <- hCapture [IO.stdout] $ runConduit $ yieldMany (map T.pack vals) .| encodeUtf8C .| stdoutC
        actual `shouldBe` expected
    prop "stderr" $ \ (vals :: [String]) -> do
        let expected = concat vals
        (actual, ()) <- hCapture [IO.stderr] $ runConduit $ yieldMany (map T.pack vals) .| encodeUtf8C .| stderrC
        actual `shouldBe` expected
#endif
    prop "map" $ \input ->
        runConduitPure (yieldMany input .| mapC succChar .| sinkList)
        `shouldBe` map succChar input
    prop "mapE" $ \(map V.fromList -> inputs) ->
        runConduitPure (yieldMany inputs .| mapCE succChar .| foldC)
        `shouldBe` V.map succChar (V.concat inputs)
    prop "omapE" $ \(map T.pack -> inputs) ->
        runConduitPure (yieldMany inputs .| omapCE succChar .| foldC)
        `shouldBe` T.map succChar (T.concat inputs)
    prop "concatMap" $ \ (input :: [Int]) ->
        runConduitPure (yieldMany input .| concatMapC showInt .| sinkList)
        `shouldBe` concatMap showInt input
    prop "concatMapE" $ \ (input :: [Int]) ->
        runConduitPure (yield input .| concatMapCE showInt .| foldC)
        `shouldBe` concatMap showInt input
    prop "take" $ \(T.pack -> input) count ->
        runConduitPure (yieldMany input .| (takeC count >>= \() -> mempty) .| sinkList)
        `shouldBe` T.unpack (T.take count input)
    prop "takeE" $ \(T.pack -> input) count ->
        runConduitPure (yield input .| (takeCE count >>= \() -> mempty) .| foldC)
        `shouldBe` T.take count input
    prop "takeWhile" $ \(T.pack -> input) sep ->
        runConduitPure (yieldMany input .| do
            x <- (takeWhileC (<= sep) >>= \() -> mempty) .| sinkList
            y <- sinkList
            return (x, y))
        `shouldBe` span (<= sep) (T.unpack input)
    prop "takeWhileE" $ \(T.pack -> input) sep ->
        runConduitPure (yield input .| do
            x <- (takeWhileCE (<= sep) >>= \() -> mempty) .| foldC
            y <- foldC
            return (x, y))
        `shouldBe` T.span (<= sep) input
    it "takeExactly" $
        let src = yieldMany [1..10]
            sink = do
                x <- takeExactlyC 5 $ return 1
                y <- sinkList
                return (x, y)
            res = runConduitPure $ src .| sink
         in res `shouldBe` (1, [6..10])
    it "takeExactlyE" $
        let src = yield ("Hello World" :: T.Text)
            sink = do
                takeExactlyCE 5 (mempty :: ConduitT T.Text Void Identity ())
                y <- sinkLazy
                return y
            res = runConduitPure $ src .| sink
         in res `shouldBe` " World"
    it "takeExactlyE Vector" $ do
        let src = yield (V.fromList $ T.unpack "Hello World")
            sink = do
                x <- takeExactlyCE 5 $ return 1
                y <- foldC
                return (x, y)
        res <- runConduit $ src .| sink
        res `shouldBe` (1, V.fromList $ T.unpack " World")
    it "takeExactlyE 2" $
        let src = yield ("Hello World" :: T.Text)
            sink = do
                x <- takeExactlyCE 5 $ return 1
                y <- sinkLazy
                return (x, y)
            res = runConduitPure $ src .| sink
            -- FIXME type signature on next line is necessary in GHC 7.6.3 to
            -- avoid a crash:
            --
            -- test: internal error: ARR_WORDS object entered!
            --     (GHC version 7.6.3 for x86_64_unknown_linux)
            --     Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
            -- Aborted (core dumped)
            --
            -- Report upstream when packages are released
         in res `shouldBe` (1, " World" :: TL.Text)
    prop "concat" $ \input ->
        runConduitPure (yield (T.pack input) .| concatC .| sinkList)
        `shouldBe` input
    prop "filter" $ \input ->
        runConduitPure (yieldMany input .| filterC evenInt .| sinkList)
        `shouldBe` filter evenInt input
    prop "filterE" $ \input ->
        runConduitPure (yield input .| filterCE evenInt .| foldC)
        `shouldBe` filter evenInt input
    prop "mapWhile" $ \input (min 20 -> highest) ->
        let f i | i < highest = Just (i + 2 :: Int)
                | otherwise   = Nothing
            res = runConduitPure $ yieldMany input .| do
                x <- (mapWhileC f >>= \() -> mempty) .| sinkList
                y <- sinkList
                return (x, y)
            (taken, dropped) = span (< highest) input
         in res `shouldBe` (map (+ 2) taken, dropped)
    prop "conduitVector" $ \(take 200 -> input) size' -> do
        let size = min 30 $ succ $ abs size'
        res <- runConduit $ yieldMany input .| conduitVector size .| sinkList
        res `shouldSatisfy` all (\v -> V.length v <= size)
        drop 1 (reverse res) `shouldSatisfy` all (\v -> V.length v == size)
        V.concat res `shouldBe` V.fromList (input :: [Int])
    prop "scanl" $ \input seed ->
        let f a b = a + b :: Int
            res = runConduitPure $ yieldMany input .| scanlC f seed .| sinkList
         in res `shouldBe` scanl f seed input
    prop "mapAccumWhile" $ \input (min 20 -> highest) ->
        let f i accum | i < highest = Right (i + accum, 2 * i :: Int)
                      | otherwise   = Left accum
            res = runConduitPure $ yieldMany input .| do
                (s, x) <- fuseBoth (mapAccumWhileC f 0) sinkList
                y <- sinkList
                return (s, x, y)
            (taken, dropped) = span (< highest) input
         in res `shouldBe` (sum taken, map (* 2) taken, tailSafe dropped)
    prop "concatMapAccum" $ \(input :: [Int]) ->
        let f a accum = (a + accum, [a, accum])
            res = runConduitPure $ yieldMany input .| concatMapAccumC f 0 .| sinkList
            expected = concat $ snd $ mapAccumL (flip f) 0 input
         in res `shouldBe` expected
    prop "intersperse" $ \xs x ->
        runConduitPure (yieldMany xs .| intersperseC x .| sinkList)
        `shouldBe` intersperse (x :: Int) xs
    prop "mapM" $ \input ->
        runConduitPure (yieldMany input .| mapMC (return . succChar) .| sinkList)
        `shouldBe` map succChar input
    prop "mapME" $ \(map V.fromList -> inputs) ->
        runConduitPure (yieldMany inputs .| mapMCE (return . succChar) .| foldC)
        `shouldBe` V.map succChar (V.concat inputs)
    prop "omapME" $ \(map T.pack -> inputs) ->
        runConduitPure (yieldMany inputs .| omapMCE (return . succChar) .| foldC)
        `shouldBe` T.map succChar (T.concat inputs)
    prop "concatMapM" $ \ (input :: [Int]) ->
        runConduitPure (yieldMany input .| concatMapMC (return . showInt) .| sinkList)
        `shouldBe` concatMap showInt input
    prop "filterM" $ \input ->
        runConduitPure (yieldMany input .| filterMC (return . evenInt) .| sinkList)
        `shouldBe` filter evenInt input
    prop "filterME" $ \input ->
        runConduitPure (yield input .| filterMCE (return . evenInt) .| foldC)
        `shouldBe` filter evenInt input
    prop "iterM" $ \input -> do
        (x, y) <- runWriterT $ runConduit $ yieldMany input .| iterMC (tell . return) .| sinkList
        x `shouldBe` (input :: [Int])
        y `shouldBe` input
    prop "scanlM" $ \input seed ->
        let f a b = a + b :: Int
            fm a b = return $ a + b
            res = runConduitPure $ yieldMany input .| scanlMC fm seed .| sinkList
         in res `shouldBe` scanl f seed input
    prop "mapAccumWhileM" $ \input (min 20 -> highest) ->
        let f i accum | i < highest = Right (i + accum, 2 * i :: Int)
                      | otherwise   = Left accum
            res = runConduitPure $ yieldMany input .| do
                (s, x) <- fuseBoth (mapAccumWhileMC ((return.).f) 0) sinkList
                y <- sinkList
                return (s, x, y)
            (taken, dropped) = span (< highest) input
         in res `shouldBe` (sum taken, map (* 2) taken, tailSafe dropped)
    prop "concatMapAccumM" $ \(input :: [Int]) ->
        let f a accum = (a + accum, [a, accum])
            res = runConduitPure $ yieldMany input .| concatMapAccumMC ((return.).f) 0 .| sinkList
            expected = concat $ snd $ mapAccumL (flip f) 0 input
         in res `shouldBe` expected
    prop "encode UTF8" $ \(map T.pack -> inputs) -> do
        let expected = encodeUtf8 $ fromChunks inputs
        actual <- runConduit
                $ yieldMany inputs
               .| encodeUtf8C
               .| sinkLazy
        actual `shouldBe` expected
    prop "encode/decode UTF8" $ \(map T.pack -> inputs) (min 50 . max 1 . abs -> chunkSize) -> do
        let expected = fromChunks inputs
        actual <- runConduit
                $ yieldMany inputs
               .| encodeUtf8C
               .| concatC
               .| conduitVector chunkSize
               .| mapC (S.pack . V.toList)
               .| decodeUtf8C
               .| sinkLazy
        actual `shouldBe` expected
    it "invalid UTF8 is an exception" $
      case runConduit $ yield "\129" .| decodeUtf8C .| sinkLazy of
        Left _ -> return () :: IO ()
        Right x -> error $ "this should have failed, got: " ++ show x
    prop "encode/decode UTF8 lenient" $ \(map T.pack -> inputs) (min 50 . max 1 . abs -> chunkSize) -> do
        let expected = fromChunks inputs
        actual <- runConduit
                $ yieldMany inputs
               .| encodeUtf8C
               .| concatC
               .| conduitVector chunkSize
               .| mapC (S.pack . V.toList)
               .| decodeUtf8LenientC
               .| sinkLazy
        actual `shouldBe` expected
    prop "line" $ \(map T.pack -> input) size ->
        let src = yieldMany input
            sink = do
                x <- lineC $ takeCE size .| foldC
                y <- foldC
                return (x, y)
            res = runConduitPure $ src .| sink
            expected =
                let (x, y) = T.break (== '\n') (T.concat input)
                 in (T.take size x, T.drop 1 y)
         in res `shouldBe` expected
    prop "lineAscii" $ \(map S.pack -> input) size ->
        let src = yieldMany input
            sink = do
                x <- lineAsciiC $ takeCE size .| foldC
                y <- foldC
                return (x, y)
            res = runConduitPure $ src .| sink
            expected =
                let (x, y) = S.break (== 10) (S.concat input)
                 in (S.take size x, S.drop 1 y)
         in res `shouldBe` expected
    prop "unlines" $ \(map T.pack -> input) ->
        runConduitPure (yieldMany input .| unlinesC .| foldC)
        `shouldBe` T.unlines input
    prop "unlinesAscii" $ \(map S.pack -> input) ->
        runConduitPure (yieldMany input .| unlinesAsciiC .| foldC)
        `shouldBe` S8.unlines input
    prop "linesUnbounded" $ \(map T.pack -> input) ->
        runConduitPure (yieldMany input .| (linesUnboundedC >>= \() -> mempty) .| sinkList)
        `shouldBe` T.lines (T.concat input)
    prop "linesUnboundedAscii" $ \(map S.pack -> input) ->
        runConduitPure (yieldMany input .| (linesUnboundedAsciiC >>= \() -> mempty) .| sinkList)
        `shouldBe` S8.lines (S.concat input)
    it "slidingWindow 0" $
        let res = runConduitPure $ yieldMany [1..5] .| slidingWindow 0 .| sinkList
        in res `shouldBe` [[1],[2],[3],[4],[5]]
    it "slidingWindow 1" $
        let res = runConduitPure $ yieldMany [1..5] .| slidingWindow 1 .| sinkList
        in res `shouldBe` [[1],[2],[3],[4],[5]]
    it "slidingWindow 2" $
        let res = runConduitPure $ yieldMany [1..5] .| slidingWindow 2 .| sinkList
        in res `shouldBe` [[1,2],[2,3],[3,4],[4,5]]
    it "slidingWindow 3" $
        let res = runConduitPure $ yieldMany [1..5] .| slidingWindow 3 .| sinkList
        in res `shouldBe` [[1,2,3],[2,3,4],[3,4,5]]
    it "slidingWindow 4" $
        let res = runConduitPure $ yieldMany [1..5] .| slidingWindow 4 .| sinkList
        in res `shouldBe` [[1,2,3,4],[2,3,4,5]]
    it "slidingWindow 5" $
        let res = runConduitPure $ yieldMany [1..5] .| slidingWindow 5 .| sinkList
        in res `shouldBe` [[1,2,3,4,5]]
    it "slidingWindow 6" $
        let res = runConduitPure $ yieldMany [1..5] .| slidingWindow 6 .| sinkList
        in res `shouldBe` [[1,2,3,4,5]]
    it "chunksOfE 1" $
        let res = runConduitPure $ yieldMany [[1,2], [3,4], [5,6]] .| chunksOfE 3 .| sinkList
        in res `shouldBe` [[1,2,3], [4,5,6]]
    it "chunksOfE 2 (last smaller)" $
        let res = runConduitPure $ yieldMany [[1,2], [3,4], [5,6,7]] .| chunksOfE 3 .| sinkList
        in res `shouldBe` [[1,2,3], [4,5,6], [7]]
    it "chunksOfE (ByteString)" $
        let res = runConduitPure $ yieldMany [S8.pack "01234", "56789ab", "cdef", "h"] .| chunksOfE 4 .| sinkList
        in res `shouldBe` ["0123", "4567", "89ab", "cdef", "h"]
    it "chunksOfExactlyE 1" $
        let res = runConduitPure $ yieldMany [[1,2], [3,4], [5,6]] .| chunksOfExactlyE 3 .| sinkList
        in res `shouldBe` [[1,2,3], [4,5,6]]
    it "chunksOfExactlyE 2 (last smaller; thus not yielded)" $
        let res = runConduitPure $ yieldMany [[1,2], [3,4], [5,6,7]] .| chunksOfExactlyE 3 .| sinkList
        in res `shouldBe` [[1,2,3], [4,5,6]]
    prop "vectorBuilder" $ \(values :: [[Int]]) ((+1) . (`mod` 30) . abs -> size) -> do
        let res = runST $ runConduit
                $ yieldMany values
               .| vectorBuilderC size mapM_CE
               .| sinkList
            expected =
                loop $ concat values
              where
                loop [] = []
                loop x =
                    VU.fromList y : loop z
                  where
                    (y, z) = splitAt size x
        res `shouldBe` expected
    prop "mapAccumS" $ \input ->
        let ints  = [1..]
            f a s = liftM (:s) $ mapC (* a) .| takeC a .| sinkList
            res   = reverse $ runConduitPure $ yieldMany input
                           .| mapAccumS f [] (yieldMany ints)
            expected = loop input ints
                where  loop []     _  = []
                       loop (a:as) xs = let (y, ys) = Prelude.splitAt a xs
                                        in  map (* a) y : loop as ys
        in  res `shouldBe` expected
    prop "peekForever" $ \(strs' :: [String]) -> do
        let strs = filter (not . null) strs'
        res1 <- runConduit $ yieldMany strs .| linesUnboundedC .| sinkList
        res2 <- runConduit $ yieldMany strs .| peekForever (lineC $ foldC >>= yield) .| sinkList
        res2 `shouldBe` res1
    prop "peekForeverE" $ \(strs :: [String]) -> do
        res1 <- runConduit $ yieldMany strs .| linesUnboundedC .| sinkList
        res2 <- runConduit $ yieldMany strs .| peekForeverE (lineC $ foldC >>= yield) .| sinkList
        res2 `shouldBe` res1
    describe "concurrentMap" $ do
        let boom = stringException "boom"
        let isBoom e = maybe False (== boom) $ fromException e
        let isLinkedBoom e = case fromException e of
                Nothing -> False
                Just (ExceptionInLinkedThread _ inner) -> isBoom inner
        let block = newEmptyMVar >>= takeMVar :: IO ()
        it "propagates exceptions" $ do
            let go = runConduitRes $ yield () .| concurrentMap 10 (\_ -> throwM boom) .| sinkNull
            go `shouldThrow` isLinkedBoom
        it "rejects invalid concurrency" $ do
            let go = runConduitRes $ yield () .| concurrentMap 0 return .| sinkNull
            go `shouldThrow` (== InvalidConcurrencyLimitException)
        it "uses concurrency" $ do
            ready <- newEmptyMVar
            let source = yield (takeMVar ready) >> yield (putMVar ready ())
            runConduitRes $ source .| concurrentMap 10 id .| sinkNull
            return () :: IO ()
        it "kills all workers upon an exception in any worker" $ do
            ready <- newEmptyMVar
            killed <- newEmptyMVar
            let source = do
                    yield $ (putMVar ready () >> block) `catchAny` (\_ -> putMVar killed ())
                    liftIO $ takeMVar ready
                    yield $ throwM boom
            let go = runConduitRes $ source .| concurrentMap 10 id .| sinkNull
            go `shouldThrow` isLinkedBoom
            takeMVar killed
        it "kills all workers upon an exception in the conduit" $ do
            ready <- newEmptyMVar
            killed <- newEmptyMVar
            let source = do
                    yield $ (putMVar ready () >> block) `catchAny` (\_ -> putMVar killed ())
                    liftIO $ takeMVar ready
                    throwM boom
            let go = runConduitRes $ source .| concurrentMap 10 id .| sinkNull
            go `shouldThrow` isBoom
            takeMVar killed
    StreamSpec.spec

evenInt :: Int -> Bool
evenInt = even

elemInt :: Int -> [Int] -> Bool
elemInt = elem

notElemInt :: Int -> [Int] -> Bool
notElemInt = notElem

addM :: Monad m => Int -> Int -> m Int
addM x y = return (x + y)

succChar :: Char -> Char
succChar c =
  case pureTry (succ c) of
    Left _ -> 'X' -- QuickCheck may generate characters out of range
    Right x -> x

showInt :: Int -> String
showInt = Prelude.show

nocrBL :: L8.ByteString -> L8.ByteString
nocrBL = L8.filter (/= '\r')
