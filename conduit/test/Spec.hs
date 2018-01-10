{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Conduit
import Prelude hiding (FilePath)
import Data.Maybe (listToMaybe)
import Data.Conduit.Combinators (slidingWindow, chunksOfE, chunksOfExactlyE)
import Data.List (intersperse, sort, find, mapAccumL)
import Safe (tailSafe)
import System.FilePath (takeExtension)
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
import System.FilePath ((</>))
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
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified StreamSpec

main :: IO ()
main = hspec $ do
    describe "yieldMany" $ do
        it "list" $
            runIdentity (yieldMany [1..10] $$ sinkList)
            `shouldBe` [1..10]
        it "Text" $
            runIdentity (yieldMany ("Hello World" :: T.Text) $$ sinkList)
            `shouldBe` "Hello World"
    it "unfold" $
        let f 11 = Nothing
            f i = Just (show i, i + 1)
         in runIdentity (unfoldC f 1 $$ sinkList)
            `shouldBe` map show [1..10]
    it "enumFromTo" $
        runIdentity (enumFromToC 1 10 $$ sinkList) `shouldBe` [1..10]
    it "iterate" $
        let f i = i + 1
            src = iterateC f seed
            seed = 1
            count = 10
            res = runIdentity $ src $$ takeC count =$ sinkList
         in res `shouldBe` take count (iterate f seed)
    it "repeat" $
        let src = repeatC seed
            seed = 1
            count = 10
            res = runIdentity $ src $$ takeC count =$ sinkList
         in res `shouldBe` take count (repeat seed)
    it "replicate" $
        let src = replicateC count seed
            seed = 1
            count = 10
            res = runIdentity $ src $$ sinkList
         in res `shouldBe` replicate count seed
    it "sourceLazy" $
        let tss = ["foo", "bar", "baz"]
            tl = TL.fromChunks tss
            res = runIdentity $ sourceLazy tl $$ sinkList
         in res `shouldBe` tss
    it "repeatM" $
        let src = repeatMC (return seed)
            seed = 1
            count = 10
            res = runIdentity $ src $$ takeC count =$ sinkList
         in res `shouldBe` take count (repeat seed)
    it "repeatWhileM" $ do
        ref <- newIORef 0
        let f = atomicModifyIORef ref $ \i -> (succ i, succ i)
            src = repeatWhileMC f (< 11)
        res <- src $$ sinkList
        res `shouldBe` [1..10]
    it "replicateM" $ do
        ref <- newIORef 0
        let f = atomicModifyIORef ref $ \i -> (succ i, succ i)
            src = replicateMC 10 f
        res <- src $$ sinkList
        res `shouldBe` [1..10]
    it "sourceFile" $ do
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        res <- runResourceT $ sourceFile fp $$ sinkLazy
        nocrBL res `shouldBe` TL.encodeUtf8 (TL.pack contents)
    it "sourceHandle" $ do
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        res <- IO.withBinaryFile "tmp" IO.ReadMode $ \h -> sourceHandle h $$ sinkLazy
        nocrBL res `shouldBe` TL.encodeUtf8 (TL.pack contents)
    it "sourceIOHandle" $ do
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        let open = IO.openBinaryFile "tmp" IO.ReadMode
        res <- runResourceT $ sourceIOHandle open $$ sinkLazy
        nocrBL res `shouldBe` TL.encodeUtf8 (TL.pack contents)
    prop "stdin" $ \(S.pack -> content) -> do
        S.writeFile "tmp" content
        IO.withBinaryFile "tmp" IO.ReadMode $ \h -> do
            hDuplicateTo h IO.stdin
            x <- stdinC $$ foldC
            x `shouldBe` content
    let hasExtension' ext fp = takeExtension fp == ext
    it "sourceDirectory" $ do
        res <- runResourceT
             $ sourceDirectory "test" $$ filterC (not . hasExtension' ".swp") =$ sinkList
        sort res `shouldBe`
          [ "test/Data"
          , "test/Spec.hs"
          , "test/StreamSpec.hs"
          , "test/doctests.hs"
          , "test/main.hs"
          , "test/subdir"
          ]
    it "sourceDirectoryDeep" $ do
        res1 <- runResourceT
              $ sourceDirectoryDeep False "test" $$ filterC (not . hasExtension' ".swp") =$ sinkList
        res2 <- runResourceT
              $ sourceDirectoryDeep True "test" $$ filterC (not . hasExtension' ".swp") =$ sinkList
        sort res1 `shouldBe`
          [ "test/Data/Conduit/Extra/ZipConduitSpec.hs"
          , "test/Data/Conduit/StreamSpec.hs"
          , "test/Spec.hs"
          , "test/StreamSpec.hs"
          , "test/doctests.hs"
          , "test/main.hs"
          , "test/subdir/dummyfile.txt"
          ]
        sort res1 `shouldBe` sort res2
    prop "drop" $ \(T.pack -> input) count ->
        runIdentity (yieldMany input $$ (dropC count >>= \() -> sinkList))
        `shouldBe` T.unpack (T.drop count input)
    prop "dropE" $ \(T.pack -> input) ->
        runIdentity (yield input $$ (dropCE 5 >>= \() -> foldC))
        `shouldBe` T.drop 5 input
    prop "dropWhile" $ \(T.pack -> input) sep ->
        runIdentity (yieldMany input $$ (dropWhileC (<= sep) >>= \() -> sinkList))
        `shouldBe` T.unpack (T.dropWhile (<= sep) input)
    prop "dropWhileE" $ \(T.pack -> input) sep ->
        runIdentity (yield input $$ (dropWhileCE (<= sep) >>= \() -> foldC))
        `shouldBe` T.dropWhile (<= sep) input
    it "fold" $
        let list = [[1..10], [11..20]]
            src = yieldMany list
            res = runIdentity $ src $$ foldC
         in res `shouldBe` concat list
    it "foldE" $
        let list = [[1..10], [11..20]]
            src = yieldMany $ Identity list
            res = runIdentity $ src $$ foldCE
         in res `shouldBe` concat list
    it "foldl" $
        let res = runIdentity $ yieldMany [1..10] $$ foldlC (+) 0
         in res `shouldBe` sum [1..10]
    it "foldlE" $
        let res = runIdentity $ yield [1..10] $$ foldlCE (+) 0
         in res `shouldBe` sum [1..10]
    it "foldMap" $
        let src = yieldMany [1..10]
            res = runIdentity $ src $$ foldMapC return
         in res `shouldBe` [1..10]
    it "foldMapE" $
        let src = yield [1..10]
            res = runIdentity $ src $$ foldMapCE return
         in res `shouldBe` [1..10]
    prop "all" $ \ (input :: [Int]) -> runIdentity (yieldMany input $$ allC even) `shouldBe` all evenInt input
    prop "allE" $ \ (input :: [Int]) -> runIdentity (yield input $$ allCE even) `shouldBe` all evenInt input
    prop "any" $ \ (input :: [Int]) -> runIdentity (yieldMany input $$ anyC even) `shouldBe` any evenInt input
    prop "anyE" $ \ (input :: [Int]) -> runIdentity (yield input $$ anyCE even) `shouldBe` any evenInt input
    prop "and" $ \ (input :: [Bool]) -> runIdentity (yieldMany input $$ andC) `shouldBe` and input
    prop "andE" $ \ (input :: [Bool]) -> runIdentity (yield input $$ andCE) `shouldBe` and input
    prop "or" $ \ (input :: [Bool]) -> runIdentity (yieldMany input $$ orC) `shouldBe` or input
    prop "orE" $ \ (input :: [Bool]) -> runIdentity (yield input $$ orCE) `shouldBe` or input
    prop "elem" $ \x xs -> runIdentity (yieldMany xs $$ elemC x) `shouldBe` elemInt x xs
    prop "elemE" $ \x xs -> runIdentity (yield xs $$ elemCE x) `shouldBe` elemInt x xs
    prop "notElem" $ \x xs -> runIdentity (yieldMany xs $$ notElemC x) `shouldBe` notElemInt x xs
    prop "notElemE" $ \x xs -> runIdentity (yield xs $$ notElemCE x) `shouldBe` notElemInt x xs
    prop "sinkVector regular" $ \xs -> do
        res <- yieldMany xs $$ sinkVector
        res `shouldBe` V.fromList (xs :: [Int])
    prop "sinkVector unboxed" $ \xs -> do
        res <- yieldMany xs $$ sinkVector
        res `shouldBe` VU.fromList (xs :: [Int])
    prop "sinkVector storable" $ \xs -> do
        res <- yieldMany xs $$ sinkVector
        res `shouldBe` VS.fromList (xs :: [Int])
    prop "sinkVectorN regular" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- yieldMany xs' $$ sinkVectorN maxSize
        res `shouldBe` V.fromList (xs :: [Int])
    prop "sinkVectorN unboxed" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- yieldMany xs' $$ sinkVectorN maxSize
        res `shouldBe` VU.fromList (xs :: [Int])
    prop "sinkVectorN storable" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- yieldMany xs' $$ sinkVectorN maxSize
        res `shouldBe` VS.fromList (xs :: [Int])
    prop "sinkBuilder" $ \(map S.pack -> inputs) ->
        let builder = runConduitPure $ yieldMany inputs .| foldMapC byteString
            ltext = toLazyByteString builder
         in ltext `shouldBe` fromChunks inputs
    prop "sinkLazyBuilder" $ \(map S.pack -> inputs) ->
        let lbs = runIdentity (yieldMany (map byteString inputs) $$ sinkLazyBuilder)
         in lbs `shouldBe` fromChunks inputs
    prop "sinkNull" $ \xs toSkip -> do
        res <- yieldMany xs $$ do
            takeC toSkip =$ sinkNull
            sinkList
        res `shouldBe` drop toSkip (xs :: [Int])
    prop "awaitNonNull" $ \xs ->
        fmap NN.toNullable (runIdentity $ yieldMany xs $$ awaitNonNull)
        `shouldBe` listToMaybe (filter (not . null) (xs :: [[Int]]))
    prop "headE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ ((,) <$> headCE <*> foldC))
        `shouldBe` (listToMaybe $ concat xs, drop 1 $ concat xs)
    prop "peek" $ \xs ->
        runIdentity (yieldMany xs $$ ((,) <$> peekC <*> sinkList))
        `shouldBe` (listToMaybe xs, xs :: [Int])
    prop "peekE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ ((,) <$> peekCE <*> foldC))
        `shouldBe` (listToMaybe $ concat xs, concat xs)
    prop "last" $ \xs ->
        runIdentity (yieldMany xs $$ lastC)
        `shouldBe` listToMaybe (reverse (xs :: [Int]))
    prop "lastE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ lastCE)
        `shouldBe` listToMaybe (reverse (concat xs))
    prop "length" $ \xs ->
        runIdentity (yieldMany xs $$ lengthC)
        `shouldBe` length (xs :: [Int])
    prop "lengthE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ lengthCE)
        `shouldBe` length (concat xs)
    prop "lengthIf" $ \x xs ->
        runIdentity (yieldMany xs $$ lengthIfC (< x))
        `shouldBe` length (filter (< x) xs :: [Int])
    prop "lengthIfE" $ \x (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ lengthIfCE (< x))
        `shouldBe` length (filter (< x) (concat xs))
    prop "maximum" $ \xs ->
        runIdentity (yieldMany xs $$ maximumC)
        `shouldBe` (if null (xs :: [Int]) then Nothing else Just (maximum xs))
    prop "maximumE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ maximumCE)
        `shouldBe` (if null (concat xs) then Nothing else Just (maximum $ concat xs))
    prop "minimum" $ \xs ->
        runIdentity (yieldMany xs $$ minimumC)
        `shouldBe` (if null (xs :: [Int]) then Nothing else Just (minimum xs))
    prop "minimumE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ minimumCE)
        `shouldBe` (if null (concat xs) then Nothing else Just (minimum $ concat xs))
    prop "null" $ \xs ->
        runIdentity (yieldMany xs $$ nullC)
        `shouldBe` null (xs :: [Int])
    prop "nullE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ ((,) <$> nullCE <*> foldC))
        `shouldBe` (null (concat xs), concat xs)
    prop "sum" $ \xs ->
        runIdentity (yieldMany xs $$ sumC)
        `shouldBe` sum (xs :: [Int])
    prop "sumE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ sumCE)
        `shouldBe` sum (concat xs)
    prop "product" $ \xs ->
        runIdentity (yieldMany xs $$ productC)
        `shouldBe` product (xs :: [Int])
    prop "productE" $ \ (xs :: [[Int]]) ->
        runIdentity (yieldMany xs $$ productCE)
        `shouldBe` product (concat xs)
    prop "find" $ \x xs ->
        runIdentity (yieldMany xs $$ findC (< x))
        `shouldBe` find (< x) (xs :: [Int])
    prop "mapM_" $ \xs ->
        let res = execWriter $ yieldMany xs $$ mapM_C (tell . return)
         in res `shouldBe` (xs :: [Int])
    prop "mapM_E" $ \xs ->
        let res = execWriter $ yield xs $$ mapM_CE (tell . return)
         in res `shouldBe` (xs :: [Int])
    prop "foldM" $ \ (xs :: [Int]) -> do
        res <- yieldMany xs $$ foldMC addM 0
        res `shouldBe` sum xs
    prop "foldME" $ \ (xs :: [Int]) -> do
        res <- yield xs $$ foldMCE addM 0
        res `shouldBe` sum xs
    it "foldMapM" $
        let src = yieldMany [1..10]
            res = runIdentity $ src $$ foldMapMC (return . return)
         in res `shouldBe` [1..10]
    it "foldMapME" $
        let src = yield [1..10]
            res = runIdentity $ src $$ foldMapMCE (return . return)
         in res `shouldBe` [1..10]
    it "sinkFile" $ do
        let contents = mconcat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
        runResourceT $ yield contents $$ sinkFile fp
        res <- S.readFile fp
        res `shouldBe` contents
    it "sinkHandle" $ do
        let contents = mconcat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
        IO.withBinaryFile "tmp" IO.WriteMode $ \h -> yield contents $$ sinkHandle h
        res <- S.readFile fp
        res `shouldBe` contents
    it "sinkIOHandle" $ do
        let contents = mconcat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
            open = IO.openBinaryFile "tmp" IO.WriteMode
        runResourceT $ yield contents $$ sinkIOHandle open
        res <- S.readFile fp
        res `shouldBe` contents
    prop "print" $ \vals -> do
        let expected = Prelude.unlines $ map showInt vals
        (actual, ()) <- hCapture [IO.stdout] $ yieldMany vals $$ printC
        actual `shouldBe` expected
#ifndef WINDOWS
    prop "stdout" $ \ (vals :: [String]) -> do
        let expected = concat vals
        (actual, ()) <- hCapture [IO.stdout] $ yieldMany (map T.pack vals) $$ encodeUtf8C =$ stdoutC
        actual `shouldBe` expected
    prop "stderr" $ \ (vals :: [String]) -> do
        let expected = concat vals
        (actual, ()) <- hCapture [IO.stderr] $ yieldMany (map T.pack vals) $$ encodeUtf8C =$ stderrC
        actual `shouldBe` expected
#endif
    prop "map" $ \input ->
        runIdentity (yieldMany input $$ mapC succChar =$ sinkList)
        `shouldBe` map succChar input
    prop "mapE" $ \(map V.fromList -> inputs) ->
        runIdentity (yieldMany inputs $$ mapCE succChar =$ foldC)
        `shouldBe` V.map succChar (V.concat inputs)
    prop "omapE" $ \(map T.pack -> inputs) ->
        runIdentity (yieldMany inputs $$ omapCE succChar =$ foldC)
        `shouldBe` T.map succChar (T.concat inputs)
    prop "concatMap" $ \ (input :: [Int]) ->
        runIdentity (yieldMany input $$ concatMapC showInt =$ sinkList)
        `shouldBe` concatMap showInt input
    prop "concatMapE" $ \ (input :: [Int]) ->
        runIdentity (yield input $$ concatMapCE showInt =$ foldC)
        `shouldBe` concatMap showInt input
    prop "take" $ \(T.pack -> input) count ->
        runIdentity (yieldMany input $$ (takeC count >>= \() -> mempty) =$ sinkList)
        `shouldBe` T.unpack (T.take count input)
    prop "takeE" $ \(T.pack -> input) count ->
        runIdentity (yield input $$ (takeCE count >>= \() -> mempty) =$ foldC)
        `shouldBe` T.take count input
    prop "takeWhile" $ \(T.pack -> input) sep ->
        runIdentity (yieldMany input $$ do
            x <- (takeWhileC (<= sep) >>= \() -> mempty) =$ sinkList
            y <- sinkList
            return (x, y))
        `shouldBe` span (<= sep) (T.unpack input)
    prop "takeWhileE" $ \(T.pack -> input) sep ->
        runIdentity (yield input $$ do
            x <- (takeWhileCE (<= sep) >>= \() -> mempty) =$ foldC
            y <- foldC
            return (x, y))
        `shouldBe` T.span (<= sep) input
    it "takeExactly" $
        let src = yieldMany [1..10]
            sink = do
                x <- takeExactlyC 5 $ return 1
                y <- sinkList
                return (x, y)
            res = runIdentity $ src $$ sink
         in res `shouldBe` (1, [6..10])
    it "takeExactlyE" $
        let src = yield ("Hello World" :: T.Text)
            sink = do
                takeExactlyCE 5 (mempty :: Sink T.Text Identity ())
                y <- sinkLazy
                return y
            res = runIdentity $ src $$ sink
         in res `shouldBe` " World"
    it "takeExactlyE Vector" $ do
        let src = yield (V.fromList $ T.unpack "Hello World")
            sink = do
                x <- takeExactlyCE 5 $ return 1
                y <- foldC
                return (x, y)
        res <- src $$ sink
        res `shouldBe` (1, V.fromList $ T.unpack " World")
    it "takeExactlyE 2" $
        let src = yield ("Hello World" :: T.Text)
            sink = do
                x <- takeExactlyCE 5 $ return 1
                y <- sinkLazy
                return (x, y)
            res = runIdentity $ src $$ sink
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
        runIdentity (yield (T.pack input) $$ concatC =$ sinkList)
        `shouldBe` input
    prop "filter" $ \input ->
        runIdentity (yieldMany input $$ filterC evenInt =$ sinkList)
        `shouldBe` filter evenInt input
    prop "filterE" $ \input ->
        runIdentity (yield input $$ filterCE evenInt =$ foldC)
        `shouldBe` filter evenInt input
    prop "mapWhile" $ \input (min 20 -> highest) ->
        let f i | i < highest = Just (i + 2 :: Int)
                | otherwise   = Nothing
            res = runIdentity $ yieldMany input $$ do
                x <- (mapWhileC f >>= \() -> mempty) =$ sinkList
                y <- sinkList
                return (x, y)
            (taken, dropped) = span (< highest) input
         in res `shouldBe` (map (+ 2) taken, dropped)
    prop "conduitVector" $ \(take 200 -> input) size' -> do
        let size = min 30 $ succ $ abs size'
        res <- yieldMany input $$ conduitVector size =$ sinkList
        res `shouldSatisfy` all (\v -> V.length v <= size)
        drop 1 (reverse res) `shouldSatisfy` all (\v -> V.length v == size)
        V.concat res `shouldBe` V.fromList (input :: [Int])
    prop "scanl" $ \input seed ->
        let f a b = a + b :: Int
            res = runIdentity $ yieldMany input $$ scanlC f seed =$ sinkList
         in res `shouldBe` scanl f seed input
    prop "mapAccumWhile" $ \input (min 20 -> highest) ->
        let f i accum | i < highest = Right (i + accum, 2 * i :: Int)
                      | otherwise   = Left accum
            res = runIdentity $ yieldMany input $$ do
                (s, x) <- fuseBoth (mapAccumWhileC f 0) sinkList
                y <- sinkList
                return (s, x, y)
            (taken, dropped) = span (< highest) input
         in res `shouldBe` (sum taken, map (* 2) taken, tailSafe dropped)
    prop "concatMapAccum" $ \(input :: [Int]) ->
        let f a accum = (a + accum, [a, accum])
            res = runIdentity $ yieldMany input $$ concatMapAccumC f 0 =$ sinkList
            expected = concat $ snd $ mapAccumL (flip f) 0 input
         in res `shouldBe` expected
    prop "intersperse" $ \xs x ->
        runIdentity (yieldMany xs $$ intersperseC x =$ sinkList)
        `shouldBe` intersperse (x :: Int) xs
    prop "mapM" $ \input ->
        runIdentity (yieldMany input $$ mapMC (return . succChar) =$ sinkList)
        `shouldBe` map succChar input
    prop "mapME" $ \(map V.fromList -> inputs) ->
        runIdentity (yieldMany inputs $$ mapMCE (return . succChar) =$ foldC)
        `shouldBe` V.map succChar (V.concat inputs)
    prop "omapME" $ \(map T.pack -> inputs) ->
        runIdentity (yieldMany inputs $$ omapMCE (return . succChar) =$ foldC)
        `shouldBe` T.map succChar (T.concat inputs)
    prop "concatMapM" $ \ (input :: [Int]) ->
        runIdentity (yieldMany input $$ concatMapMC (return . showInt) =$ sinkList)
        `shouldBe` concatMap showInt input
    prop "filterM" $ \input ->
        runIdentity (yieldMany input $$ filterMC (return . evenInt) =$ sinkList)
        `shouldBe` filter evenInt input
    prop "filterME" $ \input ->
        runIdentity (yield input $$ filterMCE (return . evenInt) =$ foldC)
        `shouldBe` filter evenInt input
    prop "iterM" $ \input -> do
        (x, y) <- runWriterT $ yieldMany input $$ iterMC (tell . return) =$ sinkList
        x `shouldBe` (input :: [Int])
        y `shouldBe` input
    prop "scanlM" $ \input seed ->
        let f a b = a + b :: Int
            fm a b = return $ a + b
            res = runIdentity $ yieldMany input $$ scanlMC fm seed =$ sinkList
         in res `shouldBe` scanl f seed input
    prop "mapAccumWhileM" $ \input (min 20 -> highest) ->
        let f i accum | i < highest = Right (i + accum, 2 * i :: Int)
                      | otherwise   = Left accum
            res = runIdentity $ yieldMany input $$ do
                (s, x) <- fuseBoth (mapAccumWhileMC ((return.).f) 0) sinkList
                y <- sinkList
                return (s, x, y)
            (taken, dropped) = span (< highest) input
         in res `shouldBe` (sum taken, map (* 2) taken, tailSafe dropped)
    prop "concatMapAccumM" $ \(input :: [Int]) ->
        let f a accum = (a + accum, [a, accum])
            res = runIdentity $ yieldMany input $$ concatMapAccumMC ((return.).f) 0 =$ sinkList
            expected = concat $ snd $ mapAccumL (flip f) 0 input
         in res `shouldBe` expected
    prop "encode UTF8" $ \(map T.pack -> inputs) -> do
        let expected = encodeUtf8 $ fromChunks inputs
        actual <- yieldMany inputs
               $$ encodeUtf8C
               =$ sinkLazy
        actual `shouldBe` expected
    prop "encode/decode UTF8" $ \(map T.pack -> inputs) (min 50 . max 1 . abs -> chunkSize) -> do
        let expected = fromChunks inputs
        actual <- yieldMany inputs
               $$ encodeUtf8C
               =$ concatC
               =$ conduitVector chunkSize
               =$ mapC (S.pack . V.toList)
               =$ decodeUtf8C
               =$ sinkLazy
        actual `shouldBe` expected
    it "invalid UTF8 is an exception" $
      case runConduit $ yield "\129" .| decodeUtf8C .| sinkLazy of
        Left _ -> return () :: IO ()
        Right x -> error $ "this should have failed, got: " ++ show x
    prop "encode/decode UTF8 lenient" $ \(map T.pack -> inputs) (min 50 . max 1 . abs -> chunkSize) -> do
        let expected = fromChunks inputs
        actual <- yieldMany inputs
               $$ encodeUtf8C
               =$ concatC
               =$ conduitVector chunkSize
               =$ mapC (S.pack . V.toList)
               =$ decodeUtf8LenientC
               =$ sinkLazy
        actual `shouldBe` expected
    prop "line" $ \(map T.pack -> input) size ->
        let src = yieldMany input
            sink = do
                x <- lineC $ takeCE size =$ foldC
                y <- foldC
                return (x, y)
            res = runIdentity $ src $$ sink
            expected =
                let (x, y) = T.break (== '\n') (T.concat input)
                 in (T.take size x, T.drop 1 y)
         in res `shouldBe` expected
    prop "lineAscii" $ \(map S.pack -> input) size ->
        let src = yieldMany input
            sink = do
                x <- lineAsciiC $ takeCE size =$ foldC
                y <- foldC
                return (x, y)
            res = runIdentity $ src $$ sink
            expected =
                let (x, y) = S.break (== 10) (S.concat input)
                 in (S.take size x, S.drop 1 y)
         in res `shouldBe` expected
    prop "unlines" $ \(map T.pack -> input) ->
        runIdentity (yieldMany input $$ unlinesC =$ foldC)
        `shouldBe` T.unlines input
    prop "unlinesAscii" $ \(map S.pack -> input) ->
        runIdentity (yieldMany input $$ unlinesAsciiC =$ foldC)
        `shouldBe` S8.unlines input
    prop "linesUnbounded" $ \(map T.pack -> input) ->
        runIdentity (yieldMany input $$ (linesUnboundedC >>= \() -> mempty) =$ sinkList)
        `shouldBe` T.lines (T.concat input)
    prop "linesUnboundedAscii" $ \(map S.pack -> input) ->
        runIdentity (yieldMany input $$ (linesUnboundedAsciiC >>= \() -> mempty) =$ sinkList)
        `shouldBe` S8.lines (S.concat input)
    it "slidingWindow 0" $
        let res = runIdentity $ yieldMany [1..5] $= slidingWindow 0 $$ sinkList
        in res `shouldBe` [[1],[2],[3],[4],[5]]
    it "slidingWindow 1" $
        let res = runIdentity $ yieldMany [1..5] $= slidingWindow 1 $$ sinkList
        in res `shouldBe` [[1],[2],[3],[4],[5]]
    it "slidingWindow 2" $
        let res = runIdentity $ yieldMany [1..5] $= slidingWindow 2 $$ sinkList
        in res `shouldBe` [[1,2],[2,3],[3,4],[4,5]]
    it "slidingWindow 3" $
        let res = runIdentity $ yieldMany [1..5] $= slidingWindow 3 $$ sinkList
        in res `shouldBe` [[1,2,3],[2,3,4],[3,4,5]]
    it "slidingWindow 4" $
        let res = runIdentity $ yieldMany [1..5] $= slidingWindow 4 $$ sinkList
        in res `shouldBe` [[1,2,3,4],[2,3,4,5]]
    it "slidingWindow 5" $
        let res = runIdentity $ yieldMany [1..5] $= slidingWindow 5 $$ sinkList
        in res `shouldBe` [[1,2,3,4,5]]
    it "slidingWindow 6" $
        let res = runIdentity $ yieldMany [1..5] $= slidingWindow 6 $$ sinkList
        in res `shouldBe` [[1,2,3,4,5]]
    it "chunksOfE 1" $
        let res = runIdentity $ yieldMany [[1,2], [3,4], [5,6]] $= chunksOfE 3 $$ sinkList
        in res `shouldBe` [[1,2,3], [4,5,6]]
    it "chunksOfE 2 (last smaller)" $
        let res = runIdentity $ yieldMany [[1,2], [3,4], [5,6,7]] $= chunksOfE 3 $$ sinkList
        in res `shouldBe` [[1,2,3], [4,5,6], [7]]
    it "chunksOfE (ByteString)" $
        let res = runIdentity $ yieldMany [S8.pack "01234", "56789ab", "cdef", "h"] $= chunksOfE 4 $$ sinkList
        in res `shouldBe` ["0123", "4567", "89ab", "cdef", "h"]
    it "chunksOfExactlyE 1" $
        let res = runIdentity $ yieldMany [[1,2], [3,4], [5,6]] $= chunksOfExactlyE 3 $$ sinkList
        in res `shouldBe` [[1,2,3], [4,5,6]]
    it "chunksOfExactlyE 2 (last smaller; thus not yielded)" $
        let res = runIdentity $ yieldMany [[1,2], [3,4], [5,6,7]] $= chunksOfExactlyE 3 $$ sinkList
        in res `shouldBe` [[1,2,3], [4,5,6]]
    prop "vectorBuilder" $ \(values :: [[Int]]) ((+1) . (`mod` 30) . abs -> size) -> do
        let res = runST
                $ yieldMany values
               $$ vectorBuilderC size mapM_CE
               =$ sinkList
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
            f a s = liftM (:s) $ mapC (* a) =$ takeC a =$ sinkList
            res   = reverse $ runIdentity $ yieldMany input
                           $$ mapAccumS f [] (yieldMany ints)
            expected = loop input ints
                where  loop []     _  = []
                       loop (a:as) xs = let (y, ys) = Prelude.splitAt a xs
                                        in  map (* a) y : loop as ys
        in  res `shouldBe` expected
    prop "peekForever" $ \(strs' :: [String]) -> do
        let strs = filter (not . null) strs'
        res1 <- yieldMany strs $$ linesUnboundedC =$ sinkList
        res2 <- yieldMany strs $$ peekForever (lineC $ foldC >>= yield) =$ sinkList
        res2 `shouldBe` res1
    prop "peekForeverE" $ \(strs :: [String]) -> do
        res1 <- yieldMany strs $$ linesUnboundedC =$ sinkList
        res2 <- yieldMany strs $$ peekForeverE (lineC $ foldC >>= yield) =$ sinkList
        res2 `shouldBe` res1
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
succChar = succ

showInt :: Int -> String
showInt = Prelude.show

nocrBL :: L8.ByteString -> L8.ByteString
nocrBL = L8.filter (/= '\r')
