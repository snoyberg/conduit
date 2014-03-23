{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.BinarySpec (spec) where

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Exception (IOException)
import qualified Data.ByteString.Lazy as L
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.IORef as I
import Control.Monad.Trans.Writer.Strict
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Functor.Identity

spec :: Spec
spec = describe "Data.Conduit.Binary" $ do

    describe "file access" $ do
        it "read" $ do
            bs <- S.readFile "conduit-extra.cabal"
            bss <- runResourceT $ CB.sourceFile "conduit-extra.cabal" C.$$ CL.consume
            S.concat bss `shouldBe` bs

        it "read range" $ do
            S.writeFile "tmp" "0123456789"
            bss <- runResourceT $ CB.sourceFileRange "tmp" (Just 2) (Just 3) C.$$ CL.consume
            S.concat bss `shouldBe` "234"

        it "write" $ do
            runResourceT $ CB.sourceFile "conduit-extra.cabal" C.$$ CB.sinkFile "tmp"
            bs1 <- S.readFile "conduit-extra.cabal"
            bs2 <- S.readFile "tmp"
            bs2 `shouldBe` bs1

        it "conduit" $ do
            runResourceT $ CB.sourceFile "conduit-extra.cabal"
                C.$= CB.conduitFile "tmp"
                C.$$ CB.sinkFile "tmp2"
            bs1 <- S.readFile "conduit-extra.cabal"
            bs2 <- S.readFile "tmp"
            bs3 <- S.readFile "tmp2"
            bs2 `shouldBe` bs1
            bs3 `shouldBe` bs1
    describe "binary isolate" $ do
        it "works" $ do
            bss <- runResourceT $ CL.sourceList (replicate 1000 "X")
                           C.$= CB.isolate 6
                           C.$$ CL.consume
            S.concat bss `shouldBe` "XXXXXX"

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

    describe "binary" $ do
        prop "lines" $ \bss' -> runIdentity $ do
            let bss = map S.pack bss'
                bs = S.concat bss
                src = CL.sourceList bss
            res <- src C.$$ CB.lines C.=$ CL.consume
            return $ S8.lines bs == res

    describe "sinkCacheLength" $ do
        it' "works" $ runResourceT $ do
            lbs <- liftIO $ L.readFile "test/Data/Conduit/BinarySpec.hs"
            (len, src) <- CB.sourceLbs lbs C.$$ CB.sinkCacheLength
            lbs' <- src C.$$ CB.sinkLbs
            liftIO $ do
                fromIntegral len `shouldBe` L.length lbs
                lbs' `shouldBe` lbs
                fromIntegral len `shouldBe` L.length lbs'

    describe "Data.Conduit.Binary.mapM_" $ do
        prop "telling works" $ \bytes ->
            let lbs = L.pack bytes
                src = CB.sourceLbs lbs
                sink = CB.mapM_ (tell . return . S.singleton)
                bss = execWriter $ src C.$$ sink
             in L.fromChunks bss == lbs

    describe "exception handling" $ do
        it "catchC" $ do
            ref <- I.newIORef 0
            let src = do
                    C.catchC (CB.sourceFile "some-file-that-does-not-exist") onErr
                    C.handleC onErr $ CB.sourceFile "conduit-extra.cabal"
                onErr :: MonadIO m => IOException -> m ()
                onErr _ = liftIO $ I.modifyIORef ref (+ 1)
            contents <- L.readFile "conduit-extra.cabal"
            res <- runResourceT $ src C.$$ CB.sinkLbs
            res `shouldBe` contents
            errCount <- I.readIORef ref
            errCount `shouldBe` (1 :: Int)
        it "tryC" $ do
            ref <- I.newIORef undefined
            let src = do
                    res1 <- C.tryC $ CB.sourceFile "some-file-that-does-not-exist"
                    res2 <- C.tryC $ CB.sourceFile "conduit-extra.cabal"
                    liftIO $ I.writeIORef ref (res1, res2)
            contents <- L.readFile "conduit-extra.cabal"
            res <- runResourceT $ src C.$$ CB.sinkLbs
            res `shouldBe` contents
            exc <- I.readIORef ref
            case exc :: (Either IOException (), Either IOException ()) of
                (Left _, Right ()) ->
                    return ()
                _ -> error $ show exc

    describe "normalFuseLeft" $ do
        it "does not double close conduit" $ do
            x <- runResourceT $ do
                let src = CL.sourceList ["foobarbazbin"]
                src C.$= CB.isolate 10 C.$$ CL.head
            x `shouldBe` Just "foobarbazb"

it' :: String -> IO () -> Spec
it' = it
