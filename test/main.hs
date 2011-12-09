{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource (runResourceT)
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import qualified Data.ByteString as S

main :: IO ()
main = hspecX $ do
    describe "sum" $ do
        prop "is idempotent" $ \list ->
            (unsafePerformIO $ runResourceT $ CL.fromList list C.$$ CL.fold (+) (0 :: Int))
            == sum list
    describe "Monoid instance for Source" $ do
        it "mappend" $ do
            x <- runResourceT $ (CL.fromList [1..5 :: Int] `mappend` CL.fromList [6..10]) C.$$ CL.fold (+) 0
            x @?= sum [1..10]
        it "mconcat" $ do
            x <- runResourceT $ mconcat
                [ CL.fromList [1..5 :: Int]
                , CL.fromList [6..10]
                , CL.fromList [11..20]
                ] C.$$ CL.fold (+) 0
            x @?= sum [1..20]
    describe "file access" $ do
        it "read" $ do
            bs <- S.readFile "conduit.cabal"
            bss <- runResourceT $ CB.sourceFile "conduit.cabal" C.$$ CL.consume
            bs @=? S.concat bss
        it "write" $ do
            runResourceT $ CB.sourceFile "conduit.cabal" C.$$ CB.sinkFile "tmp"
            bs1 <- S.readFile "conduit.cabal"
            bs2 <- S.readFile "tmp"
            bs1 @=? bs2
