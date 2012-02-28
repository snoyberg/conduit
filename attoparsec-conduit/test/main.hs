{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where
import Test.Hspec.Monadic

import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit
import Test.QuickCheck

import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as AC8
import qualified Data.Conduit as C
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import Control.Monad
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy.Char8 ()
import Data.List (intersperse)
import Control.Monad.Trans.Resource (runExceptionT_)
import Control.Applicative -- (pure, (<$>), (<*>))

instance Arbitrary S.ByteString where
      arbitrary = do
           len <- choose (0, 10)
           chars <- replicateM len (choose ('a', 'z'))
           return $ C8.pack chars

testParseWord :: String -> A.Parser S.ByteString
testParseWord s = AC8.string (C8.pack s) <* AC8.space

main :: IO ()
main = hspecX $ do

    let src = CL.sourceList $ C8.pack <$> lines "test one two\nthree four "
    let takeWord = AC8.takeWhile (/=' ')
    
    

    describe "parserSink" $ do
        
        it "only runs parser once" $ do
            res <- C.runResourceT $ src $$ CA.sinkParser $ testParseWord "test"
            res @?= "test"
        
        it "leaves the rest of input" $ do
            (x, y) <- runResourceT $ do
                bsrc <- C.bufferSource src
                x <- bsrc $$ CA.sinkParser $ testParseWord "test"
                y <- bsrc $$ CL.consume
                return (x, y)
            y @?= C8.lines "one two\nthree four "

        prop "parse first word == head" $
            \inp inp2 -> not (S.null inp) ==>
                 let res = runST $ runExceptionT_ $ runResourceT $
                         CL.sourceList [inp, inp2]
                         $$ CA.sinkParser $ AC8.takeWhile (/=' ')
                 in res == C8.takeWhile (/=' ') (inp `mappend` inp2)

        prop "parse first word leaves exactly tail" $
            \inp inp2 -> not (S.null inp) ==>
                 let res = runST $ runExceptionT_ $ runResourceT $ do
                     bsrc <- C.bufferSource $ CL.sourceList [inp, inp2]
                     _ <- bsrc $$ CA.sinkParser $ takeWord
                     C8.concat <$> (bsrc $$ CL.consume)
                 in res == C8.dropWhile (/=' ') (inp `mappend` inp2)

    describe "paserConduit" $ do
    
        it "runs parser continuously" $ do
            res <- runResourceT $ src
                   $= CA.conduitParser (takeWord <* AC8.space)
                   $$ CL.consume
            res @?= ["test", "one", "twothree", "four"]

        prop "parse word and space == init words" $
            \inp -> length inp > 3 ==>
                let res = runST $ runExceptionT_ $ runResourceT $
                         CL.sourceList  (intersperse " " inp)
                         $= CA.conduitParser (takeWord <* AC8.char ' ')
                         $$ CL.consume
                in res == init inp

