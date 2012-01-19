{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where
import Test.Hspec.Monadic

import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as AC8
import qualified Data.Conduit as C
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Lazy as CLazy
import qualified Data.Conduit.Text as CT
import Data.Conduit.Blaze (builderToByteString)
import Data.Conduit (runResourceT)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.IORef as I
import Blaze.ByteString.Builder (fromByteString, toLazyByteString, insertLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Writer (Writer)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Monad.Trans.Resource (runExceptionT_, withIO, resourceForkIO)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative -- (pure, (<$>), (<*>))


testParseWord :: String -> A.Parser S.ByteString
testParseWord s = (AC8.string $ C8.pack s) <* AC8.space

main :: IO ()
main = hspecX $ do
    describe "parserSink" $ do
        let pWord = testParseWord "test"
        let src = CL.sourceList $ C8.pack <$> lines "test one two"
        it "only runs parser once" $ do
            res <- C.runResourceT $ src $$ CA.sinkParser pWord
            res @?= C8.pack "test"
        
        it "leaves the rest of input" $ do
            (x, y) <- runResourceT $ do
                bsrc <- C.bufferSource src
                x <- bsrc $$ CA.sinkParser pWord
                y <- bsrc $$ CL.consume
                return (x, y)
            y @?= [C8.pack "one two"]
