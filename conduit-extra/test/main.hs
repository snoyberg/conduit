{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.Hspec

import Control.Applicative
--import Control.Monad
import Data.Maybe (fromJust)

import           Data.Conduit as C
--import qualified Data.Conduit.Util as C
--import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Extra as CE
import Data.Conduit (runResourceT)
import qualified Data.Conduit.ExtraSpec as ES
import qualified Data.Conduit.Extra.ZipConduitSpec as ZipConduit


main :: IO ()
main = hspec $ do
    describe "zipSink" $ do
        it "zip equal-sized" $ do
            x <- runResourceT $
                    CL.sourceList [1..100] $$
                    CE.broadcast [ CL.fold (+) 0,
                                   (`mod` 101) <$> CL.fold (*) 1 ]
            x `shouldBe` [5050, 100 :: Integer]

        it "zip distinct sizes" $ do
            let sink = CE.getZipSink $
                        (*) <$> CE.ZipSink (CL.fold (+) 0)
                            <*> CE.ZipSink (Data.Maybe.fromJust <$> await)
            x <- runResourceT $ CL.sourceList [100,99..1] $$ sink
            x `shouldBe` (505000 :: Integer)

    ES.spec
    ZipConduit.spec
