{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
import Data.Conduit (runResourceT)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()

main :: IO ()
main = hspecX $ do
    describe "zlib" $ do
        prop "idempotent" $ \bss' -> runST $ runResourceT $ do
            let bss = map S.pack bss'
                lbs = L.fromChunks bss
                src = mconcat $ map (CL.sourceList . return) bss
            outBss <- src C.$= CZ.gzip C.$= CZ.ungzip C.$$ CL.consume
            return $ lbs == L.fromChunks outBss
