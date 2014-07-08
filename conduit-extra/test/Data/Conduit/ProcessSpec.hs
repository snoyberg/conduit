{-# LANGUAGE CPP #-}
module Data.Conduit.ProcessSpec (spec, main) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent.Async (concurrently)
import System.Process (shell)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import System.Exit
import Control.Concurrent.STM

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Data.Conduit.Process" $ do
#ifndef WINDOWS
    prop "cat" $ \wss -> do
        let lbs = L.fromChunks $ map S.pack wss
        ((sink, closeStdin), source, (), ecVar) <- conduitProcess (shell "cat")
        ((), bss) <- concurrently
            (do
                mapM_ yield (L.toChunks lbs) $$ sink
                closeStdin)
            (source $$ CL.consume)
        L.fromChunks bss `shouldBe` lbs
        ec <- atomically $ readTMVar ecVar
        ec `shouldBe` ExitSuccess
#endif
    return ()
