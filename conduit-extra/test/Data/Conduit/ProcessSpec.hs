{-# LANGUAGE CPP #-}
module Data.Conduit.ProcessSpec (spec, main) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Control.Concurrent.Async (concurrently)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import System.Exit
import Control.Concurrent (threadDelay)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Data.Conduit.Process" $ do
#ifndef WINDOWS
    prop "cat" $ \wss -> do
        let lbs = L.fromChunks $ map S.pack wss
        ((sink, closeStdin), source, Inherited, cph) <- streamingProcess (shell "cat")
        ((), bss) <- concurrently
            (do
                mapM_ yield (L.toChunks lbs) $$ sink
                closeStdin)
            (source $$ CL.consume)
        L.fromChunks bss `shouldBe` lbs
        ec <- waitForStreamingProcess cph
        ec `shouldBe` ExitSuccess

    it "closed stream" $ do
        (ClosedStream, source, Inherited, cph) <- streamingProcess (shell "cat")
        bss <- source $$ CL.consume
        bss `shouldBe` []

        ec <- waitForStreamingProcess cph
        ec `shouldBe` ExitSuccess

    it "handles sub-process exit code" $ do
        (sourceCmdWithConsumer "exit 0" CL.sinkNull)
                `shouldReturn` (ExitSuccess, ())
        (sourceCmdWithConsumer "exit 11" CL.sinkNull)
                `shouldReturn` (ExitFailure 11, ())
        (sourceCmdWithConsumer "exit 12" CL.sinkNull)
                `shouldReturn` (ExitFailure 12, ())
#endif
    it "blocking vs non-blocking" $ do
        (ClosedStream, ClosedStream, ClosedStream, cph) <- streamingProcess (shell "sleep 1")

        mec1 <- getStreamingProcessExitCode cph
        mec1 `shouldBe` Nothing

        threadDelay 1500000

        mec2 <- getStreamingProcessExitCode cph
        mec2 `shouldBe` Just ExitSuccess

        ec <- waitForStreamingProcess cph
        ec `shouldBe` ExitSuccess
