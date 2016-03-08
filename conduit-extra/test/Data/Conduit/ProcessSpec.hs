{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.ProcessSpec (spec, main) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Control.Concurrent.Async (concurrently)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
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
        (sourceCmdWithStreams "exit 0" CL.sourceNull CL.sinkNull CL.sinkNull)
                `shouldReturn` (ExitSuccess, (), ())
        (sourceCmdWithStreams "exit 11" CL.sourceNull CL.sinkNull CL.sinkNull)
                `shouldReturn` (ExitFailure 11, (), ())
        (sourceCmdWithStreams "exit 12" CL.sourceNull CL.sinkNull CL.sinkNull)
                `shouldReturn` (ExitFailure 12, (), ())

    it "consumes stdout" $ do
        let mystr = "this is a test string" :: String
        sourceCmdWithStreams ("echo -n " ++ mystr)
                             CL.sourceNull
                             CL.consume -- stdout
                             CL.consume -- stderr
                `shouldReturn` (ExitSuccess, [S8.pack mystr], [])

    it "consumes stderr" $ do
        let mystr = "this is a test string" :: String
        sourceCmdWithStreams ("sh -c \">&2 echo -n " ++ mystr ++ "\"")
                             CL.sourceNull
                             CL.consume -- stdout
                             CL.consume -- stderr
                `shouldReturn` (ExitSuccess, [], [S8.pack mystr])

    it "feeds stdin" $ do
        let mystr = "this is a test string" :: S.ByteString
        sourceCmdWithStreams "cat"
                             (mapM_ yield . L.toChunks $ L.fromStrict mystr)
                             CL.consume -- stdout
                             CL.consume -- stderr
                `shouldReturn` (ExitSuccess, [mystr], [])
#endif
    it "blocking vs non-blocking" $ do
        (ClosedStream, ClosedStream, ClosedStream, cph) <- streamingProcess (shell "sleep 1")

        mec1 <- getStreamingProcessExitCode cph
        mec1 `shouldBe` Nothing

        threadDelay 1500000

        -- For slow systems where sleep may take longer than 1.5 seconds, do
        -- this in a loop.
        let loop 0 = error "Took too long for sleep to exit, your system is acting funny"
            loop i = do
                mec2 <- getStreamingProcessExitCode cph
                case mec2 of
                    Nothing -> do
                        threadDelay 500000
                        loop (pred i)
                    Just _ -> mec2 `shouldBe` Just ExitSuccess
        loop 5

        ec <- waitForStreamingProcess cph
        ec `shouldBe` ExitSuccess
