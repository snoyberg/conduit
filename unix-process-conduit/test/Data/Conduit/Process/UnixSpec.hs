{-# LANGUAGE CPP, OverloadedStrings, BangPatterns #-}
module Data.Conduit.Process.UnixSpec where

import Test.Hspec (describe, it, shouldBe, Spec)
import Data.Conduit.Process.Unix
import Data.Conduit
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import qualified Data.IORef as I
import qualified Data.Conduit.List as CL
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent (threadDelay)
import System.Posix.Signals (sigKILL, sigTERM)
import qualified Control.Concurrent.MVar as M

iorefSink :: IO (Sink ByteString IO (), IO L.ByteString)
iorefSink = do
    m <- M.newEmptyMVar
    let sink front = do
            mbs <- await
            case mbs of
                Nothing -> lift $ M.putMVar m $ L.fromChunks $ front []
                Just bs -> sink $ front . (bs:)
    return (sink id, M.takeMVar m)

spec :: Spec
spec = describe "unix-process-conduit" $ do
    it "stdin/stdout work" $ do
        let content = ["hello\n", "there\n", "world\n"]
            src = mapM_ yield content
            expected = L.fromChunks content
        (sink, getLBS) <- iorefSink
        pid <- forkExecuteFile
            "cat"
            []
            Nothing
            Nothing
            (Just src)
            (Just sink)
            Nothing
        res <- waitForProcess pid
        lbs <- getLBS
        res `shouldBe` ExitSuccess
        lbs `shouldBe` expected
    it "terminateProcess works" $ do
        let src = lift (threadDelay 1000000) >> src
        pid <- forkExecuteFile
            "cat"
            []
            Nothing
            Nothing
            (Just src)
            Nothing
            Nothing
        terminateProcess pid
        res <- waitForProcess pid
        res `shouldBe` ExitFailure
#if MIN_VERSION_process(1, 2, 0)
            (negate $ fromIntegral sigTERM)
#else
            (fromIntegral sigTERM)
#endif
    it "killProcess works" $ do
        let src = lift (threadDelay 1000000) >> src
        pid <- forkExecuteFile
            "cat"
            []
            Nothing
            Nothing
            (Just src)
            Nothing
            Nothing
        killProcess pid
        res <- waitForProcess pid
        res `shouldBe` ExitFailure
#if MIN_VERSION_process(1, 2, 0)
            (negate $ fromIntegral sigKILL)
#else
            (fromIntegral sigKILL)
#endif
    it "environment is set" $ do
        (sink, getLBS) <- iorefSink
        pid <- forkExecuteFile
            "env"
            []
            (Just [("foo", S.take (read "3") $ "barbarbar")])
            Nothing
            Nothing
            (Just sink)
            Nothing
        res <- waitForProcess pid
        lbs <- fmap (filter (L.isPrefixOf "foo=") . L.split 10) getLBS
        res `shouldBe` ExitSuccess
        lbs `shouldBe` [L.fromChunks ["foo=bar"]]
