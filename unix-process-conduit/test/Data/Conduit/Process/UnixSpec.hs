{-# LANGUAGE OverloadedStrings, BangPatterns #-}
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

iorefSink :: IO (Sink ByteString IO (), IO L.ByteString)
iorefSink = do
    ref <- I.newIORef id
    let sink = CL.mapM_ $ \bs -> do
            !() <- I.atomicModifyIORef ref $ \front -> (front . (bs:), ())
            return ()
        getLBS = do
            front <- I.readIORef ref
            return $ L.fromChunks $ front []
    return (sink, getLBS)

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
        res `shouldBe` ExitFailure (fromIntegral sigTERM)
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
        res `shouldBe` ExitFailure (fromIntegral sigKILL)
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
        lbs <- getLBS
        res `shouldBe` ExitSuccess
        lbs `shouldBe` L.fromChunks ["foo=bar\n"]
