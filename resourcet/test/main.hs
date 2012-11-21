{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Control.Monad.Trans.Resource
import Data.IORef
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Lifted (fork)
import Control.Exception (handle, SomeException)

main :: IO ()
main = hspec $ do
    describe "general" $ do
        it "survives releasing bottom" $ do
            x <- newIORef 0
            handle (\(_ :: SomeException) -> return ()) $ runResourceT $ do
                _ <- register $ writeIORef x 1
                release undefined
            x' <- readIORef x
            x' `shouldBe` 1
    describe "early release" $ do
        it "works from a different context" $ do
            x <- newIORef 0
            runResourceT $ do
                key <- register $ writeIORef x 1
                runResourceT $ release key
                y <- liftIO $ readIORef x
                liftIO $ y `shouldBe` 1
    describe "forking" $ do
        forkHelper "resourceForkIO" resourceForkIO
        --forkHelper "lifted fork" fork

forkHelper s fork' = describe s $ do
    it "waits for all threads" $ do
        x <- newEmptyMVar
        y <- newIORef 0
        z <- newEmptyMVar
        runResourceT $ do
            _ <- register $ writeIORef y 1
            fork' $ do
                () <- liftIO $ takeMVar x
                y' <- liftIO $ readIORef y
                _ <- register $ putMVar z y'
                return ()

        y1 <- readIORef y
        y1 `shouldBe` 0

        putMVar x ()

        z' <- takeMVar z
        z' `shouldBe` 0

        y2 <- readIORef y
        Just y2 `shouldBe` Just 1
