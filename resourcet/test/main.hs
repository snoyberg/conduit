{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Exception            (Exception, MaskingState (MaskedInterruptible),
                                               getMaskingState, throwIO, try, fromException)
import           Control.Exception            (SomeException, handle)
import           Control.Monad                (unless, void)
import qualified Control.Monad.Catch
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.IORef
import           Data.Typeable                (Typeable)
import           Test.Hspec
import           Data.Acquire

main :: IO ()
main = hspec $ do
    describe "general" $ do
        it "survives releasing bottom" $ do
            x <- newIORef (0 :: Int)
            handle (\(_ :: SomeException) -> return ()) $ runResourceT $ do
                _ <- register $ writeIORef x 1
                release undefined
            x' <- readIORef x
            x' `shouldBe` 1
    describe "early release" $ do
        it "works from a different context" $ do
            x <- newIORef (0 :: Int)
            runResourceT $ do
                key <- register $ writeIORef x 1
                runResourceT $ release key
                y <- liftIO $ readIORef x
                liftIO $ y `shouldBe` 1
    describe "resourceForkIO" $ do
        it "waits for all threads" $ do
            x <- newEmptyMVar
            y <- newIORef (0 :: Int)
            z <- newEmptyMVar
            w <- newEmptyMVar

            _ <- runResourceT $ do
                _ <- register $ do
                    writeIORef y 1
                    putMVar w ()
                resourceForkIO $ do
                    () <- liftIO $ takeMVar x
                    y' <- liftIO $ readIORef y
                    _ <- register $ putMVar z y'
                    return ()

            y1 <- readIORef y
            y1 `shouldBe` 0

            putMVar x ()

            z' <- takeMVar z
            z' `shouldBe` 0

            takeMVar w
            y2 <- readIORef y
            Just y2 `shouldBe` Just 1
    describe "unprotecting" $ do
        it "unprotect keeps resource from being cleared" $ do
            x <- newIORef (0 :: Int)
            _ <- runResourceT $ do
              key <- register $ writeIORef x 1
              unprotect key
            y <- readIORef x
            y `shouldBe` 0
    it "cleanup actions are masked #144" $ do
        let checkMasked name = do
                ms <- getMaskingState
                unless (ms == MaskedInterruptible) $
                    error $ show (name, ms)
        _ <- runResourceT $ do
            register (checkMasked "release") >>= release
            register (checkMasked "normal")
        Left Dummy <- try $ runResourceT $ do
            _ <- register (checkMasked "exception")
            liftIO $ throwIO Dummy
        return ()
    describe "mkAcquireType" $ do
        describe "ResourceT" $ do
            it "early" $ do
                ref <- newIORef Nothing
                let acq = mkAcquireType (return ()) $ \() -> writeIORef ref . Just
                runResourceT $ do
                    (releaseKey, ()) <- allocateAcquire acq
                    release releaseKey
                readIORef ref >>= (`shouldBe` Just ReleaseEarly)
            it "normal" $ do
                ref <- newIORef Nothing
                let acq = mkAcquireType (return ()) $ \() -> writeIORef ref . Just
                runResourceT $ do
                    (_releaseKey, ()) <- allocateAcquire acq
                    return ()
                readIORef ref >>= (`shouldBe` Just ReleaseNormal)
            it "exception" $ do
                ref <- newIORef Nothing
                let acq = mkAcquireType (return ()) $ \() -> writeIORef ref . Just
                Left Dummy <- try $ runResourceT $ do
                    (_releaseKey, ()) <- allocateAcquire acq
                    liftIO $ throwIO Dummy
                readIORef ref >>= (`shouldBe` Just ReleaseException)
        describe "with" $ do
            it "normal" $ do
                ref <- newIORef Nothing
                let acq = mkAcquireType (return ()) $ \() -> writeIORef ref . Just
                with acq $ const $ return ()
                readIORef ref >>= (`shouldBe` Just ReleaseNormal)
            it "exception" $ do
                ref <- newIORef Nothing
                let acq = mkAcquireType (return ()) $ \() -> writeIORef ref . Just
                Left Dummy <- try $ with acq $ const $ throwIO Dummy
                readIORef ref >>= (`shouldBe` Just ReleaseException)
    describe "runResourceTChecked" $ do
        it "catches exceptions" $ do
            eres <- try $ runResourceTChecked $ void $ register $ throwIO Dummy
            case eres of
              Right () -> error "Expected an exception"
              Left (ResourceCleanupException Nothing ex []) ->
                case fromException ex of
                  Just Dummy -> return ()
                  Nothing -> error "It wasn't Dummy"
              Left (ResourceCleanupException (Just _) _ []) -> error "Got a ResourceT exception"
              Left (ResourceCleanupException _ _ (_:_)) -> error "Got more than one"
        it "no exception is fine" $ (runResourceTChecked $ void $ register $ return () :: IO ())
        it "catches multiple exceptions" $ do
            eres <- try $ runResourceTChecked $ do
              void $ register $ throwIO Dummy
              void $ register $ throwIO Dummy2
            case eres of
              Right () -> error "Expected an exception"
              Left (ResourceCleanupException Nothing ex1 [ex2]) ->
                case (fromException ex1, fromException ex2) of
                  (Just Dummy, Just Dummy2) -> return ()
                  _ -> error $ "It wasn't Dummy, Dummy2: " ++ show (ex1, ex2)
              Left (ResourceCleanupException (Just _) _ [_]) -> error "Got a ResourceT exception"
              Left (ResourceCleanupException _ _ []) -> error "Only got 1"
              Left (ResourceCleanupException _ _ (_:_:_)) -> error "Got more than 2"
    describe "MonadMask" $
        it "works" (runResourceT $ Control.Monad.Catch.bracket (return ()) (const (return ())) (const (return ())) :: IO ())

data Dummy = Dummy
    deriving (Show, Typeable)
instance Exception Dummy

data Dummy2 = Dummy2
    deriving (Show, Typeable)
instance Exception Dummy2
