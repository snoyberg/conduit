module Data.Conduit.Process.Unix
    ( forkExecuteFile
    , killProcess
    , waitForProcess
    , ProcessStatus (..)
    ) where

import           Control.Concurrent                (forkIO)
import           Control.Exception                 (finally, mask, onException)
import           Control.Monad                     (unless, void)
import           Control.Monad.Trans.Class         (lift)
import           Data.ByteString                   (ByteString, null)
import           Data.ByteString.Unsafe            (unsafePackCStringFinalizer,
                                                    unsafeUseAsCStringLen)
import           Data.Conduit                      (Sink, Source, yield, ($$))
import           Data.Conduit.List                 (mapM_)
import           Foreign.Marshal.Alloc             (free, mallocBytes)
import           Foreign.Ptr                       (castPtr)
import           Prelude                           (Bool (..), IO, Maybe (..),
                                                    Monad (..), flip,
                                                    fromIntegral, fst, maybe,
                                                    snd, ($), (.))
import           System.Posix.Directory.ByteString (changeWorkingDirectory)
import           System.Posix.IO.ByteString        (closeFd, createPipe, dupTo,
                                                    fdReadBuf, fdWriteBuf,
                                                    stdError, stdInput,
                                                    stdOutput)
import           System.Posix.Process.ByteString   (ProcessStatus (..),
                                                    executeFile, forkProcess,
                                                    getProcessStatus)
import           System.Posix.Signals              (sigKILL, signalProcess)
import           System.Posix.Types                (ProcessID)

-- | Kill a process by sending it the KILL (9) signal.
--
-- Since 0.1.0
killProcess :: ProcessID -> IO ()
killProcess = signalProcess sigKILL

-- | Fork a new process and execute the given command.
--
-- This is a wrapper around with fork() and exec*() syscalls, set up to work
-- with @conduit@ datatypes for standard input, output, and error. If @Nothing@
-- is provided for any of those arguments, then the original file handles will
-- remain open to the child process.
--
-- If you would like to simply discard data provided by the child process,
-- provide @sinkNull@ for stdout and/or stderr. To provide an empty input
-- stream, use @return ()@.
--
-- Since 0.1.0
forkExecuteFile :: ByteString -- ^ command
                -> Bool -- ^ search on PATH?
                -> [ByteString] -- ^ args
                -> Maybe [(ByteString, ByteString)] -- ^ environment
                -> Maybe ByteString -- ^ working directory
                -> Maybe (Source IO ByteString) -- ^ stdin
                -> Maybe (Sink ByteString IO ()) -- ^ stdout
                -> Maybe (Sink ByteString IO ()) -- ^ stderr
                -> IO ProcessID
forkExecuteFile cmd path args menv mwdir mstdin mstdout mstderr = do
    min  <- withIn  mstdin
    mout <- withOut mstdout
    merr <- withOut mstderr
    pid <- forkProcess $ do
        maybe (return ()) changeWorkingDirectory mwdir
        case min of
            Nothing -> return ()
            Just (fdRead, fdWrite) -> do
                closeFd fdWrite
                void $ dupTo fdRead stdInput
        let goOut Nothing _ = return ()
            goOut (Just (fdRead, fdWrite)) dest = do
                closeFd fdRead
                void $ dupTo fdWrite dest
        goOut mout stdOutput
        goOut merr stdError
        executeFile cmd path args menv
    maybe (return ()) (closeFd . fst) min
    maybe (return ()) (closeFd . snd) mout
    maybe (return ()) (closeFd . snd) merr
    return pid
  where
    withIn Nothing = return Nothing
    withIn (Just src) = do
        (fdRead, fdWrite) <- createPipe
        let sink = mapM_ $ flip unsafeUseAsCStringLen $ \(ptr, size) -> void $ fdWriteBuf fdWrite (castPtr ptr) (fromIntegral size)
        void $ forkIO $ (src $$ sink) `finally` closeFd fdWrite
        return $ Just (fdRead, fdWrite)
    withOut Nothing = return Nothing
    withOut (Just sink) = do
        (fdRead, fdWrite) <- createPipe
        let buffSize = 4096
        let src = do
                bs <- lift $ mask $ \restore -> do
                    ptr <- mallocBytes buffSize
                    bytesRead <- restore (fdReadBuf fdRead ptr $ fromIntegral buffSize) `onException` free ptr
                    unsafePackCStringFinalizer ptr (fromIntegral bytesRead) (free ptr)
                unless (null bs) $ do
                    yield bs
                    src
        void $ forkIO $ (src $$ sink) `finally` closeFd fdRead
        return $ Just (fdRead, fdWrite)

-- | Wait until the given process has died, and return its @ProcessStatus@.
--
-- Since 0.1.0
waitForProcess :: ProcessID -> IO ProcessStatus
waitForProcess pid =
    loop
  where
    loop = getProcessStatus True False pid >>= maybe loop return
