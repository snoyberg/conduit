{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | A full tutorial for this module is available on FP School of Haskell:
-- <https://www.fpcomplete.com/user/snoyberg/library-documentation/data-conduit-process>.
module Data.Conduit.Process
    ( -- * Functions
      conduitProcess
      -- * Specialized streaming types
    , Inherited (..)
    , ClosedStream (..)
    , UseProvidedHandle (..)
      -- * Process handle
    , ConduitProcessHandle
    , waitForConduitProcess
    , waitForConduitProcessSTM
    , getConduitProcessExitCode
    , getConduitProcessExitCodeSTM
    , conduitProcessHandleRaw
    , conduitProcessHandleTMVar
      -- * Type classes
    , InputSource
    , OutputSink
      -- * Reexport
    , module System.Process
      -- * Deprecated compatibility functions
    , sourceCmd
    ) where

import System.Process
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVar, putTMVar, STM, readTMVar, tryReadTMVar)
import Control.Exception (throwIO)
import System.Exit (ExitCode (ExitSuccess))
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, hClose)
import Data.Conduit
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))

-- | Use the @Handle@ provided by the @CreateProcess@ value. This would allow
-- you, for example, to open up a @Handle@ to a file, set it as @std_out@, and
-- avoid any additional overhead of dealing with providing that data to your
-- process.
--
-- Since 1.1.2
data UseProvidedHandle = UseProvidedHandle

-- | Inherit the stream from the current process.
--
-- Since 1.1.2
data Inherited = Inherited

-- | Close the stream with the child process.
--
-- Since 1.1.2
data ClosedStream = ClosedStream

-- | Class for all things which can be used to provide standard input.
--
-- Since 1.1.2
class InputSource a where
    isStdStream :: (Maybe Handle -> IO a, Maybe StdStream)
instance InputSource Handle where
    isStdStream = (\(Just h) -> return h, Just CreatePipe)
instance InputSource ClosedStream where
    isStdStream = (\(Just h) -> hClose h >> return ClosedStream, Just CreatePipe)
instance (r ~ (), MonadIO m, i ~ ByteString) => InputSource (ConduitM i o m r) where
    isStdStream = (\(Just h) -> return $ sinkHandle h, Just CreatePipe)
instance (r ~ (), r' ~ (), MonadIO m, MonadIO n, i ~ ByteString) => InputSource (ConduitM i o m r, n r') where
    isStdStream = (\(Just h) -> return (sinkHandle h, liftIO $ hClose h), Just CreatePipe)
instance InputSource Inherited where
    isStdStream = (\Nothing -> return Inherited, Just Inherit)
instance InputSource UseProvidedHandle where
    isStdStream = (\Nothing -> return UseProvidedHandle, Nothing)

-- | Class for all things which can be used to consume standard output or
-- error.
--
-- Since 1.1.2
class OutputSink a where
    osStdStream :: (Maybe Handle -> IO a, Maybe StdStream)
instance OutputSink Handle where
    osStdStream = (\(Just h) -> return h, Just CreatePipe)
instance OutputSink ClosedStream where
    osStdStream = (\(Just h) -> hClose h >> return ClosedStream, Just CreatePipe)
instance (r ~ (), MonadIO m, o ~ ByteString) => OutputSink (ConduitM i o m r) where
    osStdStream = (\(Just h) -> return $ sourceHandle h, Just CreatePipe)
instance (r ~ (), r' ~ (), MonadIO m, MonadIO n, o ~ ByteString) => OutputSink (ConduitM i o m r, n r') where
    osStdStream = (\(Just h) -> return (sourceHandle h, liftIO $ hClose h), Just CreatePipe)
instance OutputSink Inherited where
    osStdStream = (\Nothing -> return Inherited, Just Inherit)
instance OutputSink UseProvidedHandle where
    osStdStream = (\Nothing -> return UseProvidedHandle, Nothing)

-- | Wraps up the standard @ProcessHandle@ to avoid the @waitForProcess@
-- deadlock. See the linked documentation from the module header for more
-- information.
--
-- Since 1.1.2
data ConduitProcessHandle = ConduitProcessHandle
    ProcessHandle
    (TMVar ExitCode)

-- | Blocking call to wait for a process to exit.
--
-- Since 1.1.2
waitForConduitProcess :: MonadIO m => ConduitProcessHandle -> m ExitCode
waitForConduitProcess = liftIO . atomically . waitForConduitProcessSTM

-- | STM version of @waitForConduitProcess@.
--
-- Since 1.1.2
waitForConduitProcessSTM :: ConduitProcessHandle -> STM ExitCode
waitForConduitProcessSTM = readTMVar . conduitProcessHandleTMVar

-- | Non-blocking call to check for a process exit code.
--
-- Since 1.1.2
getConduitProcessExitCode :: MonadIO m => ConduitProcessHandle -> m (Maybe ExitCode)
getConduitProcessExitCode = liftIO . atomically .  getConduitProcessExitCodeSTM

-- | STM version of @getConduitProcessExitCode@.
--
-- Since 1.1.2
getConduitProcessExitCodeSTM :: ConduitProcessHandle -> STM (Maybe ExitCode)
getConduitProcessExitCodeSTM = tryReadTMVar . conduitProcessHandleTMVar

-- | Get the raw @ProcessHandle@ from a @ConduitProcessHandle@. Note that
-- you should avoid using this to get the process exit code, and instead
-- use the provided functions.
--
-- Since 1.1.2
conduitProcessHandleRaw :: ConduitProcessHandle -> ProcessHandle
conduitProcessHandleRaw (ConduitProcessHandle ph _) = ph

-- | Get the @TMVar@ storing the process exit code. In general, one of the
-- above functions should be used instead to avoid accidentally corrupting the variable\'s state..
--
-- Since 1.1.2
conduitProcessHandleTMVar :: ConduitProcessHandle -> TMVar ExitCode
conduitProcessHandleTMVar (ConduitProcessHandle _ var) = var

-- | The primary function for running a process. Note that, with the
-- exception of 'UseProvidedHandle', the values for @std_in@, @std_out@
-- and @std_err@ will be ignored by this function.
--
-- Since 1.1.2
conduitProcess :: (MonadIO m, InputSource stdin, OutputSink stdout, OutputSink stderr)
               => CreateProcess
               -> m (stdin, stdout, stderr, ConduitProcessHandle)
conduitProcess cp = liftIO $ do
    let (getStdin, stdinStream) = isStdStream
        (getStdout, stdoutStream) = osStdStream
        (getStderr, stderrStream) = osStdStream

    (stdinH, stdoutH, stderrH, ph) <- createProcess cp
        { std_in = fromMaybe (std_in cp) stdinStream
        , std_out = fromMaybe (std_out cp) stdoutStream
        , std_err = fromMaybe (std_err cp) stderrStream
        }

    ec <- atomically newEmptyTMVar
    _ <- forkIO $ waitForProcess ph >>= atomically . putTMVar ec

    (,,,)
        <$> getStdin stdinH
        <*> getStdout stdoutH
        <*> getStderr stderrH
        <*> return (ConduitProcessHandle ph ec)

-- | This function is dangerous, and should not be used. The running of the
-- process is completely dependent on whether or not you consume input from it,
-- which is an unintuitive and flimsy abstraction. Please move over to using
-- @conduitProcess@ instead.
sourceCmd :: MonadIO m => String -> Source m ByteString
sourceCmd cmd = do
    (ClosedStream, (source, close), ClosedStream, cph) <- conduitProcess (shell cmd)
    flip addCleanup source $ const $ do
        close
        ec <- waitForConduitProcess cph
        case ec of
            ExitSuccess -> return ()
            _ -> liftIO $ throwIO ec
{-# DEPRECATED sourceCmd "Please use conduitProcess instead" #-}
