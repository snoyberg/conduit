{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A full tutorial for this module is available at:
-- <https://github.com/snoyberg/conduit/blob/master/PROCESS.md>.
--
-- Some utilities in this module require the threaded runtime because they use
-- 'System.Process.waitForProcess' internally.
--
-- Note that this is a very thin layer around the @Data.Streaming.Process@ module. In particular, it:
--
-- * Provides orphan instances for conduit
--
-- * Provides some useful helper functions
module Data.Conduit.Process
    ( -- * Functions
      sourceCmdWithConsumer
    , sourceProcessWithConsumer
    , sourceCmdWithStreams
    , sourceProcessWithStreams
    , withCheckedProcessCleanup
      -- * InputSource types
    , FlushInput(..)
    , BuilderInput(..)
      -- * Reexport
    , module Data.Streaming.Process
    ) where

import Data.Streaming.Process
import Data.Streaming.Process.Internal
import System.Exit (ExitCode (..))
import Control.Monad.IO.Unlift (MonadIO, liftIO, MonadUnliftIO, withRunInIO, withUnliftIO, unliftIO)
import System.IO (hClose, BufferMode (NoBuffering), hSetBuffering)
import Data.Conduit
import Data.Functor (($>))
import Data.Conduit.Binary (sourceHandle, sinkHandle, sinkHandleBuilder, sinkHandleFlush)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Control.Concurrent.Async (runConcurrently, Concurrently(..))
import Control.Exception (onException, throwIO, finally, bracket, catch)
import System.IO.Error (ioeGetErrorType, isResourceVanishedErrorType)
#if (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>), (<*>))
#endif

instance (r ~ (), MonadIO m, i ~ ByteString) => InputSource (ConduitM i o m r) where
    isStdStream = (\(Just h) -> hSetBuffering h NoBuffering $> sinkHandle h, Just CreatePipe)
instance (r ~ (), r' ~ (), MonadIO m, MonadIO n, i ~ ByteString) => InputSource (ConduitM i o m r, n r') where
    isStdStream = (\(Just h) -> hSetBuffering h NoBuffering $> (sinkHandle h, liftIO $ hClose h), Just CreatePipe)

-- | Wrapper for input source which accepts 'Data.ByteString.Builder.Builder's.
-- You can pass 'Data.ByteString.Builder.Extra.flush' to flush the input. Note
-- that the pipe will /not/ automatically close when the processing completes.
--
-- @since 1.3.2
newtype BuilderInput o m r = BuilderInput (ConduitM Builder o m r)

-- | Wrapper for input source  which accepts @Flush@es. Note that the pipe
-- will /not/ automatically close then processing completes.
--
-- @since 1.3.2
newtype FlushInput o m r = FlushInput (ConduitM (Flush ByteString) o m r)

instance (MonadIO m, r ~ ()) => InputSource (BuilderInput o m r) where
  isStdStream = (\(Just h) -> return $ BuilderInput $ sinkHandleBuilder h, Just CreatePipe)
instance (MonadIO m, MonadIO n, r ~ (), r' ~ ()) => InputSource (BuilderInput o m r, n r') where
  isStdStream = (\(Just h) -> return (BuilderInput $ sinkHandleBuilder h, liftIO $ hClose h), Just CreatePipe)
instance (MonadIO m, r ~ ()) => InputSource (FlushInput o m r) where
  isStdStream = (\(Just h) -> return $ FlushInput $ sinkHandleFlush h, Just CreatePipe)
instance (MonadIO m, MonadIO n, r ~ (), r' ~ ()) => InputSource (FlushInput o m r, n r') where
  isStdStream = (\(Just h) -> return (FlushInput $ sinkHandleFlush h, liftIO $ hClose h), Just CreatePipe)

instance (r ~ (), MonadIO m, o ~ ByteString) => OutputSink (ConduitM i o m r) where
    osStdStream = (\(Just h) -> hSetBuffering h NoBuffering $> sourceHandle h, Just CreatePipe)
instance (r ~ (), r' ~ (), MonadIO m, MonadIO n, o ~ ByteString) => OutputSink (ConduitM i o m r, n r') where
    osStdStream = (\(Just h) -> hSetBuffering h NoBuffering $> (sourceHandle h, liftIO $ hClose h), Just CreatePipe)

-- | Given a @CreateProcess@, run the process, with its output being used as a
-- @Source@ to feed the provided @Consumer@. Once the process has completed,
-- return a tuple of the @ExitCode@ from the process and the output collected
-- from the @Consumer@.
--
-- Note that, if an exception is raised by the consumer, the process is /not/
-- terminated. This behavior is different from 'sourceProcessWithStreams' due
-- to historical reasons.
--
-- Requires the threaded runtime.
--
-- Since 1.1.2
sourceProcessWithConsumer :: MonadIO m
                          => CreateProcess
                          -> ConduitT ByteString Void m a -- ^ stdout
                          -> m (ExitCode, a)
sourceProcessWithConsumer cp consumer = do
    (ClosedStream, (source, close), ClosedStream, cph) <- streamingProcess cp
    res <- runConduit $ source .| consumer
    close
    ec <- waitForStreamingProcess cph
    return (ec, res)

-- | Like @sourceProcessWithConsumer@ but providing the command to be run as
-- a @String@.
--
-- Requires the threaded runtime.
--
-- Since 1.1.2
sourceCmdWithConsumer :: MonadIO m
                      => String                  -- ^command
                      -> ConduitT ByteString Void m a -- ^stdout
                      -> m (ExitCode, a)
sourceCmdWithConsumer cmd = sourceProcessWithConsumer (shell cmd)


-- | Given a @CreateProcess@, run the process
-- and feed the provided @Producer@
-- to the stdin @Sink@ of the process.
-- Use the process outputs (stdout, stderr) as @Source@s
-- and feed it to the provided @Consumer@s.
-- Once the process has completed,
-- return a tuple of the @ExitCode@ from the process
-- and the results collected from the @Consumer@s.
--
-- If an exception is raised by any of the streams,
-- the process is terminated.
--
-- IO is required because the streams are run concurrently
-- using the <https://hackage.haskell.org/package/async async> package
--
-- Requires the threaded runtime.
--
-- @since 1.1.12
sourceProcessWithStreams
  :: MonadUnliftIO m
  => CreateProcess
  -> ConduitT () ByteString m () -- ^stdin
  -> ConduitT ByteString Void m a -- ^stdout
  -> ConduitT ByteString Void m b -- ^stderr
  -> m (ExitCode, a, b)
sourceProcessWithStreams cp producerStdin consumerStdout consumerStderr =
  withUnliftIO $ \u -> do
    (  (sinkStdin, closeStdin)
     , (sourceStdout, closeStdout)
     , (sourceStderr, closeStderr)
     , sph) <- streamingProcess cp
    let safeSinkStdin = sinkStdin `catchC` ignoreStdinClosed
        safeCloseStdin = closeStdin `catch` ignoreStdinClosed
    (_, resStdout, resStderr) <-
      runConcurrently (
        (,,)
        <$> Concurrently ((unliftIO u $ runConduit $ producerStdin .| safeSinkStdin) `finally` safeCloseStdin)
        <*> Concurrently (unliftIO u $ runConduit $ sourceStdout .| consumerStdout)
        <*> Concurrently (unliftIO u $ runConduit $ sourceStderr .| consumerStderr))
      `finally` (closeStdout >> closeStderr)
      `onException` terminateStreamingProcess sph
    ec <- waitForStreamingProcess sph
    return (ec, resStdout, resStderr)
  where
    ignoreStdinClosed :: forall m. (MonadIO m) => IOError -> m ()
    ignoreStdinClosed e =
      if isResourceVanishedErrorType (ioeGetErrorType e)
        then pure ()
        else liftIO (throwIO e)

-- | Like @sourceProcessWithStreams@ but providing the command to be run as
-- a @String@.
--
-- Requires the threaded runtime.
--
-- @since 1.1.12
sourceCmdWithStreams
  :: MonadUnliftIO m
  => String                   -- ^command
  -> ConduitT () ByteString m () -- ^stdin
  -> ConduitT ByteString Void m a -- ^stdout
  -> ConduitT ByteString Void m b -- ^stderr
  -> m (ExitCode, a, b)
sourceCmdWithStreams cmd = sourceProcessWithStreams (shell cmd)

-- | Same as 'withCheckedProcess', but kills the child process in the case of
-- an exception being thrown by the provided callback function.
--
-- Requires the threaded runtime.
--
-- @since 1.1.11
withCheckedProcessCleanup
    :: ( InputSource stdin
       , OutputSink stderr
       , OutputSink stdout
       , MonadUnliftIO m
       )
    => CreateProcess
    -> (stdin -> stdout -> stderr -> m b)
    -> m b
withCheckedProcessCleanup cp f = withRunInIO $ \run -> bracket
    (streamingProcess cp)
    (\(_, _, _, sph) -> closeStreamingProcessHandle sph)
    $ \(x, y, z, sph) -> do
        res <- run (f x y z) `onException` terminateStreamingProcess sph
        ec <- waitForStreamingProcess sph
        if ec == ExitSuccess
            then return res
            else throwIO $ ProcessExitedUnsuccessfully cp ec


terminateStreamingProcess :: MonadIO m => StreamingProcessHandle -> m ()
terminateStreamingProcess = liftIO . terminateProcess . streamingProcessHandleRaw
