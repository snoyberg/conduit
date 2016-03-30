{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A full tutorial for this module is available on FP School of Haskell:
-- <https://www.fpcomplete.com/user/snoyberg/library-documentation/data-conduit-process>.
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
      -- * Reexport
    , module Data.Streaming.Process
    ) where

import Data.Streaming.Process
import Data.Streaming.Process.Internal
import System.Exit (ExitCode (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (hClose)
import Data.Conduit
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import Data.ByteString (ByteString)
import Control.Concurrent.Async (runConcurrently, Concurrently(..))
import Control.Monad.Catch (MonadMask, onException, throwM, finally)
import Control.Applicative ((<$>), (<*>))

instance (r ~ (), MonadIO m, i ~ ByteString) => InputSource (ConduitM i o m r) where
    isStdStream = (\(Just h) -> return $ sinkHandle h, Just CreatePipe)
instance (r ~ (), r' ~ (), MonadIO m, MonadIO n, i ~ ByteString) => InputSource (ConduitM i o m r, n r') where
    isStdStream = (\(Just h) -> return (sinkHandle h, liftIO $ hClose h), Just CreatePipe)

instance (r ~ (), MonadIO m, o ~ ByteString) => OutputSink (ConduitM i o m r) where
    osStdStream = (\(Just h) -> return $ sourceHandle h, Just CreatePipe)
instance (r ~ (), r' ~ (), MonadIO m, MonadIO n, o ~ ByteString) => OutputSink (ConduitM i o m r, n r') where
    osStdStream = (\(Just h) -> return (sourceHandle h, liftIO $ hClose h), Just CreatePipe)

-- | Given a @CreateProcess@, run the process, with its output being used as a
-- @Source@ to feed the provided @Consumer@. Once the process has completed,
-- return a tuple of the @ExitCode@ from the process and the output collected
-- from the @Consumer@.
--
-- If an exception is raised by the consumer,
-- the process is terminated.
--
-- Since 1.1.2
sourceProcessWithConsumer :: (MonadMask m, MonadIO m)
                          => CreateProcess
                          -> Consumer ByteString m a -- ^stdout
                          -> m (ExitCode, a)
sourceProcessWithConsumer cp consumer = do
    (ClosedStream, (source, close), ClosedStream, sph) <- streamingProcess cp
    res <- (source $$ consumer)
           `finally` close
           `onException` liftIO (terminateStreamingProcess sph)
    ec <- waitForStreamingProcess sph
    return (ec, res)

-- | Like @sourceProcessWithConsumer@ but providing the command to be run as
-- a @String@.
--
-- Since 1.1.2
sourceCmdWithConsumer :: (MonadMask m, MonadIO m)
                      => String                  -- ^command
                      -> Consumer ByteString m a -- ^stdout
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
-- @since 1.1.12
sourceProcessWithStreams :: CreateProcess
                         -> Producer IO ByteString   -- ^stdin
                         -> Consumer ByteString IO a -- ^stdout
                         -> Consumer ByteString IO b -- ^stderr
                         -> IO (ExitCode, a, b)
sourceProcessWithStreams cp producerStdin consumerStdout consumerStderr = do
    (  (sinkStdin, closeStdin)
     , (sourceStdout, closeStdout)
     , (sourceStderr, closeStderr)
     , sph) <- streamingProcess cp
    (_, resStdout, resStderr) <-
      runConcurrently (
        (,,)
        <$> Concurrently ((producerStdin $$ sinkStdin) `finally` closeStdin)
        <*> Concurrently (sourceStdout  $$ consumerStdout)
        <*> Concurrently (sourceStderr  $$ consumerStderr))
      `finally` (closeStdout >> closeStderr)
      `onException` terminateStreamingProcess sph
    ec <- waitForStreamingProcess sph
    return (ec, resStdout, resStderr)

-- | Like @sourceProcessWithStreams@ but providing the command to be run as
-- a @String@.
--
-- @since 1.1.12
sourceCmdWithStreams :: String                   -- ^command
                     -> Producer IO ByteString   -- ^stdin
                     -> Consumer ByteString IO a -- ^stdout
                     -> Consumer ByteString IO b -- ^stderr
                     -> IO (ExitCode, a, b)
sourceCmdWithStreams cmd = sourceProcessWithStreams (shell cmd)

-- | Same as 'withCheckedProcess', but kills the child process in the case of
-- an exception being thrown by the provided callback function.
--
-- @since 1.1.11
withCheckedProcessCleanup
    :: ( InputSource stdin
       , OutputSink stderr
       , OutputSink stdout
       , MonadIO m
       , MonadMask m
       )
    => CreateProcess
    -> (stdin -> stdout -> stderr -> m b)
    -> m b
withCheckedProcessCleanup cp f = do
    (x, y, z, sph) <- streamingProcess cp
    res <- f x y z `onException` liftIO (terminateStreamingProcess sph)
    ec <- waitForStreamingProcess sph
    if ec == ExitSuccess
        then return res
        else throwM $ ProcessExitedUnsuccessfully cp ec


terminateStreamingProcess :: StreamingProcessHandle -> IO ()
terminateStreamingProcess = terminateProcess . streamingProcessHandleRaw
