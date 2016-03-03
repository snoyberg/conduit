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
import Control.Monad.Catch (MonadMask, onException, throwM)

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
-- Since 1.1.2
sourceProcessWithConsumer :: MonadIO m => CreateProcess -> Consumer ByteString m a -> m (ExitCode, a)
sourceProcessWithConsumer cp consumer = do
    (ClosedStream, (source, close), ClosedStream, cph) <- streamingProcess cp
    res <- source $$ consumer
    close
    ec <- waitForStreamingProcess cph
    return (ec, res)

-- | Like @sourceProcessWithConsumer@ but providing the command to be run as
-- a @String@.
--
-- Since 1.1.2
sourceCmdWithConsumer :: MonadIO m => String -> Consumer ByteString m a -> m (ExitCode, a)
sourceCmdWithConsumer cmd = sourceProcessWithConsumer (shell cmd)

-- | Same as 'withCheckedProcess', but kills the child process in the case of
-- an exception being thrown by the provided callback function.
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
    res <- f x y z `onException`
            liftIO (terminateProcess (streamingProcessHandleRaw sph))
    ec <- waitForStreamingProcess sph
    if ec == ExitSuccess
        then return res
        else throwM $ ProcessExitedUnsuccessfully cp ec
