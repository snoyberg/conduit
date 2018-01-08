{-# LANGUAGE DataKinds #-}

-- | The "System.Process.Typed" module from @typed-process@, but with
-- added conduit helpers.
module Data.Conduit.Process.Typed
  ( -- * Conduit specific stuff
    createSink
  , createSource
    -- * Generalized functions
  , withProcess -- FIXME import from rio instead
  , withProcess_
  , withLoggedProcess_
    -- * Reexports
  , module System.Process.Typed
  ) where

import System.Process.Typed hiding (withProcess, withProcess_)
import qualified System.Process.Typed as P
import Data.Conduit (ConduitM, (.|), runConduit)
import qualified Data.Conduit.Binary as CB
import Control.Monad.IO.Unlift
import qualified Data.ByteString as S
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Control.Exception (throwIO, catch)
import Control.Concurrent.Async (concurrently)

-- | Provide input to a process by writing to a conduit.
--
-- @since 1.2.1
createSink :: MonadIO m => StreamSpec 'STInput (ConduitM S.ByteString o m ())
createSink = CB.sinkHandle `fmap` createPipe

-- | Read output from a process by read from a conduit.
--
-- @since 1.2.1
createSource :: MonadIO m => StreamSpec 'STOutput (ConduitM i S.ByteString m ())
createSource = CB.sourceHandle `fmap` createPipe

-- | Internal function: like 'createSource', but stick all chunks into
-- the 'IORef'.
createSourceLogged
  :: MonadIO m
  => IORef ([S.ByteString] -> [S.ByteString])
  -> StreamSpec 'STOutput (ConduitM i S.ByteString m ())
createSourceLogged ref =
    -- We do not add a cleanup action to close the handle, since in
    -- withLoggedProcess_ we attempt to read from the handle twice
    (\h ->
       (  CB.sourceHandle h
       .| CL.iterM (\bs -> liftIO $ modifyIORef ref (. (bs:))))
    )
    `fmap` createPipe

-- | Same as 'P.withProcess', but generalized to 'MonadUnliftIO'.
--
-- @since 1.2.1
withProcess
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcess pc f = withRunInIO $ \run -> P.withProcess pc (run . f)

-- | Same as 'P.withProcess_', but generalized to 'MonadUnliftIO'.
--
-- @since 1.2.1
withProcess_
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcess_ pc f = withRunInIO $ \run -> P.withProcess_ pc (run . f)

-- | Run a process, throwing an exception on a failure exit code. This
-- will store all output from stdout and stderr in memory for better
-- error messages. Note that this will require unbounded memory usage,
-- so caveat emptor.
--
-- This will ignore any previous settings for the stdout and stderr
-- streams, and instead force them to use 'createSource'.
--
-- @since 1.2.3
withLoggedProcess_
  :: MonadUnliftIO m
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -> (Process stdin (ConduitM () S.ByteString m ()) (ConduitM () S.ByteString m ()) -> m a)
  -> m a
withLoggedProcess_ pc inner = withUnliftIO $ \u -> do
  stdoutBuffer <- newIORef id
  stderrBuffer <- newIORef id
  let pc' = setStdout (createSourceLogged stdoutBuffer)
          $ setStderr (createSourceLogged stderrBuffer) pc
  P.withProcess pc' $ \p -> do
    a <- unliftIO u $ inner p
    let drain src = unliftIO u (runConduit (src .| CL.sinkNull))
    ((), ()) <- drain (getStdout p) `concurrently`
                drain (getStderr p)
    checkExitCode p `catch` \ece -> do
      stdout <- readIORef stdoutBuffer
      stderr <- readIORef stderrBuffer
      throwIO ece
        { eceStdout = BL.fromChunks $ stdout []
        , eceStderr = BL.fromChunks $ stderr []
        }
    return a
