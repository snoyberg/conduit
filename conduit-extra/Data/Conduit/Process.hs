{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Conduit.Process
    ( conduitProcess
    , UseProvidedHandle (..)
    , InputSource
    , OutputSource
    ) where

import System.Process
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVar, putTMVar)
import System.Exit (ExitCode)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, hClose)
import Data.Conduit
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

data UseProvidedHandle = UseProvidedHandle

class InputSource a where
    isStdStream :: (Maybe Handle -> a, Maybe StdStream)
instance InputSource Handle where
    isStdStream = (\(Just h) -> h, Just CreatePipe)
instance (r ~ (), MonadIO m, i ~ ByteString) => InputSource (ConduitM i o m r) where
    isStdStream = (\(Just h) -> sinkHandle h, Just CreatePipe)
instance (r ~ (), MonadIO m, MonadIO n, i ~ ByteString) => InputSource (ConduitM i o m r, n ()) where
    isStdStream = (\(Just h) -> (sinkHandle h, liftIO $ hClose h), Just CreatePipe)
instance InputSource () where
    isStdStream = (\Nothing -> (), Just Inherit)
instance InputSource UseProvidedHandle where
    isStdStream = (\Nothing -> UseProvidedHandle, Nothing)

class OutputSource a where
    osStdStream :: (Maybe Handle -> a, Maybe StdStream)
instance OutputSource Handle where
    osStdStream = (\(Just h) -> h, Just CreatePipe)
instance (r ~ (), MonadIO m, o ~ ByteString) => OutputSource (ConduitM i o m r) where
    osStdStream = (\(Just h) -> sourceHandle h, Just CreatePipe)
instance (r ~ (), MonadIO m, MonadIO n, o ~ ByteString) => OutputSource (ConduitM i o m r, n ()) where
    osStdStream = (\(Just h) -> (sourceHandle h, liftIO $ hClose h), Just CreatePipe)
instance OutputSource () where
    osStdStream = (\Nothing -> (), Just Inherit)
instance OutputSource UseProvidedHandle where
    osStdStream = (\Nothing -> UseProvidedHandle, Nothing)

conduitProcess :: (MonadIO m, InputSource stdin, OutputSource stdout, OutputSource stderr)
               => CreateProcess
               -> m (stdin, stdout, stderr, TMVar ExitCode)
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

    return (getStdin stdinH, getStdout stdoutH, getStderr stderrH, ec)
