{-# LANGUAGE DataKinds #-}

-- | The "System.Process.Typed" module from @typed-process@, but with
-- added conduit helpers.
module Data.Conduit.Process.Typed
  ( -- * Conduit specific stuff
    createSink
  , createSource
    -- * Generalized functions
  , withProcess
  , withProcess_
    -- * Reexports
  , module System.Process.Typed
  ) where

import System.Process.Typed hiding (withProcess, withProcess_)
import qualified System.Process.Typed as P
import Data.Conduit (ConduitM)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Control.Monad.IO.Unlift
import qualified Data.ByteString as S
import System.IO (hClose)

-- | Provide input to a process by writing to a conduit.
--
-- @since 1.2.1
createSink :: MonadIO m => StreamSpec 'STInput (ConduitM S.ByteString o m ())
createSink =
    (\h -> C.addCleanup (\_ -> liftIO $ hClose h) (CB.sinkHandle h))
    `fmap` createPipe

-- | Read output from a process by read from a conduit.
--
-- @since 1.2.1
createSource :: MonadIO m => StreamSpec 'STOutput (ConduitM i S.ByteString m ())
createSource =
    (\h -> C.addCleanup (\_ -> liftIO $ hClose h) (CB.sourceHandle h))
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
withProcess_ pc f = withRunInIO $ \run -> P.withProcess pc (run . f)
