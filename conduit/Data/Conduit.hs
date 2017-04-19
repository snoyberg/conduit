{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
-- | If this is your first time with conduit, you should probably start with
-- the tutorial:
-- <https://github.com/snoyberg/conduit#readme>.
module Data.Conduit
    ( -- * Core interface
      -- ** Types
      Source
    , Conduit
    , Sink
    , ConduitM
    , Void
      -- ** Connect/fuse operators
    , (.|)
    , ($$)
    , ($=)
    , (=$)
    , (=$=)
    , connect
    , fuse

      -- *** Fuse with upstream results
    , fuseBoth
    , fuseBothMaybe
    , fuseUpstream

      -- ** Primitives
    , await
    , yield
    , yieldM
    , leftover
    , runConduit
    , runConduitPure
    , runConduitRes

      -- ** Finalization
    , bracketC
    , bracketP
    , addCleanup
    , yieldOr

      -- ** Exception handling
    , catchC
    , handleC
    , tryC

      -- * Generalized conduit types
    , Producer
    , Consumer
    , toProducer
    , toConsumer

      -- * Utility functions
    , awaitForever
    , transPipe
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , mergeSource
    , passthroughSink
    , sourceToList

      -- * Connect-and-resume
    , ResumableSource
    , newResumableSource
    , ($$+)
    , ($$++)
    , ($$+-)
    , ($=+)
    , unwrapResumable
    , closeResumableSource

      -- ** For @Conduit@s
    , ResumableConduit
    , newResumableConduit
    , (=$$+)
    , (=$$++)
    , (=$$+-)
    , unwrapResumableConduit

      -- * Fusion with leftovers
    , fuseLeftovers
    , fuseReturnLeftovers

      -- * Flushing
    , Flush (..)

      -- * Newtype wrappers
      -- ** ZipSource
    , ZipSource (..)
    , sequenceSources

      -- ** ZipSink
    , ZipSink (..)
    , sequenceSinks

      -- ** ZipConduit
    , ZipConduit (..)
    , sequenceConduits
    ) where

import Data.Conduit.Internal.Conduit
import Data.Conduit.Internal.Deprecated
import Data.Void (Void)
