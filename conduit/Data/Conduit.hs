{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
-- | If this is your first time with conduit, you should probably start with
-- the tutorial:
-- <https://haskell.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview>.
module Data.Conduit
    ( -- * Core interface
      -- ** Types
      Source
    , Conduit
    , Sink
    , ConduitM
      -- ** Connect/fuse operators
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
    , leftover
    , runConduit

      -- ** Finalization
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
    , passthroughSink

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

-- | Named function synonym for '$$'.
--
-- Since 1.2.3
connect :: Monad m => Source m a -> Sink a m b -> m b
connect = ($$)

-- | Named function synonym for '=$='.
--
-- Since 1.2.3
fuse :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
fuse = (=$=)
