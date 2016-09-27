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
import Data.Void (Void)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Control (MonadBaseControl)

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

infixr 2 .|
-- | Combine two @Conduit@s together into a new @Conduit@ (aka 'fuse').
--
-- Output from the upstream (left) conduit will be fed into the
-- downstream (right) conduit. Processing will terminate when
-- downstream (right) returns. Leftover data returned from the right
-- @Conduit@ will be discarded.
--
-- @since 1.2.8
(.|) :: Monad m
     => ConduitM a b m () -- ^ upstream
     -> ConduitM b c m r -- ^ downstream
     -> ConduitM a c m r
(.|) = fuse
{-# INLINE (.|) #-}

-- | Run a pure pipeline until processing completes, i.e. a pipeline
-- with @Identity@ as the base monad. This is equivalient to
-- @runIdentity . runConduit@.
--
-- @since 1.2.8
runConduitPure :: ConduitM () Void Identity r -> r
runConduitPure = runIdentity . runConduit
{-# INLINE runConduitPure #-}

-- | Run a pipeline which acquires resources with @ResourceT@, and
-- then run the @ResourceT@ transformer. This is equivalent to
-- @runResourceT . runConduit@.
--
-- @since 1.2.8
runConduitRes :: MonadBaseControl IO m
              => ConduitM () Void (ResourceT m) r
              -> m r
runConduitRes = runResourceT . runConduit
{-# INLINE runConduitRes #-}
