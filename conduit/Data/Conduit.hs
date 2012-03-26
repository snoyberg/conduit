{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | The main module, exporting types, utility functions, and fuse and connect
-- operators.
module Data.Conduit
    ( -- * Types
      -- | The three core types to this package are 'Source' (the data
      -- producer), 'Sink' (the data consumer), and 'Conduit' (the data
      -- transformer). For all three types, a result will provide the next
      -- value to be used. For example, the @Open@ constructor includes a new
      -- @Source@ in it. This leads to the main invariant for all conduit code:
      -- these three types may /never/ be reused.  While some specific values
      -- may work fine with reuse, the result is generally unpredictable and
      -- should no be relied upon.
      --
      -- The user-facing API provided by the connect and fuse operators
      -- automatically addresses the low level details of pulling, pushing, and
      -- closing, and there should rarely be need to perform these actions in
      -- user code.

      module Data.Conduit.Types
      -- * Connect/fuse operators
    , ($$)
    , ($$&)
    , ($=)
    , (=$)
    , (=$=)
      -- * Utility functions
      -- ** Source
    , module Data.Conduit.Util.Source
      -- ** Sink
    , module Data.Conduit.Util.Sink
      -- ** Conduit
    , module Data.Conduit.Util.Conduit
      -- * Flushing
    , Flush (..)
      -- * Convenience re-exports
    , ResourceT
    , MonadResource
    , MonadThrow (..)
    , MonadUnsafeIO (..)
    , runResourceT
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Types
import Data.Conduit.Util.Source
import Data.Conduit.Util.Sink
import Data.Conduit.Util.Conduit

-- $typeOverview

infixr 0 $$

infixr 0 $$&

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- There are two ways this process can terminate:
--
-- 1. If the @Sink@ is a @Done@ constructor, the @Source@ is closed.
--
-- 2. If the @Source@ is a @Closed@ constructor, the @Sink@ is closed.
--
-- This function will automatically close any @Source@s, but will not close any
-- @BufferedSource@s, allowing them to be reused. Also, leftover data will be
-- discarded when connecting a @Source@, but will be buffered when using a
-- @BufferedSource@.
--
-- Since 0.3.0
($$) :: Monad m => Source m a -> Sink a m b -> m b
src $$ sink = runPipe $ pipe src sink
{-# INLINE ($$) #-}

-- | The connect-and-resume operator. Does not close the @Source@, but instead
-- returns it to be used again.
($$&) :: Monad m => Source m a -> Sink a m b -> m (Source m a, b)
src $$& sink = runPipe $ pipeL src sink
{-# INLINE ($$&) #-}

infixl 1 $=

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Any @Source@ passed in will be automatically closed, while a
-- @BufferedSource@ will be left open. Leftover input will be discarded for a
-- @Source@, and buffered for a @BufferedSource@.
--
-- Since 0.3.0
($=) :: Monad m => Source m a -> Conduit a m b -> Source m b
($=) = pipe
{-# INLINE ($=) #-}

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Any leftover data returns from the @Sink@ will be discarded.
--
-- Since 0.3.0
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
(=$) = pipe
{-# INLINE (=$) #-}

infixr 0 =$=

-- | Middle fuse, combining two conduits together into a new conduit.
--
-- Any leftovers provided by the inner @Conduit@ will be discarded.
--
-- Since 0.3.0
(=$=) :: Monad m => Conduit a m b -> Conduit b m c -> Conduit a m c
(=$=) = pipe
{-# INLINE (=$=) #-}

-- | Provide for a stream of data that can be flushed.
--
-- A number of @Conduit@s (e.g., zlib compression) need the ability to flush
-- the stream at some point. This provides a single wrapper datatype to be used
-- in all such circumstances.
--
-- Since 0.3.0
data Flush a = Chunk a | Flush
    deriving (Show, Eq, Ord)
instance Functor Flush where
    fmap _ Flush = Flush
    fmap f (Chunk a) = Chunk (f a)
