{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | The main module, exporting types, utility functions, and fuse and connect
-- operators.
--
-- There are three main types in this package: @Source@ (data producer), @Sink@
-- (data consumer), and @Conduit@ (data transformer). All three are in fact
-- type synonyms for the underlying @Pipe@ data type.
--
-- The typical approach to use of this package is:
--
-- * Compose multiple @Sink@s together using its @Monad@ instance.
--
-- * Left-fuse @Source@s and @Conduit@s into new @Conduit@s.
--
-- * Right-fuse @Conduit@s and @Sink@s into new @Sink@s.
--
-- * Middle-fuse two @Conduit@s into a new @Conduit@.
--
-- * Connect a @Source@ to a @Sink@ to obtain a result.
module Data.Conduit
    ( -- * Types
      Pipe (..)
    , Source
    , Conduit
    , Sink
      -- * Connect/fuse operators
    , ($$)
    , ($$+)
    , ($=)
    , (=$)
    , (=$=)
      -- * Utility functions
      -- ** General
    , await
    , yield
    , hasInput
    , transPipe
    , mapOutput
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
import Data.Conduit.Internal
import Data.Conduit.Util.Source
import Data.Conduit.Util.Sink
import Data.Conduit.Util.Conduit

-- $typeOverview

infixr 0 $$
infixr 0 $$+
infixl 1 $=
infixr 2 =$
infixr 2 =$=


-- | The connect operator, which pulls data from a source and pushes to a sink.
-- There are two ways this process can terminate:
--
-- 1. If the @Sink@ is a @Done@ constructor, the @Source@ is closed.
--
-- 2. If the @Source@ is a @Done@ constructor, the @Sink@ is closed.
--
-- In other words, both the @Source@ and @Sink@ will always be closed. If you
-- would like to keep the @Source@ open to be used for another operations, use
-- the connect-and-resume operators '$$+'.
--
-- Since 0.4.0
($$) :: Monad m => Source m a -> Sink a m b -> m b
src $$ sink = runPipe $ pipe src sink
{-# INLINE ($$) #-}

-- | The connect-and-resume operator. Does not close the @Source@, but instead
-- returns it to be used again. This allows a @Source@ to be used incrementally
-- in a large program, without forcing the entire program to live in the @Sink@
-- monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.4.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (Source m a, b)
src $$+ sink = runPipe $ pipeResume src sink
{-# INLINE ($$+) #-}

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Both the @Source@ and @Conduit@ will be closed when the newly-created
-- @Source@ is closed.
--
-- Leftover data from the @Conduit@ will be discarded.
--
-- Since 0.4.0
($=) :: Monad m => Source m a -> Conduit a m b -> Source m b
($=) = pipe
{-# INLINE ($=) #-}

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Both the @Conduit@ and @Sink@ will be closed when the newly-created @Sink@
-- is closed.
--
-- Leftover data returned from the @Sink@ will be discarded.
--
-- Since 0.4.0
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
(=$) = pipe
{-# INLINE (=$) #-}

-- | Fusion operator, combining two @Pipe@s together into a new @Pipe@.
--
-- Both @Pipe@s will be closed when the newly-created @Pipe@ is closed.
--
-- Leftover data returned from the right @Pipe@ will be discarded.
--
-- Note: in previous versions, this operator would only fuse together two
-- @Conduit@s (known as middle fusion). This operator is generalized to work on
-- all @Pipe@s, including @Source@s and @Sink@s.
--
-- Since 0.4.0
(=$=) :: Monad m => Pipe a b m () -> Pipe b c m r -> Pipe a c m r
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
