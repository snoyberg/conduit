{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
-- | If this is your first time with conduit, you should probably start with
-- the tutorial:
-- <https://haskell.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview>.
module Data.Conduit
    ( -- * Core interface
      -- ** Types
      Source
    , Conduit
    , Sink
    , Pipe -- FIXME replace with ConduitM
      -- ** Connect/fuse operators
    , ($$)
    , ($=)
    , (=$)
    , (=$=)
    , (>+>)
    , (<+<)

      -- ** Primitives
    , await
    , yield
    , tryYield
    , leftover

      -- ** Finalization
    , bracketP
    , addCleanup
    , yieldOr

      -- * Generalized conduit types
    , Producer
    , Consumer
    , toProducer
    , toConsumer

      -- * Utility functions
    , awaitForever
    , mapOutput
    , mapOutputMaybe
    , mapInput

      -- * Connect-and-resume
    , ($$+)

      -- * Flushing
    , Flush (..)

      -- * Convenience re-exports
    , ResourceT
    , MonadResource
    , MonadThrow (..)
    , MonadUnsafeIO (..)
    , runResourceT
    , ExceptionT (..)
    , runExceptionT_
    , runException
    , runException_
    , MonadBaseControl
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Internal
import Data.Void (Void, absurd)

-- | Provides a stream of output values, without consuming any input or
-- producing a final result.
--
-- Since 0.5.0
type Source m o = Pipe () o () () m ()

-- | A component which produces a stream of output values, regardless of the
-- input stream. A @Producer@ is a generalization of a @Source@, and can be
-- used as either a @Source@ or a @Conduit@.
--
-- Since 1.0.0
type Producer m o = forall i. Pipe i o () () m ()

-- | Consumes a stream of input values and produces a final result, without
-- producing any output.
--
-- Since 0.5.0
type Sink i m r = Pipe i Void () Void m r

-- | A component which consumes a stream of input values and produces a final
-- result, regardless of the output stream. A @Consumer@ is a generalization of
-- a @Sink@, and can be used as either a @Sink@ or a @Conduit@.
--
-- Since 1.0.0
type Consumer i m r = forall d o t. Pipe i o d t m r

-- | Consumes a stream of input values and produces a stream of output values,
-- without producing a final result.
--
-- Since 0.5.0
type Conduit i m o = Pipe i o () () m ()

-- Define fixity of all our operators
infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 0 $$+

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- If you would like to keep the @Source@ open to be used for other
-- operations, use the connect-and-resume operator '$$+'.
--
-- Since 0.4.0
($$) :: Monad m
     => Source m a
     -> Sink a m b
     -> m b
src $$ sink = do
    (src', res) <- connectResume src sink
    closePipe src'
    return res
{-# INLINE ($$) #-}

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Both the @Source@ and @Conduit@ will be closed when the newly-created
-- @Source@ is closed.
--
-- Leftover data from the @Conduit@ will be discarded.
--
-- Since 0.4.0
($=) :: Monad m => Source m a -> Conduit a m b -> Source m b
src $= conduit = pipe id id (const Pure) (const Pure) src conduit
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
conduit =$ sink = pipe absurd (const ()) (\c as () -> Pure as c) (\c as () -> Pure as c) conduit sink
{-# INLINE (=$) #-}

-- | Fusion operator, combining two @Conduit@s together into a new @Conduit@.
--
-- Both @Conduit@s will be closed when the newly-created @Conduit@ is closed.
--
-- Leftover data returned from the right @Conduit@ will be discarded.
--
-- Since 0.4.0
(=$=) :: Monad m
      => Conduit a m b
      -> Conduit b m c
      -> Conduit a m c
up =$= down = pipe id id (const Pure) (const Pure) up down
{-# INLINE (=$=) #-}

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.5.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (Source m a, b)
src $$+ sink = connectResume src sink
{-# INLINE ($$+) #-}

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

-- | Generalize a 'Source' to a 'Producer'.
--
-- Since 1.0.0
toProducer :: Monad m => Source m a -> Producer m a
toProducer = error "toProducer"
{-
    go
  where
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput _ c) = go c
    go (Done _ls r) = Done [] r
    go (ConduitM mp) = ConduitM (liftM go mp)
    --go (Leftover p ()) = go p
-}

-- | Generalize a 'Sink' to a 'Consumer'.
--
-- Since 1.0.0
toConsumer :: Monad m => Sink a m b -> Consumer a m b
toConsumer = error "toConsumer"
{-
    go
  where
    go (HaveOutput _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (go c)
    go (Done ls r) = Done ls r
    go (ConduitM mp) = ConduitM (liftM go mp)
    --go (Leftover p l) = Leftover (go p) l
-}
