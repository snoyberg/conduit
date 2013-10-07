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
    , (+=$)
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
    , ResumableSource (..)
    , ($$+)
    , ($$++)
    , ($$+-)

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

import Control.Monad (liftM)
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
type Producer m o = forall i d. Pipe i o d d m ()

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
infixr 2 +=$
infixr 2 =$=
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- If you would like to keep the @Source@ open to be used for other
-- operations, use the connect-and-resume operator '$$+'.
--
-- Note that this applies @checkTerminate@ to upstream to ensure upstream is
-- not run if downstream never awaits any values.
--
-- Since 0.4.0
($$) :: Monad m
     => Source m a
     -> Sink a m b
     -> m b
src $$ sink = do
    (src', res) <- connectResume (checkTerminate >> src) sink
    closePipe src'
    return res
{-# INLINE ($$) #-}

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Note that this applies @checkTerminate@ to upstream to ensure upstream is
-- not run if downstream never awaits any values.
--
-- Since 0.4.0
($=) :: Monad m => Source m a -> Conduit a m b -> Source m b
src $= conduit = pipe id id (const Pure) (const Pure) (checkTerminate >> src) conduit
{-# INLINE ($=) #-}

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Note that this applies @checkTerminate@ to upstream to ensure upstream is
-- not run if downstream never awaits any values.
--
-- Since 0.4.0
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
conduit =$ sink = pipe absurd (const ()) (\c as () -> Pure as c) (\c as () -> Pure as c) (checkTerminate >> conduit) sink
{-# INLINE (=$) #-}

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Unlike @=$@, this does not apply @checkTerminate@ to upstream.
--
-- FIXME: we really need a better approach here...
--
-- Since 2.0.0
(+=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
conduit +=$ sink = pipe absurd (const ()) (\c as () -> Pure as c) (\c as () -> Pure as c) conduit sink
{-# INLINE (+=$) #-}

-- | Fusion operator, combining two @Conduit@s together into a new @Conduit@.
--
-- Note that this applies @checkTerminate@ to upstream to ensure upstream is
-- not run if downstream never awaits any values.
--
-- Since 0.4.0
(=$=) :: Monad m
      => Conduit a m b
      -> Conduit b m c
      -> Conduit a m c
up =$= down = pipe id id (const Pure) (const Pure) (checkTerminate >> up) down
{-# INLINE (=$=) #-}

-- | A @Source@ which has already started running, and which may therefore have
-- allocated resources which require finalizing.
--
-- When you use the standard composition operators on a @Source@,
-- @checkTerminate@ is used to ensure that the @Source@ is only run if
-- downstream is awaiting data. However, when dealing with an already-run
-- @Source@, we want to run finalizers even if downstream is closed. This
-- @newtype@ wrapper enforces this idea.
--
-- Of course, you're free to unwrap the @newtype@ and treat this as a normal
-- @Source@, you just need to be careful that you properly address resources.
--
-- Since 2.0.0
newtype ResumableSource m a = ResumableSource { unResumableSource :: Source m a }

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.5.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (ResumableSource m a, b)
src $$+ sink = do
    (src', b) <- connectResume (checkTerminate >> src) sink
    return (ResumableSource src', b)
{-# INLINE ($$+) #-}

-- | Continue processing after usage of @$$+@.
--
-- Since 0.5.0
($$++) :: Monad m => ResumableSource m a -> Sink a m b -> m (ResumableSource m a, b)
ResumableSource src $$++ sink = do
    (src', b) <- connectResume src sink
    return (ResumableSource src', b)
{-# INLINE ($$++) #-}

-- | Complete processing of a @ResumableSource@. This will run the finalizer
-- associated with the @ResumableSource@. In order to guarantee process resource
-- finalization, you /must/ use this operator after using @$$+@ and @$$++@.
--
-- Since 0.5.0
($$+-) :: Monad m => ResumableSource m a -> Sink a m b -> m b
ResumableSource src $$+- sink = do
    (src', res) <- connectResume src sink
    closePipe src'
    return res
{-# INLINE ($$+-) #-}

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
toProducer =
    go
  where
    go (Yield p d o) = Yield (go p) (\x _ -> go (d x ())) o
    go (Check p d) = Check (go p) (\x _ -> go (d x ()))
    go (Pure _ r) = Pure [] r
    go (Terminate _ ()) = Empty $ \_ d -> Terminate [] d
    go (M m) = M (liftM go m)
    go (Empty d) = Empty (\x _ -> go (d x ()))
    go (Await _ d) = go d

-- | Generalize a 'Sink' to a 'Consumer'.
--
-- Since 1.0.0
toConsumer :: Monad m => Sink a m b -> Consumer a m b
toConsumer =
    go
  where
    go (Yield _ _ o) = absurd o
    go (Check p d) = Check (go p) (\_ _ -> go (d [] ()))
    go (Pure is r) = Pure is r
    go (Terminate _ t) = absurd t
    go (M m) = M (liftM go m)
    go (Empty d) = Empty (\_ _ -> go (d [] ()))
    go (Await p d) = Await (go . p) (go d)
