{-# LANGUAGE RankNTypes #-}
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

      -- ** Primitives
    , await
    , yield
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
    , awaitWhile
    , awaitFold
    , awaitFoldE
    , transPipe
    , mapOutput
    , mapOutputMaybe
    , mapInput

      -- * Connect-and-resume
    , ResumableSource
    , ($$+)
    , ($$++)
    , ($$+-)
    , unwrapResumable

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
import Data.Conduit.Internal hiding (await, awaitForever, awaitWhile, awaitFold, awaitFoldE, yield, yieldOr, leftover, bracketP, addCleanup, transPipe, mapOutput, mapOutputMaybe, mapInput)
import qualified Data.Conduit.Internal as CI

-- Define fixity of all our operators
infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- If you would like to keep the @Source@ open to be used for other
-- operations, use the connect-and-resume operator '$$+'.
--
-- Since 0.4.0
($$) :: Monad m => Source m a -> Sink a m b -> m b
src $$ sink = do
    (rsrc, res) <- src $$+ sink
    rsrc $$+- return ()
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
ConduitM src $= ConduitM con = ConduitM $ pipeL src con
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
ConduitM con =$ ConduitM sink = ConduitM $ pipeL con sink
{-# INLINE (=$) #-}

-- | Fusion operator, combining two @Conduit@s together into a new @Conduit@.
--
-- Both @Conduit@s will be closed when the newly-created @Conduit@ is closed.
--
-- Leftover data returned from the right @Conduit@ will be discarded.
--
-- Since 0.4.0
(=$=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
ConduitM left =$= ConduitM right = ConduitM $ pipeL left right
{-# INLINE (=$=) #-}

-- | Wait for a single input value from upstream. If no data is available,
-- returns @Nothing@.
--
-- Since 0.5.0
await :: Monad m => Consumer i m (Maybe i)
await = ConduitM CI.await

-- | Send a value downstream to the next component to consume. If the
-- downstream component terminates, this call will never return control. If you
-- would like to register a cleanup function, please use 'yieldOr' instead.
--
-- Since 0.5.0
yield :: Monad m
      => o -- ^ output value
      -> ConduitM i o m ()
yield = ConduitM . CI.yield

-- | Provide a single piece of leftover input to be consumed by the next
-- component in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftover :: i -> ConduitM i o m ()
leftover = ConduitM . CI.leftover

-- | Perform some allocation and run an inner component. Two guarantees are
-- given about resource finalization:
--
-- 1. It will be /prompt/. The finalization will be run as early as possible.
--
-- 2. It is exception safe. Due to usage of @resourcet@, the finalization will
-- be run in the event of any exceptions.
--
-- Since 0.5.0
bracketP :: MonadResource m
         => IO a
         -> (a -> IO ())
         -> (a -> ConduitM i o m r)
         -> ConduitM i o m r
bracketP alloc free inside = ConduitM $ CI.bracketP alloc free $ unConduitM . inside

-- | Add some code to be run when the given component cleans up.
--
-- The supplied cleanup function will be given a @True@ if the component ran to
-- completion, or @False@ if it terminated early due to a downstream component
-- terminating.
--
-- Note that this function is not exception safe. For that, please use
-- 'bracketP'.
--
-- Since 0.4.1
addCleanup :: Monad m
           => (Bool -> m ())
           -> ConduitM i o m r
           -> ConduitM i o m r
addCleanup f = ConduitM . CI.addCleanup f . unConduitM

-- | Similar to 'yield', but additionally takes a finalizer to be run if the
-- downstream component terminates.
--
-- Since 0.5.0
yieldOr :: Monad m
        => o
        -> m () -- ^ finalizer
        -> ConduitM i o m ()
yieldOr o m = ConduitM $ CI.yieldOr o m

-- | Wait for input forever, calling the given inner component for each piece of
-- new input. Returns the upstream result type.
--
-- This function is provided as a convenience for the common pattern of
-- @await@ing input, checking if it's @Just@ and then looping.
--
-- Since 0.5.0
awaitForever :: Monad m => (i -> ConduitM i o m r) -> ConduitM i o m ()
awaitForever f = ConduitM $ CI.awaitForever (unConduitM . f)

-- | Similar to 'awaitForever'. In addition, allows early termination of the
-- loop.
awaitWhile :: Monad m => (i -> ConduitM i o m Bool) -> ConduitM i o m ()
awaitWhile f = ConduitM $ CI.awaitWhile (unConduitM . liftM toMaybe . f)
  where
    toMaybe True    = Nothing
    toMaybe False   = Just ()


-- | Similar to 'awaitForever', but passes an intermediate result to each next
-- repetition.
awaitFold :: Monad m => (r -> i -> ConduitM i o m r) -> r -> ConduitM i o m ()
awaitFold f = ConduitM . CI.awaitFold ((unConduitM .) . f)

-- | Similar to 'awaitFold'. In addition, allows early termination of the
-- loop.
awaitFoldE :: Monad m => (r -> i -> ConduitM i o m (Maybe r)) -> r -> ConduitM i o m ()
awaitFoldE f = ConduitM . CI.awaitFoldE (((unConduitM . liftM (maybe (Left ()) Right)) .) . f)

-- | Transform the monad that a @ConduitM@ lives in.
--
-- Note that the monad transforming function will be run multiple times,
-- resulting in unintuitive behavior in some cases. For a fuller treatment,
-- please see:
--
-- <https://github.com/snoyberg/conduit/wiki/Dealing-with-monad-transformers>
--
-- Since 0.4.0
transPipe :: Monad m => (forall a. m a -> n a) -> ConduitM i o m r -> ConduitM i o n r
transPipe f = ConduitM . CI.transPipe f . unConduitM

-- | Apply a function to all the output values of a @ConduitM@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days. It can also be simulated by fusing with the @map@ conduit from
-- "Data.Conduit.List".
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> ConduitM i o1 m r -> ConduitM i o2 m r
mapOutput f (ConduitM p) = ConduitM $ CI.mapOutput f p

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> ConduitM i o1 m r -> ConduitM i o2 m r
mapOutputMaybe f (ConduitM p) = ConduitM $ CI.mapOutputMaybe f p

-- | Apply a function to all the input values of a @ConduitM@.
--
-- Since 0.5.0
mapInput :: Monad m
         => (i1 -> i2) -- ^ map initial input to new input
         -> (i2 -> Maybe i1) -- ^ map new leftovers to initial leftovers
         -> ConduitM i2 o m r
         -> ConduitM i1 o m r
mapInput f g (ConduitM p) = ConduitM $ CI.mapInput f g p

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.5.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (ResumableSource m a, b)
src $$+ sink = connectResume (ResumableSource src (return ())) sink
{-# INLINE ($$+) #-}

-- | Continue processing after usage of @$$+@.
--
-- Since 0.5.0
($$++) :: Monad m => ResumableSource m a -> Sink a m b -> m (ResumableSource m a, b)
($$++) = connectResume
{-# INLINE ($$++) #-}

-- | Complete processing of a @ResumableSource@. This will run the finalizer
-- associated with the @ResumableSource@. In order to guarantee process resource
-- finalization, you /must/ use this operator after using @$$+@ and @$$++@.
--
-- Since 0.5.0
($$+-) :: Monad m => ResumableSource m a -> Sink a m b -> m b
rsrc $$+- sink = do
    (ResumableSource _ final, res) <- connectResume rsrc sink
    final
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
