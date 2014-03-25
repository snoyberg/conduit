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

      -- ** Primitives
    , await
    , yield
    , leftover

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

      -- * Connect-and-resume
    , ResumableSource
    , ($$+)
    , ($$++)
    , ($$+-)
    , ($=+)
    , unwrapResumable

      -- ** For @Conduit@s
    , ResumableConduit
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
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Internal hiding (await, awaitForever, yield, yieldOr, leftover, bracketP, addCleanup, transPipe, mapOutput, mapOutputMaybe, mapInput, yieldM)
import qualified Data.Conduit.Internal as CI
import Control.Monad.Morph (hoist)
import Control.Monad (liftM, forever, when, unless)
import Control.Applicative (Applicative (..))
import Data.Traversable (Traversable (..))

-- Define fixity of all our operators
infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-
infixl 1 $=+

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
-- Note: Since version 1.0.18, this operator has been generalized to be
-- identical to @=$=@.
--
-- Since 0.4.0
($=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
ConduitM src $= ConduitM con = ConduitM $ pipeL src con
{-# INLINE ($=) #-}

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Both the @Conduit@ and @Sink@ will be closed when the newly-created @Sink@
-- is closed.
--
-- Leftover data returned from the @Sink@ will be discarded.
--
-- Note: Since version 1.0.18, this operator has been generalized to be
-- identical to @=$=@.
--
-- Since 0.4.0
(=$) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
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
{-# RULES "await >>= maybe" forall x y. await >>= maybe x y = ConduitM (NeedInput (unConduitM . y) (unConduitM . const x)) #-}
{-# INLINE [1] await #-}

-- | Send a value downstream to the next component to consume. If the
-- downstream component terminates, this call will never return control. If you
-- would like to register a cleanup function, please use 'yieldOr' instead.
--
-- Since 0.5.0
yield :: Monad m
      => o -- ^ output value
      -> ConduitM i o m ()
yield = ConduitM . CI.yield
{-# INLINE [1] yield #-}

yieldM :: Monad m => m o -> ConduitM i o m ()
yieldM = ConduitM . CI.yieldM
{-# INLINE [1] yieldM #-}

{-# RULES
    "yield o >> p" forall o (p :: ConduitM i o m r). yield o >> p = ConduitM (HaveOutput (unConduitM p) (return ()) o)
  ; "mapM_ yield" mapM_ yield = ConduitM . sourceList
  ; "yieldOr o c >> p" forall o c (p :: ConduitM i o m r). yieldOr o c >> p =
        ConduitM (HaveOutput (unConduitM p) c o)
  ; "when yield next" forall b o p. when b (yield o) >> p =
        if b then ConduitM (HaveOutput (unConduitM p) (return ()) o) else p
  ; "unless yield next" forall b o p. unless b (yield o) >> p =
        if b then p else ConduitM (HaveOutput (unConduitM p) (return ()) o)
  ; "lift m >>= yield" forall m. lift m >>= yield = yieldM m
   #-}

-- | Provide a single piece of leftover input to be consumed by the next
-- component in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftover :: i -> ConduitM i o m ()
leftover = ConduitM . CI.leftover
{-# INLINE [1] leftover #-}
{-# RULES "leftover l >> p" forall l (p :: ConduitM i o m r). leftover l >> p =
    ConduitM (Leftover (unConduitM p) l) #-}

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
{-# INLINE [1] yieldOr #-}

-- | Wait for input forever, calling the given inner component for each piece of
-- new input. Returns the upstream result type.
--
-- This function is provided as a convenience for the common pattern of
-- @await@ing input, checking if it's @Just@ and then looping.
--
-- Since 0.5.0
awaitForever :: Monad m => (i -> ConduitM i o m r) -> ConduitM i o m ()
awaitForever f = ConduitM $ CI.awaitForever (unConduitM . f)

-- | Transform the monad that a @ConduitM@ lives in.
--
-- Note that the monad transforming function will be run multiple times,
-- resulting in unintuitive behavior in some cases. For a fuller treatment,
-- please see:
--
-- <https://github.com/snoyberg/conduit/wiki/Dealing-with-monad-transformers>
--
-- This function is just a synonym for 'hoist'.
--
-- Since 0.4.0
transPipe :: Monad m => (forall a. m a -> n a) -> ConduitM i o m r -> ConduitM i o n r
transPipe = hoist

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

-- | Left fusion for a resumable source.
--
-- Since 1.0.16
($=+) :: Monad m => ResumableSource m a -> Conduit a m b -> ResumableSource m b
ResumableSource src final $=+ sink = ResumableSource (src $= sink) final

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

-- | A wrapper for defining an 'Applicative' instance for 'Source's which allows
-- to combine sources together, generalizing 'zipSources'. A combined source
-- will take input yielded from each of its @Source@s until any of them stop
-- producing output.
--
-- Since 1.0.13
newtype ZipSource m o = ZipSource { getZipSource :: Source m o }

instance Monad m => Functor (ZipSource m) where
    fmap f = ZipSource . mapOutput f . getZipSource
instance Monad m => Applicative (ZipSource m) where
    pure  = ZipSource . forever . yield
    (ZipSource f) <*> (ZipSource x) = ZipSource $ zipSourcesApp f x

-- | Coalesce all values yielded by all of the @Source@s.
--
-- Implemented on top of @ZipSource@, see that data type for more details.
--
-- Since 1.0.13
sequenceSources :: (Traversable f, Monad m) => f (Source m o) -> Source m (f o)
sequenceSources = getZipSource . sequenceA . fmap ZipSource

-- | A wrapper for defining an 'Applicative' instance for 'Sink's which allows
-- to combine sinks together, generalizing 'zipSinks'. A combined sink
-- distributes the input to all its participants and when all finish, produces
-- the result. This allows to define functions like
--
-- @
-- sequenceSinks :: (Monad m)
--           => [Sink i m r] -> Sink i m [r]
-- sequenceSinks = getZipSink . sequenceA . fmap ZipSink
-- @
--
-- Note that the standard 'Applicative' instance for conduits works
-- differently. It feeds one sink with input until it finishes, then switches
-- to another, etc., and at the end combines their results.
--
-- Since 1.0.13
newtype ZipSink i m r = ZipSink { getZipSink :: Sink i m r }

instance Monad m => Functor (ZipSink i m) where
    fmap f (ZipSink x) = ZipSink (liftM f x)
instance Monad m => Applicative (ZipSink i m) where
    pure  = ZipSink . return
    (ZipSink f) <*> (ZipSink x) =
         ZipSink $ liftM (uncurry ($)) $ zipSinks f x

-- | Send incoming values to all of the @Sink@ providing, and ultimately
-- coalesce together all return values.
--
-- Implemented on top of @ZipSink@, see that data type for more details.
--
-- Since 1.0.13
sequenceSinks :: (Traversable f, Monad m) => f (Sink i m r) -> Sink i m (f r)
sequenceSinks = getZipSink . sequenceA . fmap ZipSink

-- | The connect-and-resume operator. This does not close the @Conduit@, but
-- instead returns it to be used again. This allows a @Conduit@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Leftover data returned from the @Sink@ will be discarded.
--
-- Mnemonic: connect + do more.
--
-- Since 1.0.17
(=$$+) :: Monad m => Conduit a m b -> Sink b m r -> Sink a m (ResumableConduit a m b, r)
(=$$+) conduit = connectResumeConduit (ResumableConduit conduit (return ()))
{-# INLINE (=$$+) #-}

-- | Continue processing after usage of '=$$+'. Connect a 'ResumableConduit' to
-- a sink and return the output of the sink together with a new
-- 'ResumableConduit'.
--
-- Since 1.0.17
(=$$++) :: Monad m => ResumableConduit i m o -> Sink o m r -> Sink i m (ResumableConduit i m o, r)
(=$$++) = connectResumeConduit
{-# INLINE (=$$++) #-}

-- | Complete processing of a 'ResumableConduit'. This will run the finalizer
-- associated with the @ResumableConduit@. In order to guarantee process
-- resource finalization, you /must/ use this operator after using '=$$+' and
-- '=$$++'.
--
-- Since 1.0.17
(=$$+-) :: Monad m => ResumableConduit i m o -> Sink o m r -> Sink i m r
rsrc =$$+- sink = do
    (ResumableConduit _ final, res) <- connectResumeConduit rsrc sink
    lift final
    return res
{-# INLINE (=$$+-) #-}


infixr 0 =$$+
infixr 0 =$$++
infixr 0 =$$+-

-- | Provides an alternative @Applicative@ instance for @ConduitM@. In this instance,
-- every incoming value is provided to all @ConduitM@s, and output is coalesced together.
-- Leftovers from individual @ConduitM@s will be used within that component, and then discarded
-- at the end of their computation. Output and finalizers will both be handled in a left-biased manner.
--
-- As an example, take the following program:
--
-- @
-- main :: IO ()
-- main = do
--     let src = mapM_ yield [1..3 :: Int]
--         conduit1 = CL.map (+1)
--         conduit2 = CL.concatMap (replicate 2)
--         conduit = getZipConduit $ ZipConduit conduit1 <* ZipConduit conduit2
--         sink = CL.mapM_ print
--     src $$ conduit =$ sink
-- @
--
-- It will produce the output: 2, 1, 1, 3, 2, 2, 4, 3, 3
--
-- Since 1.0.17
newtype ZipConduit i o m r = ZipConduit { getZipConduit :: ConduitM i o m r }
    deriving Functor
instance Monad m => Applicative (ZipConduit i o m) where
    pure = ZipConduit . pure
    ZipConduit left <*> ZipConduit right = ZipConduit (zipConduitApp left right)

-- | Provide identical input to all of the @Conduit@s and combine their outputs
-- into a single stream.
--
-- Implemented on top of @ZipConduit@, see that data type for more details.
--
-- Since 1.0.17
sequenceConduits :: (Traversable f, Monad m) => f (ConduitM i o m r) -> ConduitM i o m (f r)
sequenceConduits = getZipConduit . sequenceA . fmap ZipConduit
