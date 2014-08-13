{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Data.Conduit.Internal.Conduit
    ( -- ** Types
      ConduitM (..)
    , Source
    , Producer
    , Sink
    , Consumer
    , Conduit
    , ResumableSource (..)
    , ResumableConduit (..)
    , Flush (..)
      -- *** Newtype wrappers
    , ZipSource (..)
    , ZipSink (..)
    , ZipConduit (..)
      -- ** Primitives
    , await
    , awaitForever
    , yield
    , yieldM
    , yieldOr
    , leftover
    , runConduit
      -- ** Composition
    , connectResume
    , connectResumeConduit
    , fuseLeftovers
    , fuseReturnLeftovers
    , ($$+)
    , ($$++)
    , ($$+-)
    , ($=+)
    , (=$$+)
    , (=$$++)
    , (=$$+-)
    , ($$)
    , ($=)
    , (=$)
    , (=$=)
      -- ** Generalizing
    , sourceToPipe
    , sinkToPipe
    , conduitToPipe
    , toProducer
    , toConsumer
      -- ** Cleanup
    , Data.Conduit.Internal.Conduit.bracketP
    , Data.Conduit.Internal.Conduit.addCleanup
      -- ** Exceptions
    , catchC
    , handleC
    , tryC
      -- ** Utilities
    , Data.Conduit.Internal.Conduit.transPipe
    , Data.Conduit.Internal.Conduit.mapOutput
    , Data.Conduit.Internal.Conduit.mapOutputMaybe
    , Data.Conduit.Internal.Conduit.mapInput
    , Data.Conduit.Internal.Conduit.closeResumableSource
    , unwrapResumable
    , unwrapResumableConduit
    , newResumableSource
    , newResumableConduit
    , zipSinks
    , zipSources
    , zipSourcesApp
    , zipConduitApp
    , passthroughSink
    , fuseBoth
    , fuseUpstream
    , sequenceSources
    , sequenceSinks
    , sequenceConduits
    ) where

import Control.Applicative (Applicative (..))
import Control.Exception.Lifted as E (Exception)
import Control.Monad (liftM, when, liftM2)
import Control.Monad.Error.Class(MonadError(..))
import Control.Monad.Reader.Class(MonadReader(..))
import Control.Monad.RWS.Class(MonadRWS())
import Control.Monad.Writer.Class(MonadWriter(..))
import Control.Monad.State.Class(MonadState(..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Base (MonadBase (liftBase))
import Data.Void (Void, absurd)
import Data.Monoid (Monoid (mappend, mempty))
import Control.Monad.Trans.Resource
import qualified Data.IORef as I
import Control.Monad.Morph (MFunctor (..))
import Data.Conduit.Internal.Pipe hiding (yield, mapOutput, leftover, yieldM, yieldOr, await, awaitForever)
import qualified Data.Conduit.Internal.Pipe as CI
import Control.Monad (forever, unless)
import Data.Traversable (Traversable (..))
#if MIN_VERSION_exceptions(0, 6, 0)
import qualified Control.Monad.Catch as Catch
#endif

-- | Core datatype of the conduit package. This type represents a general
-- component which can consume a stream of input values @i@, produce a stream
-- of output values @o@, perform actions in the @m@ monad, and produce a final
-- result @r@. The type synonyms provided here are simply wrappers around this
-- type.
--
-- Since 1.0.0
newtype ConduitM i o m r = ConduitM { unConduitM :: Pipe i i o () m r }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MFunctor
#if MIN_VERSION_exceptions(0, 6, 0)
    , Catch.MonadCatch
#endif
    )

instance MonadReader r m => MonadReader r (ConduitM i o m) where
    ask = ConduitM ask
    local f (ConduitM m) = ConduitM (local f m)

instance MonadWriter w m => MonadWriter w (ConduitM i o m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = ConduitM . writer
#endif
    tell = ConduitM . tell
    listen (ConduitM m) = ConduitM (listen m)
    pass (ConduitM m) = ConduitM (pass m)

instance MonadState s m => MonadState s (ConduitM i o m) where
    get = ConduitM get
    put = ConduitM . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = ConduitM . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (ConduitM i o m)

instance MonadError e m => MonadError e (ConduitM i o m) where
    throwError = ConduitM . throwError
    catchError (ConduitM m) f = ConduitM $ catchError m (unConduitM . f)

instance MonadBase base m => MonadBase base (ConduitM i o m) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

instance MonadTrans (ConduitM i o) where
    lift mr = ConduitM (PipeM (Done `liftM` mr))
    {-# INLINE [1] lift #-}

instance MonadResource m => MonadResource (ConduitM i o m) where
    liftResourceT = lift . liftResourceT
    {-# INLINE liftResourceT #-}

instance Monad m => Monoid (ConduitM i o m ()) where
    mempty = return ()
    {-# INLINE mempty #-}
    mappend = (>>)
    {-# INLINE mappend #-}

-- | Provides a stream of output values, without consuming any input or
-- producing a final result.
--
-- Since 0.5.0
type Source m o = ConduitM () o m ()

-- | A component which produces a stream of output values, regardless of the
-- input stream. A @Producer@ is a generalization of a @Source@, and can be
-- used as either a @Source@ or a @Conduit@.
--
-- Since 1.0.0
type Producer m o = forall i. ConduitM i o m ()

-- | Consumes a stream of input values and produces a final result, without
-- producing any output.
--
-- > type Sink i m r = ConduitM i Void m r
--
-- Since 0.5.0
type Sink i = ConduitM i Void

-- | A component which consumes a stream of input values and produces a final
-- result, regardless of the output stream. A @Consumer@ is a generalization of
-- a @Sink@, and can be used as either a @Sink@ or a @Conduit@.
--
-- Since 1.0.0
type Consumer i m r = forall o. ConduitM i o m r

-- | Consumes a stream of input values and produces a stream of output values,
-- without producing a final result.
--
-- Since 0.5.0
type Conduit i m o = ConduitM i o m ()

-- | A @Source@ which has been started, but has not yet completed.
--
-- This type contains both the current state of the @Source@, and the finalizer
-- to be run to close it.
--
-- Since 0.5.0
data ResumableSource m o = ResumableSource (Source m o) (m ())

-- | Since 1.0.13
instance MFunctor ResumableSource where
    hoist nat (ResumableSource src m) = ResumableSource (hoist nat src) (nat m)

-- | Connect a @Source@ to a @Sink@ until the latter closes. Returns both the
-- most recent state of the @Source@ and the result of the @Sink@.
--
-- We use a @ResumableSource@ to keep track of the most recent finalizer
-- provided by the @Source@.
--
-- Since 0.5.0
connectResume :: Monad m
              => ResumableSource m o
              -> Sink o m r
              -> m (ResumableSource m o, r)
connectResume (ResumableSource (ConduitM left0) leftFinal0) (ConduitM right0) =
    goRight leftFinal0 left0 right0
  where
    goRight leftFinal left right =
        case right of
            HaveOutput _ _ o -> absurd o
            NeedInput rp rc  -> goLeft rp rc leftFinal left
            Done r2          -> return (ResumableSource (ConduitM left) leftFinal, r2)
            PipeM mp         -> mp >>= goRight leftFinal left
            Leftover p i     -> goRight leftFinal (HaveOutput left leftFinal i) p

    goLeft rp rc leftFinal left =
        case left of
            HaveOutput left' leftFinal' o -> goRight leftFinal' left' (rp o)
            NeedInput _ lc                -> recurse (lc ())
            Done ()                       -> goRight (return ()) (Done ()) (rc ())
            PipeM mp                      -> mp >>= recurse
            Leftover p ()                 -> recurse p
      where
        recurse = goLeft rp rc leftFinal

sourceToPipe :: Monad m => Source m o -> Pipe l i o u m ()
sourceToPipe =
    go . unConduitM
  where
    go (HaveOutput p c o) = HaveOutput (go p) c o
    go (NeedInput _ c) = go $ c ()
    go (Done ()) = Done ()
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p ()) = go p

sinkToPipe :: Monad m => Sink i m r -> Pipe l i o u m r
sinkToPipe =
    go . injectLeftovers . unConduitM
  where
    go (HaveOutput _ _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (const $ go $ c ())
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover _ l) = absurd l

conduitToPipe :: Monad m => Conduit i m o -> Pipe l i o u m ()
conduitToPipe =
    go . injectLeftovers . unConduitM
  where
    go (HaveOutput p c o) = HaveOutput (go p) c o
    go (NeedInput p c) = NeedInput (go . p) (const $ go $ c ())
    go (Done ()) = Done ()
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover _ l) = absurd l

-- | Unwraps a @ResumableSource@ into a @Source@ and a finalizer.
--
-- A @ResumableSource@ represents a @Source@ which has already been run, and
-- therefore has a finalizer registered. As a result, if we want to turn it
-- into a regular @Source@, we need to ensure that the finalizer will be run
-- appropriately. By appropriately, I mean:
--
-- * If a new finalizer is registered, the old one should not be called.
--
-- * If the old one is called, it should not be called again.
--
-- This function returns both a @Source@ and a finalizer which ensures that the
-- above two conditions hold. Once you call that finalizer, the @Source@ is
-- invalidated and cannot be used.
--
-- Since 0.5.2
unwrapResumable :: MonadIO m => ResumableSource m o -> m (Source m o, m ())
unwrapResumable (ResumableSource src final) = do
    ref <- liftIO $ I.newIORef True
    let final' = do
            x <- liftIO $ I.readIORef ref
            when x final
    return (liftIO (I.writeIORef ref False) >> src, final')

-- | Turn a @Source@ into a @ResumableSource@ with no attached finalizer.
--
-- Since 1.1.4
newResumableSource :: Monad m => Source m o -> ResumableSource m o
newResumableSource s = ResumableSource s (return ())

-- | Generalize a 'Source' to a 'Producer'.
--
-- Since 1.0.0
toProducer :: Monad m => Source m a -> Producer m a
toProducer =
    ConduitM . go . unConduitM
  where
    go (HaveOutput p c o) = HaveOutput (go p) c o
    go (NeedInput _ c) = go (c ())
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p ()) = go p

-- | Generalize a 'Sink' to a 'Consumer'.
--
-- Since 1.0.0
toConsumer :: Monad m => Sink a m b -> Consumer a m b
toConsumer =
    ConduitM . go . unConduitM
  where
    go (HaveOutput _ _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p l) = Leftover (go p) l

-- | Catch all exceptions thrown by the current component of the pipeline.
--
-- Note: this will /not/ catch exceptions thrown by other components! For
-- example, if an exception is thrown in a @Source@ feeding to a @Sink@, and
-- the @Sink@ uses @catchC@, the exception will /not/ be caught.
--
-- Due to this behavior (as well as lack of async exception handling), you
-- should not try to implement combinators such as @onException@ in terms of this
-- primitive function.
--
-- Note also that the exception handling will /not/ be applied to any
-- finalizers generated by this conduit.
--
-- Since 1.0.11
catchC :: (MonadBaseControl IO m, Exception e)
       => ConduitM i o m r
       -> (e -> ConduitM i o m r)
       -> ConduitM i o m r
catchC (ConduitM p) f = ConduitM (catchP p (unConduitM . f))
{-# INLINE catchC #-}

-- | The same as @flip catchC@.
--
-- Since 1.0.11
handleC :: (MonadBaseControl IO m, Exception e)
        => (e -> ConduitM i o m r)
        -> ConduitM i o m r
        -> ConduitM i o m r
handleC = flip catchC
{-# INLINE handleC #-}

-- | A version of @try@ for use within a pipeline. See the comments in @catchC@
-- for more details.
--
-- Since 1.0.11
tryC :: (MonadBaseControl IO m, Exception e)
     => ConduitM i o m r
     -> ConduitM i o m (Either e r)
tryC = ConduitM . tryP . unConduitM
{-# INLINE tryC #-}

-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- Any leftovers are discarded.
--
-- Since 0.4.1
zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
zipSinks (ConduitM x0) (ConduitM y0) =
    ConduitM $ injectLeftovers x0 >< injectLeftovers y0
  where
    (><) :: Monad m => Pipe Void i Void () m r1 -> Pipe Void i Void () m r2 -> Pipe l i o () m (r1, r2)

    Leftover _  i    >< _                = absurd i
    _                >< Leftover _  i    = absurd i
    HaveOutput _ _ o >< _                = absurd o
    _                >< HaveOutput _ _ o = absurd o

    PipeM mx         >< y                = PipeM (liftM (>< y) mx)
    x                >< PipeM my         = PipeM (liftM (x ><) my)
    Done x           >< Done y           = Done (x, y)
    NeedInput px cx  >< NeedInput py cy  = NeedInput (\i -> px i >< py i) (\() -> cx () >< cy ())
    NeedInput px cx  >< y@Done{}         = NeedInput (\i -> px i >< y)    (\u -> cx u >< y)
    x@Done{}         >< NeedInput py cy  = NeedInput (\i -> x >< py i)    (\u -> x >< cy u)

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 1.0.13
zipSources :: Monad m => Source m a -> Source m b -> Source m (a, b)
zipSources (ConduitM left0) (ConduitM right0) =
    ConduitM $ go left0 right0
  where
    go (Leftover left ()) right = go left right
    go left (Leftover right ())  = go left right
    go (Done ()) (Done ()) = Done ()
    go (Done ()) (HaveOutput _ close _) = PipeM (close >> return (Done ()))
    go (HaveOutput _ close _) (Done ()) = PipeM (close >> return (Done ()))
    go (Done ()) (PipeM _) = Done ()
    go (PipeM _) (Done ()) = Done ()
    go (PipeM mx) (PipeM my) = PipeM (liftM2 go mx my)
    go (PipeM mx) y@HaveOutput{} = PipeM (liftM (\x -> go x y) mx)
    go x@HaveOutput{} (PipeM my) = PipeM (liftM (go x) my)
    go (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (go srcx srcy) (closex >> closey) (x, y)
    go (NeedInput _ c) right = go (c ()) right
    go left (NeedInput _ c) = go left (c ())

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 1.0.13
zipSourcesApp :: Monad m => Source m (a -> b) -> Source m a -> Source m b
zipSourcesApp (ConduitM left0) (ConduitM right0) =
    ConduitM $ go left0 right0
  where
    go (Leftover left ()) right = go left right
    go left (Leftover right ())  = go left right
    go (Done ()) (Done ()) = Done ()
    go (Done ()) (HaveOutput _ close _) = PipeM (close >> return (Done ()))
    go (HaveOutput _ close _) (Done ()) = PipeM (close >> return (Done ()))
    go (Done ()) (PipeM _) = Done ()
    go (PipeM _) (Done ()) = Done ()
    go (PipeM mx) (PipeM my) = PipeM (liftM2 go mx my)
    go (PipeM mx) y@HaveOutput{} = PipeM (liftM (\x -> go x y) mx)
    go x@HaveOutput{} (PipeM my) = PipeM (liftM (go x) my)
    go (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (go srcx srcy) (closex >> closey) (x y)
    go (NeedInput _ c) right = go (c ()) right
    go left (NeedInput _ c) = go left (c ())

-- |
--
-- Since 1.0.17
zipConduitApp
    :: Monad m
    => ConduitM i o m (x -> y)
    -> ConduitM i o m x
    -> ConduitM i o m y
zipConduitApp (ConduitM left0) (ConduitM right0) =
    ConduitM $ go (return ()) (return ()) (injectLeftovers left0) (injectLeftovers right0)
  where
    go _ _ (Done f) (Done x) = Done (f x)
    go _ finalY (HaveOutput x finalX o) y = HaveOutput
        (go finalX finalY x y)
        (finalX >> finalY)
        o
    go finalX _ x (HaveOutput y finalY o) = HaveOutput
        (go finalX finalY x y)
        (finalX >> finalY)
        o
    go _ _ (Leftover _ i) _ = absurd i
    go _ _ _ (Leftover _ i) = absurd i
    go finalX finalY (PipeM mx) y = PipeM (flip (go finalX finalY) y `liftM` mx)
    go finalX finalY x (PipeM my) = PipeM (go finalX finalY x `liftM` my)
    go finalX finalY (NeedInput px cx) (NeedInput py cy) = NeedInput
        (\i -> go finalX finalY (px i) (py i))
        (\u -> go finalX finalY (cx u) (cy u))
    go finalX finalY (NeedInput px cx) (Done y) = NeedInput
        (\i -> go finalX finalY (px i) (Done y))
        (\u -> go finalX finalY (cx u) (Done y))
    go finalX finalY (Done x) (NeedInput py cy) = NeedInput
        (\i -> go finalX finalY (Done x) (py i))
        (\u -> go finalX finalY (Done x) (cy u))

-- | Same as normal fusion (e.g. @=$=@), except instead of discarding leftovers
-- from the downstream component, return them.
--
-- Since 1.0.17
fuseReturnLeftovers :: Monad m
                    => ConduitM a b m ()
                    -> ConduitM b c m r
                    -> ConduitM a c m (r, [b])
fuseReturnLeftovers (ConduitM left0) (ConduitM right0) =
    ConduitM $ goRight (return ()) [] left0 right0
  where
    goRight final bs left right =
        case right of
            HaveOutput p c o -> HaveOutput (recurse p) (c >> final) o
            NeedInput rp rc  ->
                case bs of
                    [] -> goLeft rp rc final left
                    b:bs' -> goRight final bs' left (rp b)
            Done r2          -> PipeM (final >> return (Done (r2, bs)))
            PipeM mp         -> PipeM (liftM recurse mp)
            Leftover p b     -> goRight final (b:bs) left p
      where
        recurse = goRight final bs left

    goLeft rp rc final left =
        case left of
            HaveOutput left' final' o -> goRight final' [] left' (rp o)
            NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
            Done r1                   -> goRight (return ()) [] (Done r1) (rc r1)
            PipeM mp                  -> PipeM (liftM recurse mp)
            Leftover left' i          -> Leftover (recurse left') i
      where
        recurse = goLeft rp rc final

-- | Similar to @fuseReturnLeftovers@, but use the provided function to convert
-- downstream leftovers to upstream leftovers.
--
-- Since 1.0.17
fuseLeftovers
    :: Monad m
    => ([b] -> [a])
    -> ConduitM a b m ()
    -> ConduitM b c m r
    -> ConduitM a c m r
fuseLeftovers f left right = do
    (r, bs) <- fuseReturnLeftovers left right
    mapM_ leftover $ reverse $ f bs
    return r

-- | A generalization of 'ResumableSource'. Allows to resume an arbitrary
-- conduit, keeping its state and using it later (or finalizing it).
--
-- Since 1.0.17
data ResumableConduit i m o =
    ResumableConduit (Conduit i m o) (m ())

-- | Connect a 'ResumableConduit' to a sink and return the output of the sink
-- together with a new 'ResumableConduit'.
--
-- Since 1.0.17
connectResumeConduit
    :: Monad m
    => ResumableConduit i m o
    -> Sink o m r
    -> Sink i m (ResumableConduit i m o, r)
connectResumeConduit (ResumableConduit (ConduitM left0) leftFinal0) (ConduitM right0) =
    ConduitM $ goRight leftFinal0 left0 right0
  where
    goRight leftFinal left right =
        case right of
            HaveOutput _ _ o -> absurd o
            NeedInput rp rc -> goLeft rp rc leftFinal left
            Done r2 -> Done (ResumableConduit (ConduitM left) leftFinal, r2)
            PipeM mp -> PipeM (liftM (goRight leftFinal left) mp)
            Leftover p i -> goRight leftFinal (HaveOutput left leftFinal i) p

    goLeft rp rc leftFinal left =
        case left of
            HaveOutput left' leftFinal' o -> goRight leftFinal' left' (rp o)
            NeedInput left' lc -> NeedInput (recurse . left') (recurse . lc)
            Done () -> goRight (return ()) (Done ()) (rc ())
            PipeM mp -> PipeM (liftM recurse mp)
            Leftover left' i -> Leftover (recurse left') i -- recurse p
      where
        recurse = goLeft rp rc leftFinal

-- | Unwraps a @ResumableConduit@ into a @Conduit@ and a finalizer.
--
-- Since 'unwrapResumable' for more information.
--
-- Since 1.0.17
unwrapResumableConduit :: MonadIO m => ResumableConduit i m o -> m (Conduit i m o, m ())
unwrapResumableConduit (ResumableConduit src final) = do
    ref <- liftIO $ I.newIORef True
    let final' = do
            x <- liftIO $ I.readIORef ref
            when x final
    return (liftIO (I.writeIORef ref False) >> src, final')

-- | Turn a @Conduit@ into a @ResumableConduit@ with no attached finalizer.
--
-- Since 1.1.4
newResumableConduit :: Monad m => Conduit i m o -> ResumableConduit i m o
newResumableConduit c = ResumableConduit c (return ())

-- | Turn a @Sink@ into a @Conduit@ in the following way:
--
-- * All input passed to the @Sink@ is yielded downstream.
--
-- * When the @Sink@ finishes processing, the result is passed to the provided to the finalizer function.
--
-- Note that the @Sink@ will stop receiving input as soon as the downstream it
-- is connected to shuts down.
--
-- An example usage would be to write the result of a @Sink@ to some mutable
-- variable while allowing other processing to continue.
--
-- Since 1.1.0
passthroughSink :: Monad m
                => Sink i m r
                -> (r -> m ()) -- ^ finalizer
                -> Conduit i m i
passthroughSink (ConduitM sink0) final =
    ConduitM $ go [] sink0
  where
    go _ (Done r) = do
        lift $ final r
        CI.awaitForever CI.yield
    go is (Leftover sink i) = go (i:is) sink
    go _ (HaveOutput _ _ o) = absurd o
    go is (PipeM mx) = do
        x <- lift mx
        go is x
    go (i:is) (NeedInput next _) = go is (next i)
    go [] (NeedInput next done) = do
        mx <- CI.await
        case mx of
            Nothing -> go [] (done ())
            Just x -> do
                CI.yield x
                go [] (next x)

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
{-# INLINE [1] ($$) #-}

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
($=) = (=$=)
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
(=$) = (=$=)
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
{-# INLINE [1] (=$=) #-}

-- | Wait for a single input value from upstream. If no data is available,
-- returns @Nothing@.
--
-- Since 0.5.0
await :: Monad m => Consumer i m (Maybe i)
await = ConduitM CI.await
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

  -- FIXME rule won't fire, see FIXME in .Pipe; "mapM_ yield" mapM_ yield = ConduitM . sourceList

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

-- | Run a pipeline until processing completes.
--
-- Since 1.1.8
runConduit :: Monad m => ConduitM () Void m r -> m r
runConduit (ConduitM p) = runPipe (injectLeftovers p)
{-# INLINE runConduit #-}

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

-- | Execute the finalizer associated with a @ResumableSource@, rendering the
-- @ResumableSource@ invalid for further use.
--
-- This is just a more explicit version of @$$+- return ()@.
--
-- Since 1.1.3
closeResumableSource :: Monad m => ResumableSource m a -> m ()
closeResumableSource = ($$+- return ())

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

-- | Fuse two @ConduitM@s together, and provide the return value of both. Note
-- that this will force the entire upstream @ConduitM@ to be run to produce the
-- result value, even if the downstream terminates early.
--
-- Since 1.1.5
fuseBoth :: Monad m => ConduitM a b m r1 -> ConduitM b c m r2 -> ConduitM a c m (r1, r2)
fuseBoth (ConduitM up) (ConduitM down) =
    ConduitM $ pipeL up (withUpstream $ generalizeUpstream down)
{-# INLINE fuseBoth #-}

-- | Same as @fuseBoth@, but ignore the return value from the downstream
-- @Conduit@. Same caveats of forced consumption apply.
--
-- Since 1.1.5
fuseUpstream :: Monad m => ConduitM a b m r -> Conduit b m c -> ConduitM a c m r
fuseUpstream up down = fmap fst (fuseBoth up down)
{-# INLINE fuseUpstream #-}

-- Rewrite rules

{-# RULES "ConduitM: lift x >>= f" forall m f. lift m >>= f = ConduitM (PipeM (liftM (unConduitM . f) m)) #-}
{-# RULES "ConduitM: lift x >> f" forall m f. lift m >> f = ConduitM (PipeM (liftM (\_ -> unConduitM f) m)) #-}
{-# RULES "await >>= maybe" forall x y. await >>= maybe x y = ConduitM (NeedInput (unConduitM . y) (unConduitM . const x)) #-}
{-# RULES
    "yield o >> p" forall o (p :: ConduitM i o m r). yield o >> p = ConduitM (HaveOutput (unConduitM p) (return ()) o)
  ; "yieldOr o c >> p" forall o c (p :: ConduitM i o m r). yieldOr o c >> p =
        ConduitM (HaveOutput (unConduitM p) c o)
  ; "when yield next" forall b o p. when b (yield o) >> p =
        if b then ConduitM (HaveOutput (unConduitM p) (return ()) o) else p
  ; "unless yield next" forall b o p. unless b (yield o) >> p =
        if b then p else ConduitM (HaveOutput (unConduitM p) (return ()) o)
  ; "lift m >>= yield" forall m. lift m >>= yield = yieldM m
   #-}
{-# RULES "leftover l >> p" forall l (p :: ConduitM i o m r). leftover l >> p =
    ConduitM (Leftover (unConduitM p) l) #-}
