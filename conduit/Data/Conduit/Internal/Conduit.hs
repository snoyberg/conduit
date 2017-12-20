{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Conduit.Internal.Conduit
    ( -- ** Types
      ConduitT (..)
    , ConduitM
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
    , fuse
    , connect
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
    , (.|)
      -- ** Generalizing
    , sourceToPipe
    , sinkToPipe
    , conduitToPipe
    , toProducer
    , toConsumer
      -- ** Cleanup
    , bracketP
    , addCleanup
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
    , mergeSource
    , passthroughSink
    , sourceToList
    , fuseBoth
    , fuseBothMaybe
    , fuseUpstream
    , sequenceSources
    , sequenceSinks
    , sequenceConduits
    ) where

import Control.Applicative (Applicative (..))
import Control.Exception (Exception)
import qualified Control.Exception as E (catch)
import Control.Monad (liftM, when, liftM2, ap)
import Control.Monad.Error.Class(MonadError(..))
import Control.Monad.Reader.Class(MonadReader(..))
import Control.Monad.RWS.Class(MonadRWS())
import Control.Monad.Writer.Class(MonadWriter(..), censor)
import Control.Monad.State.Class(MonadState(..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO, withRunInIO)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Data.Void (Void, absurd)
import Data.Monoid (Monoid (mappend, mempty))
import Control.Monad.Trans.Resource
import qualified Data.IORef as I
import Data.Conduit.Internal.Pipe hiding (yield, mapOutput, leftover, yieldM, yieldOr, await, awaitForever, addCleanup, bracketP)
import qualified Data.Conduit.Internal.Pipe as CI
import Control.Monad (forever)
import Data.Traversable (Traversable (..))

-- | Core datatype of the conduit package. This type represents a general
-- component which can consume a stream of input values @i@, produce a stream
-- of output values @o@, perform actions in the @m@ monad, and produce a final
-- result @r@. The type synonyms provided here are simply wrappers around this
-- type.
--
-- Since 1.0.0
newtype ConduitT i o m r = ConduitT
    { unConduitT :: forall b.
                    (r -> Pipe i i o () m b) -> Pipe i i o () m b
    }

-- | Same as 'ConduitT', for backwards compat
type ConduitM = ConduitT

instance Functor (ConduitT i o m) where
    fmap f (ConduitT c) = ConduitT $ \rest -> c (rest . f)

instance Applicative (ConduitT i o m) where
    pure x = ConduitT ($ x)
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad (ConduitT i o m) where
    return = pure
    ConduitT f >>= g = ConduitT $ \h -> f $ \a -> unConduitT (g a) h

instance MonadThrow m => MonadThrow (ConduitT i o m) where
    throwM = lift . throwM

instance MonadIO m => MonadIO (ConduitT i o m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadReader r m => MonadReader r (ConduitT i o m) where
    ask = lift ask
    {-# INLINE ask #-}

    local f (ConduitT c0) = ConduitT $ \rest ->
        let go (HaveOutput p c o) = HaveOutput (go p) c o
            go (NeedInput p c) = NeedInput (\i -> go (p i)) (\u -> go (c u))
            go (Done x) = rest x
            go (PipeM mp) = PipeM (liftM go $ local f mp)
            go (Leftover p i) = Leftover (go p) i
         in go (c0 Done)

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x, y, z) 0
#endif

instance MonadWriter w m => MonadWriter w (ConduitT i o m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif
    tell = lift . tell

    listen (ConduitT c0) = ConduitT $ \rest ->
        let go front (HaveOutput p c o) = HaveOutput (go front p) c o
            go front (NeedInput p c) = NeedInput (\i -> go front (p i)) (\u -> go front (c u))
            go front (Done x) = rest (x, front)
            go front (PipeM mp) = PipeM $ do
                (p,w) <- listen mp
                return $ go (front `mappend` w) p
            go front (Leftover p i) = Leftover (go front p) i
         in go mempty (c0 Done)

    pass (ConduitT c0) = ConduitT $ \rest ->
        let go front (HaveOutput p c o) = HaveOutput (go front p) c o
            go front (NeedInput p c) = NeedInput (\i -> go front (p i)) (\u -> go front (c u))
            go front (PipeM mp) = PipeM $ do
                (p,w) <- censor (const mempty) (listen mp)
                return $ go (front `mappend` w) p
            go front (Done (x,f)) = PipeM $ do
                tell (f front)
                return $ rest x
            go front (Leftover p i) = Leftover (go front p) i
         in go mempty (c0 Done)

instance MonadState s m => MonadState s (ConduitT i o m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (ConduitT i o m)

instance MonadError e m => MonadError e (ConduitT i o m) where
    throwError = lift . throwError
    catchError (ConduitT c0) f = ConduitT $ \rest ->
        let go (HaveOutput p c o) = HaveOutput (go p) c o
            go (NeedInput p c) = NeedInput (\i -> go (p i)) (\u -> go (c u))
            go (Done x) = rest x
            go (PipeM mp) =
              PipeM $ catchError (liftM go mp) $ \e -> do
                return $ unConduitT (f e) rest
            go (Leftover p i) = Leftover (go p) i
         in go (c0 Done)

instance MonadTrans (ConduitT i o) where
    lift mr = ConduitT $ \rest -> PipeM (liftM rest mr)
    {-# INLINE [1] lift #-}

instance MonadResource m => MonadResource (ConduitT i o m) where
    liftResourceT = lift . liftResourceT
    {-# INLINE liftResourceT #-}

instance Monad m => Monoid (ConduitT i o m ()) where
    mempty = return ()
    {-# INLINE mempty #-}
    mappend = (>>)
    {-# INLINE mappend #-}

instance PrimMonad m => PrimMonad (ConduitT i o m) where
  type PrimState (ConduitT i o m) = PrimState m
  primitive = lift . primitive

-- | Provides a stream of output values, without consuming any input or
-- producing a final result.
--
-- Since 0.5.0
type Source m o = ConduitT () o m ()
{-# DEPRECATED Source "Use ConduitT directly" #-}

-- | A component which produces a stream of output values, regardless of the
-- input stream. A @Producer@ is a generalization of a @Source@, and can be
-- used as either a @Source@ or a @Conduit@.
--
-- Since 1.0.0
type Producer m o = forall i. ConduitT i o m ()
{-# DEPRECATED Producer "Use ConduitT directly" #-}

-- | Consumes a stream of input values and produces a final result, without
-- producing any output.
--
-- > type Sink i m r = ConduitT i Void m r
--
-- Since 0.5.0
type Sink i = ConduitT i Void
{-# DEPRECATED Sink "Use ConduitT directly" #-}

-- | A component which consumes a stream of input values and produces a final
-- result, regardless of the output stream. A @Consumer@ is a generalization of
-- a @Sink@, and can be used as either a @Sink@ or a @Conduit@.
--
-- Since 1.0.0
type Consumer i m r = forall o. ConduitT i o m r
{-# DEPRECATED Consumer "Use ConduitT directly" #-}

-- | Consumes a stream of input values and produces a stream of output values,
-- without producing a final result.
--
-- Since 0.5.0
type Conduit i m o = ConduitT i o m ()
{-# DEPRECATED Conduit "Use ConduitT directly" #-}

-- | A @Source@ which has been started, but has not yet completed.
--
-- This type contains both the current state of the @Source@, and the finalizer
-- to be run to close it.
--
-- Since 0.5.0
data ResumableSource m o = ResumableSource (Pipe () () o () m ()) (m ())

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
connectResume (ResumableSource left0 leftFinal0) (ConduitT right0) =
    goRight leftFinal0 left0 (right0 Done)
  where
    goRight leftFinal left right =
        case right of
            HaveOutput _ _ o -> absurd o
            NeedInput rp rc  -> goLeft rp rc leftFinal left
            Done r2          -> return (ResumableSource left leftFinal, r2)
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
    go . flip unConduitT Done
  where
    go (HaveOutput p c o) = HaveOutput (go p) c o
    go (NeedInput _ c) = go $ c ()
    go (Done ()) = Done ()
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p ()) = go p

sinkToPipe :: Monad m => Sink i m r -> Pipe l i o u m r
sinkToPipe =
    go . injectLeftovers . flip unConduitT Done
  where
    go (HaveOutput _ _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (const $ go $ c ())
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover _ l) = absurd l

conduitToPipe :: Monad m => Conduit i m o -> Pipe l i o u m ()
conduitToPipe =
    go . injectLeftovers . flip unConduitT Done
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
    return (liftIO (I.writeIORef ref False) >> (ConduitT (src >>=)), final')

-- | Turn a @Source@ into a @ResumableSource@ with no attached finalizer.
--
-- Since 1.1.4
newResumableSource :: Monad m => Source m o -> ResumableSource m o
newResumableSource (ConduitT s) = ResumableSource (s Done) (return ())

-- | Generalize a 'Source' to a 'Producer'.
--
-- Since 1.0.0
toProducer :: Monad m => Source m a -> Producer m a
toProducer (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p c o) = HaveOutput (go p) c o
    go (NeedInput _ c) = go (c ())
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p ()) = go p
    in go (c0 Done)

-- | Generalize a 'Sink' to a 'Consumer'.
--
-- Since 1.0.0
toConsumer :: Monad m => Sink a m b -> Consumer a m b
toConsumer (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput _ _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p l) = Leftover (go p) l
    in go (c0 Done)

-- | Catch all exceptions thrown by the current component of the pipeline.
--
-- Note: this will /not/ catch exceptions thrown by other components! For
-- example, if an exception is thrown in a @Source@ feeding to a @Sink@, and
-- the @Sink@ uses @catchC@, the exception will /not/ be caught.
--
-- Due to this behavior (as well as lack of async exception safety), you
-- should not try to implement combinators such as @onException@ in terms of this
-- primitive function.
--
-- Note also that the exception handling will /not/ be applied to any
-- finalizers generated by this conduit.
--
-- Since 1.0.11
catchC :: (MonadUnliftIO m, Exception e)
       => ConduitT i o m r
       -> (e -> ConduitT i o m r)
       -> ConduitT i o m r
catchC (ConduitT p0) onErr = ConduitT $ \rest -> let
    go (Done r) = rest r
    go (PipeM mp) = PipeM $ withRunInIO $ \run -> E.catch (run (liftM go mp))
        (return . flip unConduitT rest . onErr)
    go (Leftover p i) = Leftover (go p) i
    go (NeedInput x y) = NeedInput (go . x) (go . y)
    go (HaveOutput p c o) = HaveOutput (go p) c o
    in go (p0 Done)
{-# INLINE catchC #-}

-- | The same as @flip catchC@.
--
-- Since 1.0.11
handleC :: (MonadUnliftIO m, Exception e)
        => (e -> ConduitT i o m r)
        -> ConduitT i o m r
        -> ConduitT i o m r
handleC = flip catchC
{-# INLINE handleC #-}

-- | A version of @try@ for use within a pipeline. See the comments in @catchC@
-- for more details.
--
-- Since 1.0.11
tryC :: (MonadUnliftIO m, Exception e)
     => ConduitT i o m r
     -> ConduitT i o m (Either e r)
tryC c = fmap Right c `catchC` (return . Left)
{-# INLINE tryC #-}

-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- Any leftovers are discarded.
--
-- Since 0.4.1
zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
zipSinks (ConduitT x0) (ConduitT y0) = ConduitT $ \rest -> let
    Leftover _  i    >< _                = absurd i
    _                >< Leftover _  i    = absurd i
    HaveOutput _ _ o >< _                = absurd o
    _                >< HaveOutput _ _ o = absurd o

    PipeM mx         >< y                = PipeM (liftM (>< y) mx)
    x                >< PipeM my         = PipeM (liftM (x ><) my)
    Done x           >< Done y           = rest (x, y)
    NeedInput px cx  >< NeedInput py cy  = NeedInput (\i -> px i >< py i) (\() -> cx () >< cy ())
    NeedInput px cx  >< y@Done{}         = NeedInput (\i -> px i >< y)    (\u -> cx u >< y)
    x@Done{}         >< NeedInput py cy  = NeedInput (\i -> x >< py i)    (\u -> x >< cy u)
    in injectLeftovers (x0 Done) >< injectLeftovers (y0 Done)

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 1.0.13
zipSources :: Monad m => Source m a -> Source m b -> Source m (a, b)
zipSources (ConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    go (Leftover left ()) right = go left right
    go left (Leftover right ())  = go left right
    go (Done ()) (Done ()) = rest ()
    go (Done ()) (HaveOutput _ close _) = PipeM (close >> return (rest ()))
    go (HaveOutput _ close _) (Done ()) = PipeM (close >> return (rest ()))
    go (Done ()) (PipeM _) = rest ()
    go (PipeM _) (Done ()) = rest ()
    go (PipeM mx) (PipeM my) = PipeM (liftM2 go mx my)
    go (PipeM mx) y@HaveOutput{} = PipeM (liftM (\x -> go x y) mx)
    go x@HaveOutput{} (PipeM my) = PipeM (liftM (go x) my)
    go (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (go srcx srcy) (closex >> closey) (x, y)
    go (NeedInput _ c) right = go (c ()) right
    go left (NeedInput _ c) = go left (c ())
    in go (left0 Done) (right0 Done)

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 1.0.13
zipSourcesApp :: Monad m => Source m (a -> b) -> Source m a -> Source m b
zipSourcesApp (ConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    go (Leftover left ()) right = go left right
    go left (Leftover right ())  = go left right
    go (Done ()) (Done ()) = rest ()
    go (Done ()) (HaveOutput _ close _) = PipeM (close >> return (rest ()))
    go (HaveOutput _ close _) (Done ()) = PipeM (close >> return (rest ()))
    go (Done ()) (PipeM _) = rest ()
    go (PipeM _) (Done ()) = rest ()
    go (PipeM mx) (PipeM my) = PipeM (liftM2 go mx my)
    go (PipeM mx) y@HaveOutput{} = PipeM (liftM (\x -> go x y) mx)
    go x@HaveOutput{} (PipeM my) = PipeM (liftM (go x) my)
    go (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (go srcx srcy) (closex >> closey) (x y)
    go (NeedInput _ c) right = go (c ()) right
    go left (NeedInput _ c) = go left (c ())
    in go (left0 Done) (right0 Done)

-- |
--
-- Since 1.0.17
zipConduitApp
    :: Monad m
    => ConduitT i o m (x -> y)
    -> ConduitT i o m x
    -> ConduitT i o m y
zipConduitApp (ConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    go _ _ (Done f) (Done x) = rest (f x)
    go finalX finalY (PipeM mx) y = PipeM (flip (go finalX finalY) y `liftM` mx)
    go finalX finalY x (PipeM my) = PipeM (go finalX finalY x `liftM` my)
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
    go finalX finalY (NeedInput px cx) (NeedInput py cy) = NeedInput
        (\i -> go finalX finalY (px i) (py i))
        (\u -> go finalX finalY (cx u) (cy u))
    go finalX finalY (NeedInput px cx) (Done y) = NeedInput
        (\i -> go finalX finalY (px i) (Done y))
        (\u -> go finalX finalY (cx u) (Done y))
    go finalX finalY (Done x) (NeedInput py cy) = NeedInput
        (\i -> go finalX finalY (Done x) (py i))
        (\u -> go finalX finalY (Done x) (cy u))
  in go (return ()) (return ()) (injectLeftovers $ left0 Done) (injectLeftovers $ right0 Done)

-- | Same as normal fusion (e.g. @=$=@), except instead of discarding leftovers
-- from the downstream component, return them.
--
-- Since 1.0.17
fuseReturnLeftovers :: Monad m
                    => ConduitT a b m ()
                    -> ConduitT b c m r
                    -> ConduitT a c m (r, [b])
fuseReturnLeftovers (ConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    goRight final bs left right =
        case right of
            HaveOutput p c o -> HaveOutput (recurse p) (c >> final) o
            NeedInput rp rc  ->
                case bs of
                    [] -> goLeft rp rc final left
                    b:bs' -> goRight final bs' left (rp b)
            Done r2          -> PipeM (final >> return (rest (r2, bs)))
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
    in goRight (return ()) [] (left0 Done) (right0 Done)

-- | Similar to @fuseReturnLeftovers@, but use the provided function to convert
-- downstream leftovers to upstream leftovers.
--
-- Since 1.0.17
fuseLeftovers
    :: Monad m
    => ([b] -> [a])
    -> ConduitT a b m ()
    -> ConduitT b c m r
    -> ConduitT a c m r
fuseLeftovers f left right = do
    (r, bs) <- fuseReturnLeftovers left right
    mapM_ leftover $ reverse $ f bs
    return r

-- | A generalization of 'ResumableSource'. Allows to resume an arbitrary
-- conduit, keeping its state and using it later (or finalizing it).
--
-- Since 1.0.17
data ResumableConduit i m o =
    ResumableConduit (Pipe i i o () m ()) (m ())

-- | Connect a 'ResumableConduit' to a sink and return the output of the sink
-- together with a new 'ResumableConduit'.
--
-- Since 1.0.17
connectResumeConduit
    :: Monad m
    => ResumableConduit i m o
    -> Sink o m r
    -> Sink i m (ResumableConduit i m o, r)
connectResumeConduit (ResumableConduit left0 leftFinal0) (ConduitT right0) = ConduitT $ \rest -> let
    goRight leftFinal left right =
        case right of
            HaveOutput _ _ o -> absurd o
            NeedInput rp rc -> goLeft rp rc leftFinal left
            Done r2 -> rest (ResumableConduit left leftFinal, r2)
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
    in goRight leftFinal0 left0 (right0 Done)

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
    return (ConduitT ((liftIO (I.writeIORef ref False) >> src) >>=), final')

-- | Turn a @Conduit@ into a @ResumableConduit@ with no attached finalizer.
--
-- Since 1.1.4
newResumableConduit :: Monad m => Conduit i m o -> ResumableConduit i m o
newResumableConduit (ConduitT c) = ResumableConduit (c Done) (return ())


-- | Merge a @Source@ into a @Conduit@.
-- The new conduit will stop processing once either source or upstream have been exhausted.
mergeSource
  :: Monad m
  => Source m i
  -> Conduit a m (i, a)
mergeSource = loop . newResumableSource
  where
    loop :: Monad m => ResumableSource m i -> Conduit a m (i, a)
    loop src0 = await >>= maybe (lift $ closeResumableSource src0) go
      where
        go a = do
          (src1, mi) <- lift $ src0 $$++ await
          case mi of
            Nothing -> lift $ closeResumableSource src1
            Just i  -> yield (i, a) >> loop src1


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
passthroughSink (ConduitT sink0) final = ConduitT $ \rest -> let
    -- A bit of explanation is in order, this function is
    -- non-obvious. The purpose of go is to keep track of the sink
    -- we're passing values to, and then yield values downstream. The
    -- third argument to go is the current state of that sink. That's
    -- relatively straightforward.
    --
    -- The second value is the leftover buffer. These are values that
    -- the sink itself has called leftover on, and must be provided
    -- back to the sink the next time it awaits. _However_, these
    -- values should _not_ be reyielded downstream: we have already
    -- yielded them downstream ourself, and it is the responsibility
    -- of the functions wrapping around passthroughSink to handle the
    -- leftovers from downstream.
    --
    -- The trickiest bit is the first argument, which is a solution to
    -- bug https://github.com/snoyberg/conduit/issues/304. The issue
    -- is that, once we get a value, we need to provide it to both the
    -- inner sink _and_ yield it downstream. The obvious thing to do
    -- is yield first and then recursively call go. Unfortunately,
    -- this doesn't work in all cases: if the downstream component
    -- never calls await again, our yield call will never return, and
    -- our sink will not get the last value. This results is confusing
    -- behavior where the sink and downstream component receive a
    -- different number of values.
    --
    -- Solution: keep a buffer of the next value to yield downstream,
    -- and only yield it downstream in one of two cases: our sink is
    -- asking for another value, or our sink is done. This way, we
    -- ensure that, in all cases, we pass exactly the same number of
    -- values to the inner sink as to downstream.

    go mbuf _ (Done r) = do
        maybe (return ()) CI.yield mbuf
        lift $ final r
        unConduitT (awaitForever yield) rest
    go mbuf is (Leftover sink i) = go mbuf (i:is) sink
    go _ _ (HaveOutput _ _ o) = absurd o
    go mbuf is (PipeM mx) = do
        x <- lift mx
        go mbuf is x
    go mbuf (i:is) (NeedInput next _) = go mbuf is (next i)
    go mbuf [] (NeedInput next done) = do
        maybe (return ()) CI.yield mbuf
        mx <- CI.await
        case mx of
            Nothing -> go Nothing [] (done ())
            Just x -> go (Just x) [] (next x)
    in go Nothing [] (sink0 Done)

-- | Convert a @Source@ into a list. The basic functionality can be explained as:
--
-- > sourceToList src = src $$ Data.Conduit.List.consume
--
-- However, @sourceToList@ is able to produce its results lazily, which cannot
-- be done when running a conduit pipeline in general. Unlike the
-- @Data.Conduit.Lazy@ module (in conduit-extra), this function performs no
-- unsafe I\/O operations, and therefore can only be as lazily as the
-- underlying monad.
--
-- Since 1.2.6
sourceToList :: Monad m => Source m a -> m [a]
sourceToList =
    go . flip unConduitT Done
  where
    go (Done _) = return []
    go (HaveOutput src _ x) = liftM (x:) (go src)
    go (PipeM msrc) = msrc >>= go
    go (NeedInput _ c) = go (c ())
    go (Leftover p _) = go p

-- Define fixity of all our operators
infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-
infixl 1 $=+
infixr 2 .|

-- | Equivalent to using 'runConduit' and '.|' together.
--
-- Since 1.2.3
connect :: Monad m
        => ConduitT () a m ()
        -> ConduitT a Void m r
        -> m r
connect = ($$)

-- | Named function synonym for '.|'.
--
-- Since 1.2.3
fuse :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
fuse = (=$=)

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
{-# DEPRECATED ($$) "Use runConduit and .|" #-}

-- | A synonym for '=$=' for backwards compatibility.
--
-- Since 0.4.0
($=) :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
($=) = (=$=)
{-# INLINE [0] ($=) #-}
{-# RULES "conduit: $= is =$=" ($=) = (=$=) #-}
{-# DEPRECATED ($=) "Use .|" #-}

-- | A synonym for '=$=' for backwards compatibility.
--
-- Since 0.4.0
(=$) :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
(=$) = (=$=)
{-# INLINE [0] (=$) #-}
{-# RULES "conduit: =$ is =$=" (=$) = (=$=) #-}
{-# DEPRECATED (=$) "Use .|" #-}

-- | Fusion operator, combining two @Conduit@s together into a new @Conduit@.
--
-- Both @Conduit@s will be closed when the newly-created @Conduit@ is closed.
--
-- Leftover data returned from the right @Conduit@ will be discarded.
--
-- Since 0.4.0
(=$=) :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
ConduitT left0 =$= ConduitT right0 = ConduitT $ \rest ->
    let goRight final left right =
            case right of
                HaveOutput p c o  -> HaveOutput (recurse p) (c >> final) o
                NeedInput rp rc   -> goLeft rp rc final left
                Done r2           -> PipeM (final >> return (rest r2))
                PipeM mp          -> PipeM (liftM recurse mp)
                Leftover right' i -> goRight final (HaveOutput left final i) right'
          where
            recurse = goRight final left

        goLeft rp rc final left =
            case left of
                HaveOutput left' final' o -> goRight final' left' (rp o)
                NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
                Done r1                   -> goRight (return ()) (Done r1) (rc r1)
                PipeM mp                  -> PipeM (liftM recurse mp)
                Leftover left' i          -> Leftover (recurse left') i
          where
            recurse = goLeft rp rc final
     in goRight (return ()) (left0 Done) (right0 Done)
  where
{-# INLINE [1] (=$=) #-}
{-# DEPRECATED (=$=) "Use .|" #-}

-- | Wait for a single input value from upstream. If no data is available,
-- returns @Nothing@. Once @await@ returns @Nothing@, subsequent calls will
-- also return @Nothing@.
--
-- Since 0.5.0
await :: Monad m => Consumer i m (Maybe i)
await = ConduitT $ \f -> NeedInput (f . Just) (const $ f Nothing)
{-# INLINE [0] await #-}

await' :: Monad m
       => ConduitT i o m r
       -> (i -> ConduitT i o m r)
       -> ConduitT i o m r
await' f g = ConduitT $ \rest -> NeedInput
    (\i -> unConduitT (g i) rest)
    (const $ unConduitT f rest)
{-# INLINE await' #-}
{-# RULES "conduit: await >>= maybe" forall x y. await >>= maybe x y = await' x y #-}

-- | Send a value downstream to the next component to consume. If the
-- downstream component terminates, this call will never return control. If you
-- would like to register a cleanup function, please use 'yieldOr' instead.
--
-- Since 0.5.0
yield :: Monad m
      => o -- ^ output value
      -> ConduitT i o m ()
yield o = yieldOr o (return ())
{-# INLINE yield #-}

-- | Send a monadic value downstream for the next component to consume.
--
-- @since 1.2.7
yieldM :: Monad m => m o -> ConduitT i o m ()
yieldM mo = lift mo >>= yield
{-# INLINE yieldM #-}

  -- FIXME rule won't fire, see FIXME in .Pipe; "mapM_ yield" mapM_ yield = ConduitT . sourceList

-- | Provide a single piece of leftover input to be consumed by the next
-- component in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- @since 0.5.0
leftover :: i -> ConduitT i o m ()
leftover i = ConduitT $ \rest -> Leftover (rest ()) i
{-# INLINE leftover #-}

-- | Run a pipeline until processing completes.
--
-- Since 1.2.1
runConduit :: Monad m => ConduitT () Void m r -> m r
runConduit (ConduitT p) = runPipe $ injectLeftovers $ p Done
{-# INLINE [0] runConduit #-}

-- | Bracket a conduit computation between allocation and release of a
-- resource. Two guarantees are given about resource finalization:
--
-- 1. It will be /prompt/. The finalization will be run as early as possible.
--
-- 2. It is exception safe. Due to usage of @resourcet@, the finalization will
-- be run in the event of any exceptions.
--
-- Since 0.5.0
bracketP :: MonadResource m

         => IO a
            -- ^ computation to run first (\"acquire resource\")
         -> (a -> IO ())
            -- ^ computation to run last (\"release resource\")
         -> (a -> ConduitT i o m r)
            -- ^ computation to run in-between
         -> ConduitT i o m r
            -- returns the value from the in-between computation
bracketP alloc free inside = ConduitT $ \rest -> PipeM $ do
    (key, seed) <- allocate alloc free
    return $ unConduitT (addCleanup (const $ release key) (inside seed)) rest

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
           -> ConduitT i o m r
           -> ConduitT i o m r
addCleanup cleanup (ConduitT c0) = ConduitT $ \rest -> let
    go (Done r) = PipeM (cleanup True >> return (rest r))
    go (HaveOutput src close x) = HaveOutput
        (go src)
        (cleanup False >> close)
        x
    go (PipeM msrc) = PipeM (liftM (go) msrc)
    go (NeedInput p c) = NeedInput
        (go . p)
        (go . c)
    go (Leftover p i) = Leftover (go p) i
    in go (c0 Done)

-- | Similar to 'yield', but additionally takes a finalizer to be run if the
-- downstream component terminates.
--
-- Since 0.5.0
yieldOr :: Monad m
        => o
        -> m () -- ^ finalizer
        -> ConduitT i o m ()
yieldOr o m = ConduitT $ \rest -> HaveOutput (rest ()) m o
{-# INLINE yieldOr #-}

-- | Wait for input forever, calling the given inner component for each piece of
-- new input.
--
-- This function is provided as a convenience for the common pattern of
-- @await@ing input, checking if it's @Just@ and then looping.
--
-- Since 0.5.0
awaitForever :: Monad m => (i -> ConduitT i o m r) -> ConduitT i o m ()
awaitForever f = ConduitT $ \rest ->
    let go = NeedInput (\i -> unConduitT (f i) (const go)) rest
     in go

-- | Transform the monad that a @ConduitT@ lives in.
--
-- Note that the monad transforming function will be run multiple times,
-- resulting in unintuitive behavior in some cases. For a fuller treatment,
-- please see:
--
-- <https://github.com/snoyberg/conduit/wiki/Dealing-with-monad-transformers>
--
-- Since 0.4.0
transPipe :: Monad m => (forall a. m a -> n a) -> ConduitT i o m r -> ConduitT i o n r
transPipe f (ConduitT c0) = ConduitT $ \rest -> let
        go (HaveOutput p c o) = HaveOutput (go p) (f c) o
        go (NeedInput p c) = NeedInput (go . p) (go . c)
        go (Done r) = rest r
        go (PipeM mp) =
            PipeM (f $ liftM go $ collapse mp)
          where
            -- Combine a series of monadic actions into a single action.  Since we
            -- throw away side effects between different actions, an arbitrary break
            -- between actions will lead to a violation of the monad transformer laws.
            -- Example available at:
            --
            -- http://hpaste.org/75520
            collapse mpipe = do
                pipe' <- mpipe
                case pipe' of
                    PipeM mpipe' -> collapse mpipe'
                    _ -> return pipe'
        go (Leftover p i) = Leftover (go p) i
        in go (c0 Done)

-- | Apply a function to all the output values of a @ConduitT@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days. It can also be simulated by fusing with the @map@ conduit from
-- "Data.Conduit.List".
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
mapOutput f (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p c o) = HaveOutput (go p) c (f o)
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM (go) mp)
    go (Leftover p i) = Leftover (go p) i
    in go (c0 Done)

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
mapOutputMaybe f (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p c o) = maybe id (\o' p' -> HaveOutput p' c o') (f o) (go p)
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM (go) mp)
    go (Leftover p i) = Leftover (go p) i
    in go (c0 Done)

-- | Apply a function to all the input values of a @ConduitT@.
--
-- Since 0.5.0
mapInput :: Monad m
         => (i1 -> i2) -- ^ map initial input to new input
         -> (i2 -> Maybe i1) -- ^ map new leftovers to initial leftovers
         -> ConduitT i2 o m r
         -> ConduitT i1 o m r
mapInput f f' (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p c o) = HaveOutput (go p) c o
    go (NeedInput p c) = NeedInput (go . p . f) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM $ liftM go mp
    go (Leftover p i) = maybe id (flip Leftover) (f' i) (go p)
    in go (c0 Done)

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.5.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (ResumableSource m a, b)
ConduitT src $$+ sink =
    connectResume (ResumableSource (src Done) (return ())) sink
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
ResumableSource src final $=+ ConduitT sink =
    ResumableSource (src `pipeL` sink Done) final

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
-- Implemented on top of @ZipSource@ and as such, it exhibits the same
-- short-circuiting behavior as @ZipSource@. See that data type for more
-- details. If you want to create a source that yields *all* values from
-- multiple sources, use `sequence_`.
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
-- This newtype is in fact a type constrained version of 'ZipConduit', and has
-- the same behavior. It's presented as a separate type since (1) it
-- historically predates @ZipConduit@, and (2) the type constraining can make
-- your code clearer (and thereby make your error messages more easily
-- understood).
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
(=$$+) (ConduitT conduit) = connectResumeConduit (ResumableConduit (conduit Done) (return ()))
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

-- | Provides an alternative @Applicative@ instance for @ConduitT@. In this instance,
-- every incoming value is provided to all @ConduitT@s, and output is coalesced together.
-- Leftovers from individual @ConduitT@s will be used within that component, and then discarded
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
newtype ZipConduit i o m r = ZipConduit { getZipConduit :: ConduitT i o m r }
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
sequenceConduits :: (Traversable f, Monad m) => f (ConduitT i o m r) -> ConduitT i o m (f r)
sequenceConduits = getZipConduit . sequenceA . fmap ZipConduit

-- | Fuse two @ConduitT@s together, and provide the return value of both. Note
-- that this will force the entire upstream @ConduitT@ to be run to produce the
-- result value, even if the downstream terminates early.
--
-- Since 1.1.5
fuseBoth :: Monad m => ConduitT a b m r1 -> ConduitT b c m r2 -> ConduitT a c m (r1, r2)
fuseBoth (ConduitT up) (ConduitT down) =
    ConduitT (pipeL (up Done) (withUpstream $ generalizeUpstream $ down Done) >>=)
{-# INLINE fuseBoth #-}

-- | Like 'fuseBoth', but does not force consumption of the @Producer@.
-- In the case that the @Producer@ terminates, the result value is
-- provided as a @Just@ value. If it does not terminate, then a
-- @Nothing@ value is returned.
--
-- One thing to note here is that "termination" here only occurs if the
-- @Producer@ actually yields a @Nothing@ value. For example, with the
-- @Producer@ @mapM_ yield [1..5]@, if five values are requested, the
-- @Producer@ has not yet terminated. Termination only occurs when the
-- sixth value is awaited for and the @Producer@ signals termination.
--
-- Since 1.2.4
fuseBothMaybe
    :: Monad m
    => ConduitT a b m r1
    -> ConduitT b c m r2
    -> ConduitT a c m (Maybe r1, r2)
fuseBothMaybe (ConduitT up) (ConduitT down) =
    ConduitT (pipeL (up Done) (go Nothing $ down Done) >>=)
  where
    go mup (Done r) = Done (mup, r)
    go mup (PipeM mp) = PipeM $ liftM (go mup) mp
    go mup (HaveOutput p c o) = HaveOutput (go mup p) c o
    go _ (NeedInput p c) = NeedInput
        (\i -> go Nothing (p i))
        (\u -> go (Just u) (c ()))
    go mup (Leftover p i) = Leftover (go mup p) i
{-# INLINABLE fuseBothMaybe #-}

-- | Same as @fuseBoth@, but ignore the return value from the downstream
-- @Conduit@. Same caveats of forced consumption apply.
--
-- Since 1.1.5
fuseUpstream :: Monad m => ConduitT a b m r -> Conduit b m c -> ConduitT a c m r
fuseUpstream up down = fmap fst (fuseBoth up down)
{-# INLINE fuseUpstream #-}

-- Rewrite rules

{- FIXME
{-# RULES "conduit: ConduitT: lift x >>= f" forall m f. lift m >>= f = ConduitT (PipeM (liftM (unConduitT . f) m)) #-}
{-# RULES "conduit: ConduitT: lift x >> f" forall m f. lift m >> f = ConduitT (PipeM (liftM (\_ -> unConduitT f) m)) #-}

{-# RULES "conduit: ConduitT: liftIO x >>= f" forall m (f :: MonadIO m => a -> ConduitT i o m r). liftIO m >>= f = ConduitT (PipeM (liftM (unConduitT . f) (liftIO m))) #-}
{-# RULES "conduit: ConduitT: liftIO x >> f" forall m (f :: MonadIO m => ConduitT i o m r). liftIO m >> f = ConduitT (PipeM (liftM (\_ -> unConduitT f) (liftIO m))) #-}

{-# RULES "conduit: ConduitT: liftBase x >>= f" forall m (f :: MonadBase b m => a -> ConduitT i o m r). liftBase m >>= f = ConduitT (PipeM (liftM (unConduitT . f) (liftBase m))) #-}
{-# RULES "conduit: ConduitT: liftBase x >> f" forall m (f :: MonadBase b m => ConduitT i o m r). liftBase m >> f = ConduitT (PipeM (liftM (\_ -> unConduitT f) (liftBase m))) #-}

{-# RULES
    "yield o >> p" forall o (p :: ConduitT i o m r). yield o >> p = ConduitT (HaveOutput (unConduitT p) (return ()) o)
  ; "yieldOr o c >> p" forall o c (p :: ConduitT i o m r). yieldOr o c >> p =
        ConduitT (HaveOutput (unConduitT p) c o)
  ; "when yield next" forall b o p. when b (yield o) >> p =
        if b then ConduitT (HaveOutput (unConduitT p) (return ()) o) else p
  ; "unless yield next" forall b o p. unless b (yield o) >> p =
        if b then p else ConduitT (HaveOutput (unConduitT p) (return ()) o)
  ; "lift m >>= yield" forall m. lift m >>= yield = yieldM m
   #-}
{-# RULES "conduit: leftover l >> p" forall l (p :: ConduitT i o m r). leftover l >> p =
    ConduitT (Leftover (unConduitT p) l) #-}
    -}
