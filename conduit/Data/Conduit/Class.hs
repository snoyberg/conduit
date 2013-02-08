{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
-- | Note: This module is experimental, and might be modified at any time.
-- Caveat emptor!
module Data.Conduit.Class
    ( MFunctor (..)
    , MonadStream (..)
    , sourceList
    , bracketP
    , Source
    , Sink (..)
    , Conduit
    , MonadSource
    , MonadSink
    , MonadConduit
    , awaitForever
    , ResumableSource
    , unwrapResumable
    , SourceM (..)
    , ConduitM (..)
    , liftStreamIO
    , ($$)
    , ($=)
    , (=$)
    , (=$=)
    , ($$+)
    , ($$++)
    , ($$+-)
    ) where

import Prelude (Monad (..), Functor (..), ($), const, IO, Maybe, Either, Bool (..), (.), either, mapM_, maybe)
import Data.Void (Void)
import Control.Applicative (Applicative (..))
import qualified Data.Conduit.Internal as CI
import Data.Conduit.Internal (Pipe, transPipe, pipeL, connectResume, ResumableSource (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Resource (allocate, release, MonadThrow, MonadResource, ResourceT)
import Control.Monad.Trans.Control (liftWith, restoreT, MonadTransControl)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Monoid (Monoid (..))
import Control.Monad (when)
import qualified Data.IORef as I

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

-- | Provides a stream of output values, without consuming any input or
-- producing a final result.
--
-- Since 0.6.0
type Source m o = SourceM o m ()

newtype SourceM o m r = SourceM { unSourceM :: Pipe () () o () m r }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MFunctor)

instance Monad m => Monoid (SourceM o m ()) where
    mempty = return ()
    mappend = (>>)

-- | Consumes a stream of input values and produces a stream of output values,
-- without producing a final result.
--
-- Since 0.6.0
type Conduit i m o = ConduitM i o m ()

newtype ConduitM i o m r = ConduitM { unConduitM :: Pipe i i o () m r }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MFunctor)

instance Monad m => Monoid (ConduitM i o m ()) where
    mempty = return ()
    mappend = (>>)

-- | Consumes a stream of input values and produces a final result, without
-- producing any output.
--
-- Since 0.6.0
newtype Sink i m r = Sink { unSink :: Pipe i i Void () m r }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MFunctor)

instance Monad m => Monoid (Sink i m ()) where
    mempty = return ()
    mappend = (>>)

class (Monad m, Monad (StreamMonad m)) => MonadStream m where
    type Upstream m
    type Downstream m
    type StreamMonad m :: * -> *

    -- | Wait for a single input value from upstream, terminating immediately if no
    -- data is available.
    --
    -- Since 0.5.0
    await :: m (Maybe (Upstream m))

    -- | Provide a single piece of leftover input to be consumed by the next pipe
    -- in the current monadic binding.
    --
    -- /Note/: it is highly encouraged to only return leftover values from input
    -- already consumed from upstream.
    --
    -- Since 0.5.0
    leftover :: Upstream m -> m ()

    -- | Send a single output value downstream. If the downstream @Pipe@
    -- terminates, this @Pipe@ will terminate as well.
    --
    -- Since 0.5.0
    yield :: Downstream m -> m ()

    -- | Similar to @yield@, but additionally takes a finalizer to be run if the
    -- downstream @Pipe@ terminates.
    --
    -- Since 0.5.0
    yieldOr :: Downstream m -> StreamMonad m () -> m ()

    liftStreamMonad :: StreamMonad m a -> m a

    -- | Add some code to be run when the given @Pipe@ cleans up.
    --
    -- Since 0.4.1
    addCleanup :: (Bool -> StreamMonad m ()) -- ^ @True@ if @Pipe@ ran to completion, @False@ for early termination.
               -> m r
               -> m r

instance (Monad m, l ~ i) => MonadStream (Pipe l i o u m) where
    type Upstream (Pipe l i o u m) = i
    type Downstream (Pipe l i o u m) = o
    type StreamMonad (Pipe l i o u m) = m

    await = CI.await
    {-# INLINE [1] await #-}

    leftover = CI.leftover
    {-# INLINE [1] leftover #-}

    yield = CI.yield
    {-# INLINE yield #-}

    yieldOr = CI.yieldOr
    {-# INLINE yieldOr #-}

    liftStreamMonad = lift

    addCleanup = CI.addCleanup

instance Monad m => MonadStream (SourceM o m) where
    type Upstream (SourceM o m) = ()
    type Downstream (SourceM o m) = o
    type StreamMonad (SourceM o m) = m

    await = SourceM await
    {-# INLINE await #-}

    leftover = SourceM . leftover
    {-# INLINE leftover #-}

    yield = SourceM . yield
    {-# INLINE yield #-}

    yieldOr a = SourceM . yieldOr a
    {-# INLINE yieldOr #-}

    liftStreamMonad = lift
    {-# INLINE liftStreamMonad #-}

    addCleanup c (SourceM p) = SourceM (addCleanup c p)
    {-# INLINE addCleanup #-}

instance Monad m => MonadStream (ConduitM i o m) where
    type Upstream (ConduitM i o m) = i
    type Downstream (ConduitM i o m) = o
    type StreamMonad (ConduitM i o m) = m

    await = ConduitM await
    {-# INLINE await #-}

    leftover = ConduitM . leftover
    {-# INLINE leftover #-}

    yield = ConduitM . yield
    {-# INLINE yield #-}

    yieldOr a = ConduitM . yieldOr a
    {-# INLINE yieldOr #-}

    liftStreamMonad = lift
    {-# INLINE liftStreamMonad #-}

    addCleanup c (ConduitM p) = ConduitM (addCleanup c p)
    {-# INLINE addCleanup #-}

instance Monad m => MonadStream (Sink i m) where
    type Upstream (Sink i m) = i
    type Downstream (Sink i m) = Void
    type StreamMonad (Sink i m) = m

    await = Sink await
    {-# INLINE await #-}

    leftover = Sink . leftover
    {-# INLINE leftover #-}

    yield = Sink . yield
    {-# INLINE yield #-}

    yieldOr a = Sink . yieldOr a
    {-# INLINE yieldOr #-}

    liftStreamMonad = lift
    {-# INLINE liftStreamMonad #-}

    addCleanup c (Sink p) = Sink (addCleanup c p)
    {-# INLINE addCleanup #-}

-- | Perform some allocation and run an inner @Pipe@. Two guarantees are given
-- about resource finalization:
--
-- 1. It will be /prompt/. The finalization will be run as early as possible.
--
-- 2. It is exception safe. Due to usage of @resourcet@, the finalization will
--    be run in the event of any exceptions.
--
-- Since 0.5.0
bracketP :: (MonadStream m, MonadResource (StreamMonad m))
         => IO a -- ^ allocate
         -> (a -> IO ()) -- ^ free
         -> (a -> m r) -- ^ inside
         -> m r
bracketP alloc free inside = do
    (key, seed) <- liftStreamMonad $ allocate alloc free
    addCleanup (const $ release key) (inside seed)

#define GOALL(C, T) instance C => MonadStream (T) where { type Upstream (T) = Upstream m; type StreamMonad (T) = StreamMonad m; type Downstream (T) = Downstream m; await = lift await; leftover = lift . leftover; yield = lift . yield; yieldOr a = lift . yieldOr a; liftStreamMonad = lift . liftStreamMonad; addCleanup c r = liftWith (\run -> run $ addCleanup c r) >>= restoreT . return}
#define GO(T) GOALL(MonadStream m, T m)
#define GOX(X, T) GOALL((MonadStream m, X), T m)
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(StateT s)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
GO(ResourceT)
#undef GO
#undef GOX
#undef GOALL

-- | Wait for input forever, calling the given inner @Pipe@ for each piece of
-- new input.
--
-- Since 0.5.0
awaitForever :: MonadStream m => (Upstream m -> m r') -> m ()
awaitForever inner =
    self
  where
    self = await >>= maybe (return ()) (\i -> inner i >> self)
{-# INLINE [1] awaitForever #-}

infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- When either side closes, the other side will immediately be closed as well.
-- If you would like to keep the @Source@ open to be used for another
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
SourceM src $= ConduitM con = SourceM $ src `pipeL` con
{-# INLINE ($=) #-}

-- | Fusion operator, combining two @Conduit@s together into a new @Conduit@.
--
-- Both @Conduit@s will be closed when the newly-created @Conduit@ is closed.
--
-- Leftover data returned from the right @Conduit@ will be discarded.
--
-- Since 0.4.0
(=$=) :: Monad m => Conduit a m b -> Conduit b m c -> Conduit a m c
ConduitM l =$= ConduitM r = ConduitM $ l `pipeL` r
{-# INLINE (=$=) #-}

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Both the @Conduit@ and @Sink@ will be closed when the newly-created @Sink@
-- is closed.
--
-- Leftover data returned from the @Sink@ will be discarded.
--
-- Since 0.4.0
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
ConduitM l =$ Sink r = Sink $ l `pipeL` r
{-# INLINE (=$) #-}

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.5.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (ResumableSource m a, b)
SourceM src $$+ Sink sink = connectResume (ResumableSource src (return ())) sink
{-# INLINE ($$+) #-}

-- | Continue processing after usage of @$$+@.
--
-- Since 0.5.0
($$++) :: Monad m => ResumableSource m a -> Sink a m b -> m (ResumableSource m a, b)
rsrc $$++ Sink sink = rsrc `connectResume` sink
{-# INLINE ($$++) #-}

-- | Complete processing of a @ResumableSource@. This will run the finalizer
-- associated with the @ResumableSource@. In order to guarantee process resource
-- finalization, you /must/ use this operator after using @$$+@ and @$$++@.
--
-- Since 0.5.0
($$+-) :: Monad m => ResumableSource m a -> Sink a m b -> m b
rsrc $$+- Sink sink = do
    (ResumableSource _ final, res) <- connectResume rsrc sink
    final
    return res
{-# INLINE ($$+-) #-}

sourceList :: MonadStream m => [Downstream m] -> m ()
sourceList = mapM_ yield
{-# INLINE sourceList #-}

type MonadSource m a = forall source. (MonadStream source, a ~ Downstream source, StreamMonad source ~ m) => source ()
type MonadConduit a m b = forall conduit. (MonadStream conduit, a ~ Upstream conduit, b ~ Downstream conduit, StreamMonad conduit ~ m) => conduit ()
type MonadSink a m b = forall sink. (MonadStream sink, a ~ Upstream sink, StreamMonad sink ~ m) => sink b

-- | Borrowed from pipes, hopefully will be released separately.
class MFunctor t where
    hoist :: Monad m
          => (forall a. m a -> n a)
          -> t m b
          -> t n b
instance MFunctor (Pipe l i o u) where
    hoist = transPipe

-- | Unwraps a @ResumableSource@ into a @Source@ and a finalizer.
--
-- A @ResumableSource@ represents a @Source@ which has already been run, and
-- therefore has a finalizer registered. As a result, if we want to turn it
-- into a regular @Source@, we need to ensure that the finalizer will be run
-- appropriately. By appropriately, I mean:
--
-- * If a new finalizer is registered, the old one should not be called.
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
    return (liftIO (I.writeIORef ref False) >> SourceM src, final')

liftStreamIO :: (MonadStream m, MonadIO (StreamMonad m)) => IO a -> m a
liftStreamIO = liftStreamMonad . liftIO
