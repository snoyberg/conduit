{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Note: This module is experimental, and might be modified at any time.
-- Caveat emptor!
module Data.Conduit.Class
    ( module Data.Conduit.Class
    , C.ResumableSource
    , C.runResourceT
    , C.Flush (..)
    , C.ResourceT
    , C.unwrapResumable
    ) where

import Prelude (Monad (..), Functor (..), ($), const, IO, Maybe, Either, Bool, (.), either, mapM_)
import Data.Void (Void)
import Control.Applicative (Applicative (..))
import qualified Data.Conduit as C
import Data.Conduit.Internal (Pipe (PipeM))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Resource (allocate, release, MonadThrow, MonadResource, ResourceT)
import Control.Monad.Trans.Control (liftWith, restoreT, MonadTransControl)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (Monoid (..))

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
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, ResourcePipe, MonadThrow)

instance Monad m => Monoid (SourceM o m ()) where
    mempty = return ()
    mappend = (>>)

-- | Consumes a stream of input values and produces a stream of output values,
-- without producing a final result.
--
-- Since 0.6.0
type Conduit i m o = ConduitM i o m ()

newtype ConduitM i o m r = ConduitM { unConduitM :: Pipe i i o () m r }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, ResourcePipe, MonadThrow)

instance Monad m => Monoid (ConduitM i o m ()) where
    mempty = return ()
    mappend = (>>)

-- | Consumes a stream of input values and produces a final result, without
-- producing any output.
--
-- Since 0.6.0
newtype Sink i m r = Sink { unSink :: Pipe i i Void () m r }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, ResourcePipe, MonadThrow)

instance Monad m => Monoid (Sink i m ()) where
    mempty = return ()
    mappend = (>>)

class (Monad m, Monad (PipeMonad m)) => IsPipe m where
    type PipeInput m
    type PipeTerm m -- FIXME remove in the future
    type PipeOutput m
    type PipeMonad m :: * -> *
    type PipeLeftover m -- FIXME remove in the future

    -- | Wait for a single input value from upstream, terminating immediately if no
    -- data is available.
    --
    -- Since 0.5.0
    await :: m (Maybe (PipeInput m))

    -- | This is similar to @await@, but will return the upstream result value as
    -- @Left@ if available.
    --
    -- Since 0.5.0
    awaitE :: m (Either (PipeTerm m) (PipeInput m))

    -- | Provide a single piece of leftover input to be consumed by the next pipe
    -- in the current monadic binding.
    --
    -- /Note/: it is highly encouraged to only return leftover values from input
    -- already consumed from upstream.
    --
    -- Since 0.5.0
    leftover :: PipeLeftover m -> m ()

    -- | Send a single output value downstream. If the downstream @Pipe@
    -- terminates, this @Pipe@ will terminate as well.
    --
    -- Since 0.5.0
    yield :: PipeOutput m -> m ()

    -- | Similar to @yield@, but additionally takes a finalizer to be run if the
    -- downstream @Pipe@ terminates.
    --
    -- Since 0.5.0
    yieldOr :: PipeOutput m -> PipeMonad m () -> m ()

    liftPipeMonad :: PipeMonad m a -> m a

    -- | Add some code to be run when the given @Pipe@ cleans up.
    --
    -- Since 0.4.1
    addCleanup :: (Bool -> PipeMonad m ()) -- ^ @True@ if @Pipe@ ran to completion, @False@ for early termination.
               -> m r
               -> m r

instance Monad m => IsPipe (Pipe l i o u m) where
    type PipeInput (Pipe l i o u m) = i
    type PipeTerm (Pipe l i o u m) = u
    type PipeOutput (Pipe l i o u m) = o
    type PipeMonad (Pipe l i o u m) = m
    type PipeLeftover (Pipe l i o u m) = l

    await = C.await
    {-# INLINE [1] await #-}

    awaitE = C.awaitE
    {-# INLINE [1] awaitE #-}

    leftover = C.leftover
    {-# INLINE [1] leftover #-}

    yield = C.yield
    {-# INLINE yield #-}

    yieldOr = C.yieldOr
    {-# INLINE yieldOr #-}

    liftPipeMonad = lift

    addCleanup = C.addCleanup

instance Monad m => IsPipe (SourceM o m) where
    type PipeInput (SourceM o m) = ()
    type PipeTerm (SourceM o m) = ()
    type PipeOutput (SourceM o m) = o
    type PipeMonad (SourceM o m) = m
    type PipeLeftover (SourceM o m) = ()

    await = SourceM await
    {-# INLINE await #-}

    awaitE = SourceM awaitE
    {-# INLINE awaitE #-}

    leftover = SourceM . leftover
    {-# INLINE leftover #-}

    yield = SourceM . yield
    {-# INLINE yield #-}

    yieldOr a = SourceM . yieldOr a
    {-# INLINE yieldOr #-}

    liftPipeMonad = lift
    {-# INLINE liftPipeMonad #-}

    addCleanup c (SourceM p) = SourceM (addCleanup c p)
    {-# INLINE addCleanup #-}

instance Monad m => IsPipe (ConduitM i o m) where
    type PipeInput (ConduitM i o m) = i
    type PipeTerm (ConduitM i o m) = ()
    type PipeOutput (ConduitM i o m) = o
    type PipeMonad (ConduitM i o m) = m
    type PipeLeftover (ConduitM i o m) = i

    await = ConduitM await
    {-# INLINE await #-}

    awaitE = ConduitM awaitE
    {-# INLINE awaitE #-}

    leftover = ConduitM . leftover
    {-# INLINE leftover #-}

    yield = ConduitM . yield
    {-# INLINE yield #-}

    yieldOr a = ConduitM . yieldOr a
    {-# INLINE yieldOr #-}

    liftPipeMonad = lift
    {-# INLINE liftPipeMonad #-}

    addCleanup c (ConduitM p) = ConduitM (addCleanup c p)
    {-# INLINE addCleanup #-}

instance Monad m => IsPipe (Sink i m) where
    type PipeInput (Sink i m) = i
    type PipeTerm (Sink i m) = ()
    type PipeOutput (Sink i m) = Void
    type PipeMonad (Sink i m) = m
    type PipeLeftover (Sink i m) = i

    await = Sink await
    {-# INLINE await #-}

    awaitE = Sink awaitE
    {-# INLINE awaitE #-}

    leftover = Sink . leftover
    {-# INLINE leftover #-}

    yield = Sink . yield
    {-# INLINE yield #-}

    yieldOr a = Sink . yieldOr a
    {-# INLINE yieldOr #-}

    liftPipeMonad = lift
    {-# INLINE liftPipeMonad #-}

    addCleanup c (Sink p) = Sink (addCleanup c p)
    {-# INLINE addCleanup #-}

class (IsPipe m, MonadResource (PipeMonad m), MonadIO m) => ResourcePipe m where
    -- | Perform some allocation and run an inner @Pipe@. Two guarantees are given
    -- about resource finalization:
    --
    -- 1. It will be /prompt/. The finalization will be run as early as possible.
    --
    -- 2. It is exception safe. Due to usage of @resourcet@, the finalization will
    --    be run in the event of any exceptions.
    --
    -- Since 0.5.0
    bracketP :: IO a -> (a -> IO ()) -> (a -> m r) -> m r

instance MonadResource m => ResourcePipe (Pipe l i o u m) where
    bracketP alloc free inside = PipeM $ do
        (key, seed) <- allocate alloc free
        return $ addCleanup (const $ release key) (inside seed)

#define GOALL(C, C2, T) instance C => IsPipe (T) where { type PipeInput (T) = PipeInput m; type PipeMonad (T) = PipeMonad m; type PipeTerm (T) = PipeTerm m; type PipeOutput (T) = PipeOutput m; type PipeLeftover (T) = PipeLeftover m; await = lift await; awaitE = lift awaitE; leftover = lift . leftover; yield = lift . yield; yieldOr a = lift . yieldOr a; liftPipeMonad = lift . liftPipeMonad; addCleanup c r = liftWith (\run -> run $ addCleanup c r) >>= restoreT . return}; instance C2 => ResourcePipe (T) where { bracketP = controlBracketP }
#define GO(T) GOALL(IsPipe m, ResourcePipe m, T m)
#define GOX(X, T) GOALL((IsPipe m, X), (ResourcePipe m, X), T m)
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

controlBracketP :: (ResourcePipe m, Monad (t m), MonadTransControl t)
                => IO a -> (a -> IO ()) -> (a -> t m r) -> t m r
controlBracketP alloc free inside = liftWith (\run -> bracketP alloc free (run . inside)) >>= restoreT . return

-- | Wait for input forever, calling the given inner @Pipe@ for each piece of
-- new input. Returns the upstream result type.
--
-- Since 0.5.0
awaitForever :: IsPipe m
             => (PipeInput m -> m r')
             -> m (PipeTerm m)
awaitForever inner =
    self
  where
    self = awaitE >>= either return (\i -> inner i >> self)
{-# INLINE [1] awaitForever #-}

infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-

($$) :: Monad m => Source m a -> Sink a m b -> m b
SourceM src $$ Sink sink = src C.$$ sink
{-# INLINE ($$) #-}

($=) :: Monad m => Source m a -> Conduit a m b -> Source m b
SourceM src $= ConduitM con = SourceM $ src C.$= con
{-# INLINE ($=) #-}

(=$=) :: Monad m => Conduit a m b -> Conduit b m c -> Conduit a m c
ConduitM l =$= ConduitM r = ConduitM $ l C.=$= r
{-# INLINE (=$=) #-}

(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
ConduitM l =$ Sink r = Sink $ l C.=$ r
{-# INLINE (=$) #-}

($$+) :: Monad m => Source m a -> Sink a m b -> m (C.ResumableSource m a, b)
SourceM src $$+ Sink sink = src C.$$+ sink
{-# INLINE ($$+) #-}

($$++) :: Monad m => C.ResumableSource m a -> Sink a m b -> m (C.ResumableSource m a, b)
rsrc $$++ Sink sink = rsrc C.$$++ sink
{-# INLINE ($$++) #-}

($$+-) :: Monad m => C.ResumableSource m a -> Sink a m b -> m b
rsrc $$+- Sink sink = rsrc C.$$+- sink
{-# INLINE ($$+-) #-}

sourceList :: IsPipe m => [PipeOutput m] -> m ()
sourceList = mapM_ yield
{-# INLINE sourceList #-}
