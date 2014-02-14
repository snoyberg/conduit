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
module Data.Conduit.Internal
    ( -- * Types
      Pipe (..)
    , ConduitM (..)
    , Source
    , Producer
    , Sink
    , Consumer
    , Conduit
    , ResumableSource (..)
      -- * Primitives
    , await
    , awaitE
    , awaitForever
    , yield
    , yieldOr
    , leftover
      -- * Finalization
    , bracketP
    , addCleanup
      -- * Composition
    , idP
    , pipe
    , pipeL
    , connectResume
    , runPipe
    , injectLeftovers
    , (>+>)
    , (<+<)
      -- * Generalizing
    , sourceToPipe
    , sinkToPipe
    , conduitToPipe
    , toProducer
    , toConsumer
      -- * Exceptions
    , catchP
    , handleP
    , tryP
    , catchC
    , handleC
    , tryC
      -- * Utilities
    , transPipe
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , sourceList
    , withUpstream
    , unwrapResumable
    , Data.Conduit.Internal.enumFromTo
    , zipSinks
    , zipSources
    , zipSourcesApp
    ) where

import Control.Applicative (Applicative (..))
import Control.Exception.Lifted as E (Exception, catch)
import Control.Monad ((>=>), liftM, ap, when, liftM2)
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
import qualified GHC.Exts
import qualified Data.IORef as I
import Control.Monad.Morph (MFunctor (..))

-- | The underlying datatype for all the types in this package.  In has six
-- type parameters:
--
-- * /l/ is the type of values that may be left over from this @Pipe@. A @Pipe@
-- with no leftovers would use @Void@ here, and one with leftovers would use
-- the same type as the /i/ parameter. Leftovers are automatically provided to
-- the next @Pipe@ in the monadic chain.
--
-- * /i/ is the type of values for this @Pipe@'s input stream.
--
-- * /o/ is the type of values for this @Pipe@'s output stream.
--
-- * /u/ is the result type from the upstream @Pipe@.
--
-- * /m/ is the underlying monad.
--
-- * /r/ is the result type.
--
-- A basic intuition is that every @Pipe@ produces a stream of output values
-- (/o/), and eventually indicates that this stream is terminated by sending a
-- result (/r/). On the receiving end of a @Pipe@, these become the /i/ and /u/
-- parameters.
--
-- Since 0.5.0
data Pipe l i o u m r =
    -- | Provide new output to be sent downstream. This constructor has three
    -- fields: the next @Pipe@ to be used, a finalization function, and the
    -- output value.
    HaveOutput (Pipe l i o u m r) (m ()) o
    -- | Request more input from upstream. The first field takes a new input
    -- value and provides a new @Pipe@. The second takes an upstream result
    -- value, which indicates that upstream is producing no more results.
  | NeedInput (i -> Pipe l i o u m r) (u -> Pipe l i o u m r)
    -- | Processing with this @Pipe@ is complete, providing the final result.
  | Done r
    -- | Require running of a monadic action to get the next @Pipe@.
  | PipeM (m (Pipe l i o u m r))
    -- | Return leftover input, which should be provided to future operations.
  | Leftover (Pipe l i o u m r) l

instance Monad m => Functor (Pipe l i o u m) where
    fmap = liftM

instance Monad m => Applicative (Pipe l i o u m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Pipe l i o u m) where
    return = Done

    HaveOutput p c o >>= fp = HaveOutput (p >>= fp)            c          o
    NeedInput p c    >>= fp = NeedInput  (p >=> fp)            (c >=> fp)
    Done x           >>= fp = fp x
    PipeM mp         >>= fp = PipeM      ((>>= fp) `liftM` mp)
    Leftover p i     >>= fp = Leftover   (p >>= fp)            i

instance MonadBase base m => MonadBase base (Pipe l i o u m) where
    liftBase = lift . liftBase

instance MonadTrans (Pipe l i o u) where
    lift mr = PipeM (Done `liftM` mr)

instance MonadIO m => MonadIO (Pipe l i o u m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (Pipe l i o u m) where
    monadThrow = lift . monadThrow

instance MonadActive m => MonadActive (Pipe l i o u m) where
    monadActive = lift monadActive

instance Monad m => Monoid (Pipe l i o u m ()) where
    mempty = return ()
    mappend = (>>)

instance MonadResource m => MonadResource (Pipe l i o u m) where
    liftResourceT = lift . liftResourceT

instance MonadReader r m => MonadReader r (Pipe l i o u m) where
    ask = lift ask
    local f (HaveOutput p c o) = HaveOutput (local f p) c o
    local f (NeedInput p c) = NeedInput (\i -> local f (p i)) (\u -> local f (c u))
    local _ (Done x) = Done x
    local f (PipeM mp) = PipeM (local f mp)
    local f (Leftover p i) = Leftover (local f p) i

-- Provided for doctest
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x, y, z) 0
#endif

instance MonadWriter w m => MonadWriter w (Pipe l i o u m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif

    tell = lift . tell

    listen (HaveOutput p c o) = HaveOutput (listen p) c o
    listen (NeedInput p c) = NeedInput (\i -> listen (p i)) (\u -> listen (c u))
    listen (Done x) = Done (x,mempty)
    listen (PipeM mp) =
      PipeM $
      do (p,w) <- listen mp
         return $ do (x,w') <- listen p
                     return (x, w `mappend` w')
    listen (Leftover p i) = Leftover (listen p) i

    pass (HaveOutput p c o) = HaveOutput (pass p) c o
    pass (NeedInput p c) = NeedInput (\i -> pass (p i)) (\u -> pass (c u))
    pass (PipeM mp) = PipeM $ mp >>= (return . pass)
    pass (Done (x,_)) = Done x
    pass (Leftover p i) = Leftover (pass p) i

instance MonadState s m => MonadState s (Pipe l i o u m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (Pipe l i o u m)

instance MonadError e m => MonadError e (Pipe l i o u m) where
    throwError = lift . throwError
    catchError (HaveOutput p c o) f = HaveOutput (catchError p f) c o
    catchError (NeedInput p c) f = NeedInput (\i -> catchError (p i) f) (\u -> catchError (c u) f)
    catchError (Done x) _ = Done x
    catchError (PipeM mp) f =
      PipeM $ catchError (liftM (flip catchError f) mp) (\e -> return (f e))
    catchError (Leftover p i) f = Leftover (catchError p f) i

-- | Core datatype of the conduit package. This type represents a general
-- component which can consume a stream of input values @i@, produce a stream
-- of output values @o@, perform actions in the @m@ monad, and produce a final
-- result @r@. The type synonyms provided here are simply wrappers around this
-- type.
--
-- Since 1.0.0
newtype ConduitM i o m r = ConduitM { unConduitM :: Pipe i i o () m r }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadActive, MonadResource, MFunctor)

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

instance Monad m => Monoid (ConduitM i o m ()) where
    mempty = return ()
    mappend = (>>)

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

-- | Wait for a single input value from upstream.
--
-- Since 0.5.0
await :: Pipe l i o u m (Maybe i)
await = NeedInput (Done . Just) (\_ -> Done Nothing)
{-# RULES "CI.await >>= maybe" forall x y. await >>= maybe x y = NeedInput y (const x) #-}
{-# INLINE [1] await #-}

-- | This is similar to @await@, but will return the upstream result value as
-- @Left@ if available.
--
-- Since 0.5.0
awaitE :: Pipe l i o u m (Either u i)
awaitE = NeedInput (Done . Right) (Done . Left)
{-# RULES "awaitE >>= either" forall x y. awaitE >>= either x y = NeedInput y x #-}
{-# INLINE [1] awaitE #-}

-- | Wait for input forever, calling the given inner @Pipe@ for each piece of
-- new input. Returns the upstream result type.
--
-- Since 0.5.0
awaitForever :: Monad m => (i -> Pipe l i o r m r') -> Pipe l i o r m r
awaitForever inner =
    self
  where
    self = awaitE >>= either return (\i -> inner i >> self)
{-# INLINE [1] awaitForever #-}

-- | Send a single output value downstream. If the downstream @Pipe@
-- terminates, this @Pipe@ will terminate as well.
--
-- Since 0.5.0
yield :: Monad m
      => o -- ^ output value
      -> Pipe l i o u m ()
yield = HaveOutput (Done ()) (return ())
{-# INLINE [1] yield #-}

-- | Similar to @yield@, but additionally takes a finalizer to be run if the
-- downstream @Pipe@ terminates.
--
-- Since 0.5.0
yieldOr :: Monad m
        => o
        -> m () -- ^ finalizer
        -> Pipe l i o u m ()
yieldOr o f = HaveOutput (Done ()) f o
{-# INLINE [1] yieldOr #-}

{-# RULES
    "CI.yield o >> p" forall o (p :: Pipe l i o u m r). yield o >> p = HaveOutput p (return ()) o
  ; "mapM_ CI.yield" mapM_ yield = sourceList
  ; "CI.yieldOr o c >> p" forall o c (p :: Pipe l i o u m r). yieldOr o c >> p = HaveOutput p c o #-}

-- | Provide a single piece of leftover input to be consumed by the next pipe
-- in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftover :: l -> Pipe l i o u m ()
leftover = Leftover (Done ())
{-# INLINE [1] leftover #-}
{-# RULES "leftover l >> p" forall l (p :: Pipe l i o u m r). leftover l >> p = Leftover p l #-}

-- | Perform some allocation and run an inner @Pipe@. Two guarantees are given
-- about resource finalization:
--
-- 1. It will be /prompt/. The finalization will be run as early as possible.
--
-- 2. It is exception safe. Due to usage of @resourcet@, the finalization will
--    be run in the event of any exceptions.
--
-- Since 0.5.0
bracketP :: MonadResource m
         => IO a
         -> (a -> IO ())
         -> (a -> Pipe l i o u m r)
         -> Pipe l i o u m r
bracketP alloc free inside =
    PipeM start
  where
    start = do
        (key, seed) <- allocate alloc free
        return $ addCleanup (const $ release key) (inside seed)

-- | Add some code to be run when the given @Pipe@ cleans up.
--
-- Since 0.4.1
addCleanup :: Monad m
           => (Bool -> m ()) -- ^ @True@ if @Pipe@ ran to completion, @False@ for early termination.
           -> Pipe l i o u m r
           -> Pipe l i o u m r
addCleanup cleanup (Done r) = PipeM (cleanup True >> return (Done r))
addCleanup cleanup (HaveOutput src close x) = HaveOutput
    (addCleanup cleanup src)
    (cleanup False >> close)
    x
addCleanup cleanup (PipeM msrc) = PipeM (liftM (addCleanup cleanup) msrc)
addCleanup cleanup (NeedInput p c) = NeedInput
    (addCleanup cleanup . p)
    (addCleanup cleanup . c)
addCleanup cleanup (Leftover p i) = Leftover (addCleanup cleanup p) i

-- | The identity @Pipe@.
--
-- Since 0.5.0
idP :: Monad m => Pipe l a a r m r
idP = NeedInput (HaveOutput idP (return ())) Done

-- | Compose a left and right pipe together into a complete pipe. The left pipe
-- will be automatically closed when the right pipe finishes.
--
-- Since 0.5.0
pipe :: Monad m => Pipe l a b r0 m r1 -> Pipe Void b c r1 m r2 -> Pipe l a c r0 m r2
pipe =
    goRight (return ())
  where
    goRight final left right =
        case right of
            HaveOutput p c o -> HaveOutput (recurse p) (c >> final) o
            NeedInput rp rc  -> goLeft rp rc final left
            Done r2          -> PipeM (final >> return (Done r2))
            PipeM mp         -> PipeM (liftM recurse mp)
            Leftover _ i     -> absurd i
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

-- | Same as 'pipe', but automatically applies 'injectLeftovers' to the right @Pipe@.
--
-- Since 0.5.0
pipeL :: Monad m => Pipe l a b r0 m r1 -> Pipe b b c r1 m r2 -> Pipe l a c r0 m r2
-- Note: The following should be equivalent to the simpler:
--
--     pipeL l r = l `pipe` injectLeftovers r
--
-- However, this version tested as being significantly more efficient.
pipeL =
    goRight (return ())
  where
    goRight final left right =
        case right of
            HaveOutput p c o  -> HaveOutput (recurse p) (c >> final) o
            NeedInput rp rc   -> goLeft rp rc final left
            Done r2           -> PipeM (final >> return (Done r2))
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

-- | Run a pipeline until processing completes.
--
-- Since 0.5.0
runPipe :: Monad m => Pipe Void () Void () m r -> m r
runPipe (HaveOutput _ _ o) = absurd o
runPipe (NeedInput _ c) = runPipe (c ())
runPipe (Done r) = return r
runPipe (PipeM mp) = mp >>= runPipe
runPipe (Leftover _ i) = absurd i

-- | Transforms a @Pipe@ that provides leftovers to one which does not,
-- allowing it to be composed.
--
-- This function will provide any leftover values within this @Pipe@ to any
-- calls to @await@. If there are more leftover values than are demanded, the
-- remainder are discarded.
--
-- Since 0.5.0
injectLeftovers :: Monad m => Pipe i i o u m r -> Pipe l i o u m r
injectLeftovers =
    go []
  where
    go ls (HaveOutput p c o) = HaveOutput (go ls p) c o
    go (l:ls) (NeedInput p _) = go ls $ p l
    go [] (NeedInput p c) = NeedInput (go [] . p) (go [] . c)
    go _ (Done r) = Done r
    go ls (PipeM mp) = PipeM (liftM (go ls) mp)
    go ls (Leftover p l) = go (l:ls) p

-- | Transform the monad that a @Pipe@ lives in.
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
transPipe :: Monad m => (forall a. m a -> n a) -> Pipe l i o u m r -> Pipe l i o u n r
transPipe f (HaveOutput p c o) = HaveOutput (transPipe f p) (f c) o
transPipe f (NeedInput p c) = NeedInput (transPipe f . p) (transPipe f . c)
transPipe _ (Done r) = Done r
transPipe f (PipeM mp) =
    PipeM (f $ liftM (transPipe f) $ collapse mp)
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
transPipe f (Leftover p i) = Leftover (transPipe f p) i

-- | Apply a function to all the output values of a @Pipe@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days.
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> Pipe l i o1 u m r -> Pipe l i o2 u m r
mapOutput f =
    go
  where
    go (HaveOutput p c o) = HaveOutput (go p) c (f o)
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM (go) mp)
    go (Leftover p i) = Leftover (go p) i
{-# INLINE mapOutput #-}

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> Pipe l i o1 u m r -> Pipe l i o2 u m r
mapOutputMaybe f =
    go
  where
    go (HaveOutput p c o) = maybe id (\o' p' -> HaveOutput p' c o') (f o) (mapOutputMaybe f p)
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM (go) mp)
    go (Leftover p i) = Leftover (go p) i
{-# INLINE mapOutputMaybe #-}

-- | Apply a function to all the input values of a @Pipe@.
--
-- Since 0.5.0
mapInput :: Monad m
         => (i1 -> i2) -- ^ map initial input to new input
         -> (l2 -> Maybe l1) -- ^ map new leftovers to initial leftovers
         -> Pipe l2 i2 o u m r
         -> Pipe l1 i1 o u m r
mapInput f f' (HaveOutput p c o) = HaveOutput (mapInput f f' p) c o
mapInput f f' (NeedInput p c)    = NeedInput (mapInput f f' . p . f) (mapInput f f' . c)
mapInput _ _  (Done r)           = Done r
mapInput f f' (PipeM mp)         = PipeM (liftM (mapInput f f') mp)
mapInput f f' (Leftover p i)     = maybe id (flip Leftover) (f' i) $ mapInput f f' p

enumFromTo :: (Enum o, Eq o, Monad m)
           => o
           -> o
           -> Pipe l i o u m ()
enumFromTo start stop =
    loop start
  where
    loop i
        | i == stop = HaveOutput (Done ()) (return ()) i
        | otherwise = HaveOutput (loop (succ i)) (return ()) i
{-# INLINE enumFromTo #-}

-- | Convert a list into a source.
--
-- Since 0.3.0
sourceList :: Monad m => [a] -> Pipe l i a u m ()
sourceList =
    go
  where
    go [] = Done ()
    go (o:os) = HaveOutput (go os) (return ()) o
{-# INLINE [1] sourceList #-}

-- | The equivalent of @GHC.Exts.build@ for @Pipe@.
--
-- Since 0.4.2
build :: Monad m => (forall b. (o -> b -> b) -> b -> b) -> Pipe l i o u m ()
build g = g (\o p -> HaveOutput p (return ()) o) (return ())

{-# RULES
    "sourceList/build" forall (f :: (forall b. (a -> b -> b) -> b -> b)). sourceList (GHC.Exts.build f) = build f #-}

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

-- | Returns a tuple of the upstream and downstream results. Note that this
-- will force consumption of the entire input stream.
--
-- Since 0.5.0
withUpstream :: Monad m
             => Pipe l i o u m r
             -> Pipe l i o u m (u, r)
withUpstream down =
    down >>= go
  where
    go r =
        loop
      where
        loop = awaitE >>= either (\u -> return (u, r)) (\_ -> loop)

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
infixr 9 <+<
infixl 9 >+>

-- | Fuse together two @Pipe@s, connecting the output from the left to the
-- input of the right.
--
-- Notice that the /leftover/ parameter for the @Pipe@s must be @Void@. This
-- ensures that there is no accidental data loss of leftovers during fusion. If
-- you have a @Pipe@ with leftovers, you must first call 'injectLeftovers'.
--
-- Since 0.5.0
(>+>) :: Monad m => Pipe l a b r0 m r1 -> Pipe Void b c r1 m r2 -> Pipe l a c r0 m r2
(>+>) = pipe
{-# INLINE (>+>) #-}

-- | Same as '>+>', but reverse the order of the arguments.
--
-- Since 0.5.0
(<+<) :: Monad m => Pipe Void b c r1 m r2 -> Pipe l a b r0 m r1 -> Pipe l a c r0 m r2
(<+<) = flip pipe
{-# INLINE (<+<) #-}

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

-- | Since 1.0.4
instance MFunctor (Pipe l i o u) where
    hoist = transPipe

-- | See 'catchC' for more details.
--
-- Since 1.0.11
catchP :: (MonadBaseControl IO m, Exception e)
       => Pipe l i o u m r
       -> (e -> Pipe l i o u m r)
       -> Pipe l i o u m r
catchP p0 onErr =
    go p0
  where
    go (Done r) = Done r
    go (PipeM mp) = PipeM $ E.catch (liftM go mp) (return . onErr)
    go (Leftover p i) = Leftover (go p) i
    go (NeedInput x y) = NeedInput (go . x) (go . y)
    go (HaveOutput p c o) = HaveOutput (go p) c o
{-# INLINABLE catchP #-}

-- | The same as @flip catchP@.
--
-- Since 1.0.11
handleP :: (MonadBaseControl IO m, Exception e)
        => (e -> Pipe l i o u m r)
        -> Pipe l i o u m r
        -> Pipe l i o u m r
handleP = flip catchP
{-# INLINE handleP #-}

-- | See 'tryC' for more details.
--
-- Since 1.0.11
tryP :: (MonadBaseControl IO m, Exception e)
     => Pipe l i o u m r
     -> Pipe l i o u m (Either e r)
tryP =
    go
  where
    go (Done r) = Done (Right r)
    go (PipeM mp) = PipeM $ E.catch (liftM go mp) (return . Done . Left)
    go (Leftover p i) = Leftover (go p) i
    go (NeedInput x y) = NeedInput (go . x) (go . y)
    go (HaveOutput p c o) = HaveOutput (go p) c o
{-# INLINABLE tryP #-}

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
