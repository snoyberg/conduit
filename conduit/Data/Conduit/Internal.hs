{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Data.Conduit.Internal
    ( -- * Types
      Pipe (..)
    , Source
    , GSource
    , Sink
    , GSink
    , GLSink
    , GInfSink
    , GLInfSink
    , Conduit
    , GConduit
    , GLConduit
    , GInfConduit
    , GLInfConduit
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
      -- * Generalizing
    , sourceToPipe
    , sinkToPipe
    , conduitToPipe
      -- * Utilities
    , transPipe
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , sourceList
    , withUpstream
    , unwrapResumable
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), liftM, ap, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Base (MonadBase (liftBase))
import Data.Void (Void, absurd)
import Data.Monoid (Monoid (mappend, mempty))
import Control.Monad.Trans.Resource
import qualified GHC.Exts
import qualified Data.IORef as I

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
  | Done (m ()) r
    -- | Require running of a monadic action to get the next @Pipe@.
  | PipeM (m (Pipe l i o u m r))
    -- | Return leftover input, which should be provided to future operations.
  | Leftover (Pipe l i o u m r) l

pass :: Monad m => m ()
pass = return ()

done :: Monad m => r -> Pipe l i o u m r
done = Done pass

instance Monad m => Functor (Pipe l i o u m) where
    fmap = liftM

instance Monad m => Applicative (Pipe l i o u m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Pipe l i o u m) where
    return = done

    Done c x         >>= fp = PipeM      (c >> return (fp x))
    HaveOutput p c o >>= fp = HaveOutput (p >>= fp)            c          o
    NeedInput p c    >>= fp = NeedInput  (p >=> fp)            (c >=> fp)
    PipeM mp         >>= fp = PipeM      ((>>= fp) `liftM` mp)
    Leftover p i     >>= fp = Leftover   (p >>= fp)            i

instance MonadBase base m => MonadBase base (Pipe l i o u m) where
    liftBase = lift . liftBase

instance MonadTrans (Pipe l i o u) where
    lift mr = PipeM (done `liftM` mr)

instance MonadIO m => MonadIO (Pipe l i o u m) where
    liftIO = lift . liftIO

instance Monad m => Monoid (Pipe l i o u m ()) where
    mempty = return ()
    mappend = (>>)

-- | Provides a stream of output values, without consuming any input or
-- producing a final result.
--
-- Since 0.5.0
type Source m o = Pipe () () o () m ()

-- | Generalized 'Source'.
--
-- Since 0.5.0
type GSource m o = forall l i u. Pipe l i o u m ()

-- | Consumes a stream of input values and produces a final result, without
-- producing any output.
--
-- Since 0.5.0
type Sink i m r = Pipe i i Void () m r

-- | Generalized 'Sink' without leftovers.
--
-- Since 0.5.0
type GSink i m r = forall l o u. Pipe l i o u m r

-- | Generalized 'Sink' with leftovers.
--
-- Since 0.5.0
type GLSink i m r = forall o u. Pipe i i o u m r

-- | Generalized 'Sink' without leftovers returning upstream result.
--
-- Since 0.5.0
type GInfSink i m = forall l o r. Pipe l i o r m r

-- | Generalized 'Sink' with leftovers returning upstream result.
--
-- Since 0.5.0
type GLInfSink i m = forall o r. Pipe i i o r m r

-- | Consumes a stream of input values and produces a stream of output values,
-- without producing a final result.
--
-- Since 0.5.0
type Conduit i m o = Pipe i i o () m ()

-- | Generalized conduit without leftovers.
--
-- Since 0.5.0
type GConduit i m o = forall l u. Pipe l i o u m ()

-- | Generalized conduit with leftovers.
--
-- Since 0.5.0
type GLConduit i m o = forall u. Pipe i i o u m ()

-- | Generalized conduit without leftovers returning upstream result.
--
-- Since 0.5.0
type GInfConduit i m o = forall l r. Pipe l i o r m r

-- | Generalized conduit with leftovers returning upstream result.
--
-- Since 0.5.0
type GLInfConduit i m o = forall r. Pipe i i o r m r

-- | A @Source@ which has been started, but has not yet completed.
--
-- This type contains both the current state of the @Source@, and the finalizer
-- to be run to close it.
--
-- Since 0.5.0
data ResumableSource m o = ResumableSource (Source m o) (m ())

-- | Wait for a single input value from upstream, terminating immediately if no
-- data is available.
--
-- Since 0.5.0
await :: Monad m => Pipe l i o u m (Maybe i)
await = NeedInput (done . Just) (\_ -> done Nothing)
{-# RULES "await >>= maybe" forall x y. await >>= maybe x y = NeedInput y (const x) #-}
{-# INLINE [1] await #-}

-- | This is similar to @await@, but will return the upstream result value as
-- @Left@ if available.
--
-- Since 0.5.0
awaitE :: Monad m => Pipe l i o u m (Either u i)
awaitE = NeedInput (done . Right) (done . Left)
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
yield = HaveOutput pass pass
{-# INLINE [1] yield #-}

-- | Similar to @yield@, but additionally takes a finalizer to be run if the
-- downstream @Pipe@ terminates.
--
-- Since 0.5.0
yieldOr :: Monad m
        => o
        -> m () -- ^ finalizer
        -> Pipe l i o u m ()
yieldOr o f = HaveOutput pass f o
{-# INLINE [1] yieldOr #-}

{-# RULES
    "yield o >> p" forall o (p :: Pipe l i o u m r). yield o >> p = HaveOutput p (return ()) o
  ; "mapM_ yield" mapM_ yield = sourceList
  ; "yieldOr o c >> p" forall o c (p :: Pipe l i o u m r). yieldOr o c >> p = HaveOutput p c o
  #-}

-- | Provide a single piece of leftover input to be consumed by the next pipe
-- in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftover :: Monad m => l -> Pipe l i o u m ()
leftover = Leftover pass
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
addCleanup cleanup (Done c r) = Done (c >> cleanup True) r
addCleanup cleanup (HaveOutput src close x) = HaveOutput
    (addCleanup cleanup src)
    (close >> cleanup False)
    x
addCleanup cleanup (PipeM msrc) = PipeM (liftM (addCleanup cleanup) msrc)
addCleanup cleanup (NeedInput p c) = NeedInput
    (addCleanup cleanup . p)
    (addCleanup cleanup . c)
addCleanup cleanup (Leftover p i) = Leftover (addCleanup cleanup p) i

onSuccess :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
onSuccess cleanup = addCleanup (\b -> if b then cleanup else pass)

-- | The identity @Pipe@.
--
-- Since 0.5.0
idP :: Monad m => Pipe l a a r m r
idP = NeedInput (HaveOutput idP pass) done

-- | Compose a left and right pipe together into a complete pipe. The left pipe
-- will be automatically closed when the right pipe finishes.
--
-- Since 0.5.0
pipe :: Monad m => Pipe l a b r0 m r1 -> Pipe Void b c r1 m r2 -> Pipe l a c r0 m r2
pipe =
    pipe' pass
  where
    pipe' final left right =
        case right of
            Done finalR r2 -> Done (final >> finalR) r2
            HaveOutput p c o -> HaveOutput (pipe' final left p) (final >> c) o
            PipeM mp -> PipeM (liftM (pipe' final left) mp)
            Leftover _ i -> absurd i
            NeedInput rp rc -> upstream rp rc
      where
        upstream rp rc =
            case left of
                Done final' r1 -> PipeM
                    (final' >> return (pipe (Done pass r1) (rc r1)))
                HaveOutput left' final' o -> pipe' final' left' (rp o)
                PipeM mp -> PipeM (liftM (\left' -> pipe' final left' right) mp)
                Leftover left' i -> Leftover (pipe' final left' right) i
                NeedInput left' lc -> NeedInput
                    (\a -> pipe' final (left' a) right)
                    (\r0 -> pipe' final (lc r0) right)

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
    pipe' pass
  where
    pipe' :: Monad m => m () -> Pipe l a b r0 m r1 -> Pipe b b c r1 m r2 -> Pipe l a c r0 m r2
    pipe' final left right =
        case right of
            Done finalR r2 -> Done (final >> finalR) r2
            HaveOutput p c o -> HaveOutput (pipe' final left p) (final >> c) o
            PipeM mp -> PipeM (liftM (pipe' final left) mp)
            Leftover right' i -> pipe' final (HaveOutput left final i) right'
            NeedInput rp rc ->
                case left of
                    Done final' r1 -> PipeM
                        (final' >> return (pipeL (done r1) (rc r1)))
                    HaveOutput left' final' o -> pipe' final' left' (rp o)
                    PipeM mp -> PipeM (liftM (\left' -> pipe' final left' right) mp)
                    NeedInput left' lc -> NeedInput
                        (\a -> pipe' final (left' a) right)
                        (\r0 -> pipe' final (lc r0) right)
                    Leftover left' i -> Leftover (pipe' final left' right) i

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
connectResume (ResumableSource left0 leftFinal0) =
    go leftFinal0 left0
  where
    go leftFinal left right =
        case right of
            Done finalR r2 ->
                finalR >> return (ResumableSource left leftFinal, r2)
            PipeM mp -> mp >>= go leftFinal left
            HaveOutput _ _ o -> absurd o
            Leftover p i -> go leftFinal (HaveOutput left leftFinal i) p
            NeedInput rp rc ->
                case left of
                    Leftover p () -> go leftFinal p right
                    HaveOutput left' leftFinal' o -> go leftFinal' left' (rp o)
                    NeedInput _ lc -> go leftFinal (lc ()) right
                    Done final' () -> final' >> go pass pass (rc ())
                    PipeM mp -> mp >>= \left' -> go leftFinal left' right

-- | Run a pipeline until processing completes.
--
-- Since 0.5.0
runPipe :: Monad m => Pipe Void () Void () m r -> m r
runPipe (HaveOutput _ _ o) = absurd o
runPipe (NeedInput _ c) = runPipe (c ())
runPipe (Done c r) = c >> return r
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
    go _ (Done c r) = Done c r
    go ls (HaveOutput p c o) = HaveOutput (go ls p) c o
    go ls (PipeM mp) = PipeM (liftM (go ls) mp)
    go ls (Leftover p l) = go (l:ls) p
    go (l:ls) (NeedInput p _) = go ls $ p l
    go [] (NeedInput p c) = NeedInput (go [] . p) (go [] . c)

-- | Transform the monad that a @Pipe@ lives in.
--
-- Since 0.4.0
transPipe :: Monad m => (forall a. m a -> n a) -> Pipe l i o u m r -> Pipe l i o u n r
transPipe f (HaveOutput p c o) = HaveOutput (transPipe f p) (f c) o
transPipe f (NeedInput p c) = NeedInput (transPipe f . p) (transPipe f . c)
transPipe f (Done c r) = Done (f c) r
transPipe f (PipeM mp) = PipeM (f $ liftM (transPipe f) mp)
transPipe f (Leftover p i) = Leftover (transPipe f p) i

-- | Apply a function to all the output values of a @Pipe@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days.
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> Pipe l i o1 u m r -> Pipe l i o2 u m r
mapOutput f (HaveOutput p c o) = HaveOutput (mapOutput f p) c (f o)
mapOutput f (NeedInput p c) = NeedInput (mapOutput f . p) (mapOutput f . c)
mapOutput _ (Done c r) = Done c r
mapOutput f (PipeM mp) = PipeM (liftM (mapOutput f) mp)
mapOutput f (Leftover p i) = Leftover (mapOutput f p) i

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> Pipe l i o1 u m r -> Pipe l i o2 u m r
mapOutputMaybe f (HaveOutput p c o) = maybe id (\o' p' -> HaveOutput p' c o') (f o) (mapOutputMaybe f p)
mapOutputMaybe f (NeedInput p c) = NeedInput (mapOutputMaybe f . p) (mapOutputMaybe f . c)
mapOutputMaybe _ (Done c r) = Done c r
mapOutputMaybe f (PipeM mp) = PipeM (liftM (mapOutputMaybe f) mp)
mapOutputMaybe f (Leftover p i) = Leftover (mapOutputMaybe f p) i

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
mapInput _ _  (Done c r)         = Done c r
mapInput f f' (PipeM mp)         = PipeM (liftM (mapInput f f') mp)
mapInput f f' (Leftover p i)     = maybe id (flip Leftover) (f' i) $ mapInput f f' p

-- | Convert a list into a source.
--
-- Since 0.3.0
sourceList :: Monad m => [a] -> Pipe l i a u m ()
sourceList =
    go
  where
    go [] = pass
    go (o:os) = HaveOutput (go os) pass o
{-# INLINE [1] sourceList #-}

-- | The equivalent of @GHC.Exts.build@ for @Pipe@.
--
-- Since 0.4.2
build :: Monad m => (forall b. (o -> b -> b) -> b -> b) -> Pipe l i o u m ()
build g = g (\o p -> HaveOutput p (return ()) o) (return ())

{-# RULES
    "sourceList/build" forall (f :: (forall b. (a -> b -> b) -> b -> b)). sourceList (GHC.Exts.build f) = build f
  #-}

sourceToPipe :: Monad m => Source m o -> Pipe l i o u m ()
sourceToPipe (Done c ()) = Done c ()
sourceToPipe (PipeM mp) = PipeM (liftM sourceToPipe mp)
sourceToPipe (NeedInput _ c) = sourceToPipe $ c ()
sourceToPipe (HaveOutput p c o) = HaveOutput (sourceToPipe p) c o
sourceToPipe (Leftover p ()) = sourceToPipe p

sinkToPipe :: Monad m => Sink i m r -> Pipe l i o u m r
sinkToPipe =
    go . injectLeftovers
  where
    go (Done c r) = Done c r
    go (PipeM mp) = PipeM (liftM go mp)
    go (NeedInput p c) = NeedInput (go . p) (const $ go $ c ())
    go (HaveOutput _ _ o) = absurd o
    go (Leftover _ l) = absurd l

conduitToPipe :: Monad m => Conduit i m o -> Pipe l i o u m ()
conduitToPipe =
    go . injectLeftovers
  where
    go (Done c ()) = Done c ()
    go (PipeM mp) = PipeM (liftM go mp)
    go (NeedInput p c) = NeedInput (go . p) (const $ go $ c ())
    go (HaveOutput p c o) = HaveOutput (go p) c o
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
