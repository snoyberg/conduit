{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Conduit.Internal
    ( -- * Types
      ConduitM (..)
    , Source
    , Producer
    , Sink
    , Consumer
    , Conduit
    , ResumableSource (..)
      -- * Primitives
    , await
    , awaitForever
    , yield
    , yieldOr
    , leftover
      -- * Finalization
    , bracketP
    , addCleanup
    , setCleanup
    , clearCleanup
    , setFinalizer
      -- * Composition
    , idP
    , pipe
    , connectResume
    , runConduitM
    , (>+>)
    , (<+<)
      -- * Generalizing
    , sourceToConduitM
    , sinkToConduitM
    , conduitToConduitM
    , toProducer
    , toConsumer
      -- * Utilities
    , transConduitM
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , sourceList
    , unwrapResumable
      -- * Internal
    , getCleanup
    , dropOutput
    ) where

import Data.Maybe (mapMaybe)
import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), liftM, ap, when)
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
-- * /l/ is the type of values that may be left over from this @ConduitM@. A @ConduitM@
-- with no leftovers would use @Void@ here, and one with leftovers would use
-- the same type as the /i/ parameter. Leftovers are automatically provided to
-- the next @ConduitM@ in the monadic chain.
--
-- * /i/ is the type of values for this @ConduitM@'s input stream.
--
-- * /o/ is the type of values for this @ConduitM@'s output stream.
--
-- * /u/ is the result type from the upstream @ConduitM@.
--
-- * /m/ is the underlying monad.
--
-- * /r/ is the result type.
--
-- A basic intuition is that every @ConduitM@ produces a stream of output values
-- (/o/), and eventually indicates that this stream is terminated by sending a
-- result (/r/). On the receiving end of a @ConduitM@, these become the /i/ and /u/
-- parameters.
--
-- Since 0.5.0
data ConduitM i o m r =
    -- | Provide new output to be sent downstream. This constructor has three
    -- fields: the next @ConduitM@ to be used, a finalization function, and the
    -- output value.
    HaveOutput (ConduitM i o m r) o
    -- | Request more input from upstream. The first field takes a new input
    -- value and provides a new @ConduitM@. The second takes an upstream result
    -- value, which indicates that upstream is producing no more results.
  | NeedInput (i -> ConduitM i o m r) (ConduitM i o m r)
    -- | Processing with this @ConduitM@ is complete, providing the final result.
  | Done [i] r
    -- | Require running of a monadic action to get the next @ConduitM@.
  | ConduitM (m (ConduitM i o m r))
    -- | Return leftover input, which should be provided to future operations.
  | Cleanup (ConduitM i o m r) ([o] -> ConduitM i o m ())

instance Monad m => Functor (ConduitM i o m) where
    fmap = liftM

instance Monad m => Applicative (ConduitM i o m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (ConduitM i o m) where
    return = Done []

    HaveOutput p o   >>= fp = HaveOutput (p >>= fp)            o
    NeedInput p c    >>= fp = NeedInput  (p >=> fp)            (c >>= fp)
    Done is x        >>= fp = inject is (fp x)
    ConduitM mp      >>= fp = ConduitM      ((>>= fp) `liftM` mp)
    Cleanup p c      >>= fp = Cleanup (p >>= fp) c

inject :: Monad m => [i] -> ConduitM i o m r -> ConduitM i o m r
inject [] c = c
inject is (Done [] r) = Done is r
inject is (Done is' r) = Done (is' ++ is) r
inject is (ConduitM m) = ConduitM (liftM (inject is) m)
inject (i:is) (NeedInput f _) = inject is (f i)
inject is (HaveOutput c o) = HaveOutput (inject is c) o
inject is (Cleanup c cleanup) = Cleanup (inject is c) (inject is . cleanup)

instance MonadBase base m => MonadBase base (ConduitM i o m) where
    liftBase = lift . liftBase

instance MonadTrans (ConduitM i o) where
    lift mr = ConduitM (Done [] `liftM` mr)

instance MonadIO m => MonadIO (ConduitM i o m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (ConduitM i o m) where
    monadThrow = lift . monadThrow

instance MonadActive m => MonadActive (ConduitM i o m) where
    monadActive = lift monadActive

instance Monad m => Monoid (ConduitM i o m ()) where
    mempty = return ()
    mappend = (>>)

instance MonadResource m => MonadResource (ConduitM i o m) where
    liftResourceT = lift . liftResourceT

instance MonadReader r m => MonadReader r (ConduitM i o m) where
    ask = lift ask
    local f (HaveOutput p o) = HaveOutput (local f p) o
    local f (NeedInput p c) = NeedInput (\i -> local f (p i)) (local f c)
    local _ (Done ls x) = Done ls x
    local f (ConduitM mp) = ConduitM (local f mp)
    --local f (Leftover p i) = Leftover (local f p) i

-- Provided for doctest
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x, y, z) 0
#endif

instance MonadWriter w m => MonadWriter w (ConduitM i o m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif

    tell = lift . tell

    listen (HaveOutput p o) = HaveOutput (listen p) o
    listen (NeedInput p c) = NeedInput (\i -> listen (p i)) (listen c)
    listen (Done ls x) = Done ls (x,mempty)
    listen (ConduitM mp) =
      ConduitM $
      do (p,w) <- listen mp
         return $ do (x,w') <- listen p
                     return (x, w `mappend` w')
    --listen (Leftover p i) = Leftover (listen p) i

    pass (HaveOutput p o) = HaveOutput (pass p) o
    pass (NeedInput p c) = NeedInput (\i -> pass (p i)) (pass c)
    pass (ConduitM mp) = ConduitM $ mp >>= (return . pass)
    pass (Done ls (x,_)) = Done ls x
    --pass (Leftover p i) = Leftover (pass p) i

instance MonadState s m => MonadState s (ConduitM i o m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (ConduitM i o m)

instance MonadError e m => MonadError e (ConduitM i o m) where
    throwError = lift . throwError
    catchError (HaveOutput p o) f = HaveOutput (catchError p f) o
    catchError (NeedInput p c) f = NeedInput (\i -> catchError (p i) f) (catchError c f)
    catchError (Done ls x) _ = Done ls x
    catchError (ConduitM mp) f =
      ConduitM $ catchError (liftM (flip catchError f) mp) (\e -> return (f e))
    --catchError (Leftover p i) f = Leftover (catchError p f) i

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
-- Since 0.5.0
type Sink i m r = ConduitM i Void m r

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
data ResumableSource m o = ResumableSource (Source m o) -- FIXME remove wrapper

-- | Wait for a single input value from upstream.
--
-- Since 0.5.0
await :: ConduitM i o m (Maybe i)
await = NeedInput (Done [] . Just) (Done [] Nothing)
-- {-# RULES "await >>= maybe" forall x y. await >>= maybe x y = NeedInput y (const x) #-}
{-# INLINE [1] await #-}

-- | Wait for input forever, calling the given inner @ConduitM@ for each piece of
-- new input. Returns the upstream result type.
--
-- Since 0.5.0
awaitForever :: Monad m => (i -> ConduitM i o m r') -> ConduitM i o m ()
awaitForever inner =
    self
  where
    self = await >>= maybe (return ()) (\i -> inner i >> self)
{-# INLINE [1] awaitForever #-}

-- | Send a single output value downstream. If the downstream @ConduitM@
-- terminates, this @ConduitM@ will terminate as well.
--
-- Since 0.5.0
yield :: Monad m
      => o -- ^ output value
      -> ConduitM i o m ()
yield = HaveOutput (Done [] ())
{-# INLINE [1] yield #-}

-- | Similar to @yield@, but additionally takes a finalizer to be run if the
-- downstream @ConduitM@ terminates.
--
-- Since 0.5.0
yieldOr :: Monad m
        => o
        -> m () -- ^ finalizer
        -> ConduitM i o m ()
yieldOr o f = setFinalizer f >> yield o
{-# INLINE [1] yieldOr #-}

{-
{-# RULES
    "yield o >> p" forall o (p :: ConduitM i o m r). yield o >> p = HaveOutput p (return ()) o
  ; "mapM_ yield" mapM_ yield = sourceList
  ; "yieldOr o c >> p" forall o c (p :: ConduitM i o m r). yieldOr o c >> p = HaveOutput p c o
  #-}
-}

-- | Provide a single piece of leftover input to be consumed by the next pipe
-- in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftover :: i -> ConduitM i o m ()
leftover i = Done [i] ()
{-# INLINE [1] leftover #-}

-- | Perform some allocation and run an inner @ConduitM@. Two guarantees are given
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
         -> (a -> ConduitM i o m r)
         -> ConduitM i o m r
bracketP alloc free inside =
    ConduitM start
  where
    start = do
        (key, seed) <- allocate alloc free
        return $ addCleanup (const $ release key) (inside seed)

-- | Add some code to be run when the given @ConduitM@ cleans up.
--
-- Since 0.4.1
addCleanup :: Monad m
           => (Bool -> m ()) -- ^ @True@ if @ConduitM@ ran to completion, @False@ for early termination.
           -> ConduitM i o m r
           -> ConduitM i o m r
addCleanup cleanup (Done ls r) = ConduitM (cleanup True >> return (Done ls r))
addCleanup cleanup (HaveOutput src x) = HaveOutput
    (addCleanup cleanup src)
    x
addCleanup cleanup (ConduitM msrc) = ConduitM (liftM (addCleanup cleanup) msrc)
addCleanup cleanup (NeedInput p c) = NeedInput
    (addCleanup cleanup . p)
    (addCleanup cleanup c)
{-
--addCleanup cleanup (Leftover p i) = Leftover (addCleanup cleanup p) i
addCleanup cleanup (HaveOutput src close x) = HaveOutput
    (addCleanup cleanup src)
    (cleanup False >> close)
    x
-}

-- | The identity @ConduitM@.
--
-- Since 0.5.0
idP :: Monad m => ConduitM a a m ()
idP = setCleanup (flip Done ()) >> awaitForever yield

-- | Compose a left and right pipe together into a complete pipe. The left pipe
-- will be automatically closed when the right pipe finishes.
--
-- Since 0.5.0
pipe :: Monad m => ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r
pipe left right = goR (getCleanup left) left right

getCleanup :: Monad m
           => ConduitM i o m r
           -> ([o] -> ConduitM i o m ())
getCleanup (Cleanup _ cleanup) = cleanup
getCleanup _ = defaultCleanup -- FIXME see GOALS file warning

defaultCleanup :: Monad m => [o] -> ConduitM i o m ()
defaultCleanup _ = Done [] ()

goR :: Monad m
    => ([b] -> ConduitM a b m ())
    -> ConduitM a b m ()
    -> ConduitM b c m r
    -> ConduitM a c m r
goR cleanup _left (Done bs r) = dropOutput (cleanup bs) >> Done [] r
goR cleanup left (ConduitM m) = ConduitM (liftM (goR cleanup left) m)
goR cleanup left (NeedInput push close) = goL cleanup push close left
goR cleanup left (HaveOutput right' c) = HaveOutput (goR cleanup left right') c
goR cleanup left (Cleanup right' cleanupR) = Cleanup (goR cleanup left right') (doCleanup cleanup cleanupR)

goL :: Monad m
    => ([b] -> ConduitM a b m ())
    -> (b -> ConduitM b c m r)
    -> ConduitM b c m r
    -> ConduitM a b m ()
    -> ConduitM a c m r
goL cleanup _push close (Done as ()) = goR (inject as . cleanup) (Done [] ()) close
goL cleanup push close (ConduitM m) = ConduitM (liftM (goL cleanup push close) m)
goL cleanup push close (NeedInput pushL closeL) = NeedInput
    (goL cleanup push close . pushL)
    (goL cleanup push close closeL)
goL cleanup push _close (HaveOutput left b) = goR cleanup left (push b)
goL _cleanup push close (Cleanup left cleanup) = goL cleanup push close left

doCleanup :: Monad m
          => ([b] -> ConduitM a b m ())
          -> ([c] -> ConduitM b c m ())
          -> ([c] -> ConduitM a c m ())
doCleanup left right cs = goR left (Done [] ()) (right cs)

dropOutput :: Monad m
           => ConduitM i o m r
           -> ConduitM i o' m r
dropOutput (Done is r) = Done is r
dropOutput (ConduitM m) = ConduitM (liftM dropOutput m)
dropOutput (NeedInput push close) = NeedInput (dropOutput . push) (dropOutput close)
dropOutput (HaveOutput c _) = dropOutput c
dropOutput (Cleanup c cleanup) = Cleanup (dropOutput c) (const (dropOutput (cleanup []))) -- this doesn't feel quite right

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
connectResume (ResumableSource left0) right0 = do
    (src, r) <- goRight (getCleanup left0) left0 right0
    return (ResumableSource src, r)

goRight :: Monad m
        => ([b] -> ConduitM a b m ())
        -> ConduitM a b m ()
        -> ConduitM b c m r
        -> m (ConduitM a b m (), r)
goRight leftFinal left right =
    case right of
        --HaveOutput right' o -> absurd o
        NeedInput rp rc  -> goLeft rp rc leftFinal left
        Done ls r2       -> return (Cleanup (addLeftovers (reverse ls) left) leftFinal, r2) -- FIXME analyze
        ConduitM mp      -> mp >>= goRight leftFinal left
        --Leftover p i   -> goRight leftFinal (HaveOutput left leftFinal i) p
  where
    addLeftovers [] x = x
    addLeftovers (l:ls) x = addLeftovers ls (HaveOutput x l)

goLeft :: Monad m
       => (b -> ConduitM b c m r)
       -> ConduitM b c m r
       -> ([b] -> ConduitM a b m ())
       -> ConduitM a b m ()
       -> m (ConduitM a b m (), r)
goLeft rp rc leftFinal left =
    case left of
        HaveOutput left' o            -> goRight leftFinal left' (rp o)
        NeedInput _ lc                -> recurse lc
        Done ls ()                    -> goRight (inject ls . leftFinal) (Done ls ()) rc
        ConduitM mp                   -> mp >>= recurse
        Cleanup p leftFinal'          -> goLeft rp rc leftFinal' p
  where
    recurse = goLeft rp rc leftFinal

-- | Run a pipeline until processing completes.
--
-- Since 0.5.0
runConduitM :: Monad m => ConduitM () Void m r -> m r
runConduitM (HaveOutput _ o) = absurd o
runConduitM (NeedInput _ c) = runConduitM c
runConduitM (Done _ r) = return r
runConduitM (ConduitM mp) = mp >>= runConduitM
--runConduitM (Leftover p ()) = runConduitM p

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
transConduitM :: Monad m => (forall a. m a -> n a) -> ConduitM i o m r -> ConduitM i o n r
transConduitM f (HaveOutput p o) = HaveOutput (transConduitM f p) o
transConduitM f (NeedInput p c) = NeedInput (transConduitM f . p) (transConduitM f c)
transConduitM _ (Done ls r) = Done ls r
transConduitM f (ConduitM mp) =
    ConduitM (f $ liftM (transConduitM f) $ collapse mp)
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
            ConduitM mpipe' -> collapse mpipe'
            _ -> return pipe'
--transConduitM f (Leftover p i) = Leftover (transConduitM f p) i

-- | Apply a function to all the output values of a @ConduitM@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days.
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> ConduitM i o1 m r -> ConduitM i o2 m r
mapOutput f (HaveOutput p o) = HaveOutput (mapOutput f p) (f o)
mapOutput f (NeedInput p c) = NeedInput (mapOutput f . p) (mapOutput f c)
mapOutput _ (Done ls r) = Done ls r
mapOutput f (ConduitM mp) = ConduitM (liftM (mapOutput f) mp)
--mapOutput f (Leftover p i) = Leftover (mapOutput f p) i

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> ConduitM i o1 m r -> ConduitM i o2 m r
mapOutputMaybe f (HaveOutput p o) = maybe id (\o' p' -> HaveOutput p' o') (f o) (mapOutputMaybe f p)
mapOutputMaybe f (NeedInput p c) = NeedInput (mapOutputMaybe f . p) (mapOutputMaybe f c)
mapOutputMaybe _ (Done ls r) = Done ls r
mapOutputMaybe f (ConduitM mp) = ConduitM (liftM (mapOutputMaybe f) mp)
--mapOutputMaybe f (Leftover p i) = Leftover (mapOutputMaybe f p) i

-- | Apply a function to all the input values of a @ConduitM@.
--
-- Since 0.5.0
mapInput :: Monad m
         => (i1 -> i2) -- ^ map initial input to new input
         -> (i2 -> Maybe i1) -- ^ map new leftovers to initial leftovers
         -> ConduitM i2 o m r
         -> ConduitM i1 o m r
mapInput f f' (HaveOutput p o) = HaveOutput (mapInput f f' p) o
mapInput f f' (NeedInput p c)    = NeedInput (mapInput f f' . p . f) (mapInput f f' c)
mapInput _ f' (Done ls r)        = Done (mapMaybe f' ls) r
mapInput f f' (ConduitM mp)         = ConduitM (liftM (mapInput f f') mp)
--mapInput f f' (Leftover p i)     = maybe id (flip Leftover) (f' i) $ mapInput f f' p

-- | Convert a list into a source.
--
-- Since 0.3.0
sourceList :: Monad m => [a] -> ConduitM i a m ()
sourceList =
    go
  where
    go [] = Done [] ()
    go (o:os) = HaveOutput (go os) o
{-# INLINE [1] sourceList #-}

-- | The equivalent of @GHC.Exts.build@ for @ConduitM@.
--
-- Since 0.4.2
build :: Monad m => (forall b. (o -> b -> b) -> b -> b) -> ConduitM i o m ()
build g = g (\o p -> HaveOutput p o) (return ())

{-# RULES
    "sourceList/build" forall (f :: (forall b. (a -> b -> b) -> b -> b)). sourceList (GHC.Exts.build f) = build f
  #-}

sourceToConduitM :: Monad m => Source m o -> ConduitM i o m ()
sourceToConduitM =
    go
  where
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput _ c) = go c
    go (Done _ ()) = Done [] ()
    go (ConduitM mp) = ConduitM (liftM go mp)
    --go (Leftover p ()) = go p

sinkToConduitM :: Monad m => Sink i m r -> ConduitM i o m r
sinkToConduitM =
    go
  where
    go (HaveOutput _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (go c)
    go (Done ls r) = Done ls r
    go (ConduitM mp) = ConduitM (liftM go mp)
    --go (Leftover _ l) = error "sinkToConduitM: FIXME"

conduitToConduitM :: Monad m => Conduit i m o -> ConduitM i o m ()
conduitToConduitM =
    go
  where
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput p c) = NeedInput (go . p) (go c)
    go (Done ls ()) = Done ls ()
    go (ConduitM mp) = ConduitM (liftM go mp)
    --go (Leftover _ l) = error "conduitToConduitM: FIXME"

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
unwrapResumable (ResumableSource src) = do -- FIXME remove this
    return (src, runConduitM $ src >+> return ())

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
(>+>) :: Monad m => ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r
(>+>) = pipe
{-# INLINE (>+>) #-}

-- | Same as '>+>', but reverse the order of the arguments.
--
-- Since 0.5.0
(<+<) :: Monad m => ConduitM b c m r -> ConduitM a b m () -> ConduitM a c m r
(<+<) = flip pipe
{-# INLINE (<+<) #-}

-- | Generalize a 'Source' to a 'Producer'.
--
-- Since 1.0.0
toProducer :: Monad m => Source m a -> Producer m a
toProducer =
    go
  where
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput _ c) = go c
    go (Done _ls r) = Done [] r
    go (ConduitM mp) = ConduitM (liftM go mp)
    --go (Leftover p ()) = go p

-- | Generalize a 'Sink' to a 'Consumer'.
--
-- Since 1.0.0
toConsumer :: Monad m => Sink a m b -> Consumer a m b
toConsumer =
    go
  where
    go (HaveOutput _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (go c)
    go (Done ls r) = Done ls r
    go (ConduitM mp) = ConduitM (liftM go mp)
    --go (Leftover p l) = Leftover (go p) l

-- | Since 1.0.4
instance MFunctor (ConduitM i o) where
    hoist = transConduitM

setCleanup :: Monad m => ([o] -> ConduitM i o m ()) -> ConduitM i o m ()
setCleanup = Cleanup (Done [] ())

setFinalizer :: Monad m => m () -> ConduitM i o m ()
setFinalizer = Cleanup (Done [] ()) . const . lift

clearCleanup :: Monad m => ConduitM i o m ()
clearCleanup = setCleanup defaultCleanup
