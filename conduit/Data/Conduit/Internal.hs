{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Conduit.Internal
    ( -- * Types
      Pipe (..)
    , ResumableSource (..)
      -- * Primitives
    , awaitE
    , awaitForever
      -- * Finalization
    , bracketP
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
      -- * Utilities
    , transPipe
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , sourceList
    , withUpstream
      -- * Class
    , MFunctor (..)
    , MonadStream (..)
    , Source
    , Sink (..)
    , Conduit
    , MonadSource
    , MonadSink
    , MonadConduit
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
import Control.Monad.Trans.Control (liftWith, restoreT, MonadTransControl)

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

    Done x           >>= fp = fp x
    HaveOutput p c o >>= fp = HaveOutput (p >>= fp)            c          o
    NeedInput p c    >>= fp = NeedInput  (p >=> fp)            (c >=> fp)
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

-- | A @Source@ which has been started, but has not yet completed.
--
-- This type contains both the current state of the @Source@, and the finalizer
-- to be run to close it.
--
-- Since 0.5.0
data ResumableSource m o = ResumableSource (Pipe () () o () m ()) (m ())

-- | Wait for a single input value from upstream, terminating immediately if no
-- data is available.
--
-- Since 0.5.0
awaitPipe :: Pipe l i o u m (Maybe i)
awaitPipe = NeedInput (Done . Just) (\_ -> Done Nothing)
{-# RULES "awaitPipe >>= maybe" forall x y. awaitPipe >>= maybe x y = NeedInput y (const x) #-}
{-# INLINE [1] awaitPipe #-}

-- | This is similar to @await@, but will return the upstream result value as
-- @Left@ if available.
--
-- Since 0.5.0
awaitE :: Pipe l i o u m (Either u i)
awaitE = NeedInput (Done . Right) (Done . Left)
{-# RULES "awaitE >>= either" forall x y. awaitE >>= either x y = NeedInput y x #-}
{-# INLINE [1] awaitE #-}

-- | Send a single output value downstream. If the downstream @Pipe@
-- terminates, this @Pipe@ will terminate as well.
--
-- Since 0.5.0
yieldPipe :: Monad m
          => o -- ^ output value
          -> Pipe l i o u m ()
yieldPipe = HaveOutput (Done ()) (return ())
{-# INLINE [1] yieldPipe #-}

-- | Similar to @yield@, but additionally takes a finalizer to be run if the
-- downstream @Pipe@ terminates.
--
-- Since 0.5.0
yieldOrPipe :: Monad m
            => o
            -> m () -- ^ finalizer
            -> Pipe l i o u m ()
yieldOrPipe o f = HaveOutput (Done ()) f o
{-# INLINE [1] yieldOrPipe #-}

{-# RULES
    "yieldPipe o >> p" forall o (p :: Pipe l i o u m r). yieldPipe o >> p = HaveOutput p (return ()) o
  ; "mapM_ yieldPipe" mapM_ yieldPipe = sourceList
  ; "yieldOrPipe o c >> p" forall o c (p :: Pipe l i o u m r). yieldOrPipe o c >> p = HaveOutput p c o
  #-}

-- | Provide a single piece of leftover input to be consumed by the next pipe
-- in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftoverPipe :: l -> Pipe l i o u m ()
leftoverPipe = Leftover (Done ())
{-# INLINE [1] leftoverPipe #-}
{-# RULES "leftoverPipe l >> p" forall l (p :: Pipe l i o u m r). leftoverPipe l >> p = Leftover p l #-}

-- | Add some code to be run when the given @Pipe@ cleans up.
--
-- Since 0.4.1
addCleanupPipe :: Monad m
               => (Bool -> m ()) -- ^ @True@ if @Pipe@ ran to completion, @False@ for early termination.
               -> Pipe l i o u m r
               -> Pipe l i o u m r
addCleanupPipe cleanup (Done r) = PipeM (cleanup True >> return (Done r))
addCleanupPipe cleanup (HaveOutput src close x) = HaveOutput
    (addCleanupPipe cleanup src)
    (cleanup False >> close)
    x
addCleanupPipe cleanup (PipeM msrc) = PipeM (liftM (addCleanupPipe cleanup) msrc)
addCleanupPipe cleanup (NeedInput p c) = NeedInput
    (addCleanupPipe cleanup . p)
    (addCleanupPipe cleanup . c)
addCleanupPipe cleanup (Leftover p i) = Leftover (addCleanupPipe cleanup p) i

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
    pipe' (return ())
  where
    pipe' final left right =
        case right of
            Done r2 -> PipeM (final >> return (Done r2))
            HaveOutput p c o -> HaveOutput (pipe' final left p) (c >> final) o
            PipeM mp -> PipeM (liftM (pipe' final left) mp)
            Leftover _ i -> absurd i
            NeedInput rp rc -> upstream rp rc
      where
        upstream rp rc =
            case left of
                Done r1 -> pipe (Done r1) (rc r1)
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
    pipe' (return ())
  where
    pipe' :: Monad m => m () -> Pipe l a b r0 m r1 -> Pipe b b c r1 m r2 -> Pipe l a c r0 m r2
    pipe' final left right =
        case right of
            Done r2 -> PipeM (final >> return (Done r2))
            HaveOutput p c o -> HaveOutput (pipe' final left p) (c >> final) o
            PipeM mp -> PipeM (liftM (pipe' final left) mp)
            Leftover right' i -> pipe' final (HaveOutput left final i) right'
            NeedInput rp rc ->
                case left of
                    Done r1 -> pipe' (return ()) (Done r1) (rc r1)
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
              -> Pipe o o Void () m r
              -> m (ResumableSource m o, r)
connectResume (ResumableSource left0 leftFinal0) =
    go leftFinal0 left0
  where
    go leftFinal left right =
        case right of
            Done r2 -> return (ResumableSource left leftFinal, r2)
            PipeM mp -> mp >>= go leftFinal left
            HaveOutput _ _ o -> absurd o
            Leftover p i -> go leftFinal (HaveOutput left leftFinal i) p
            NeedInput rp rc ->
                case left of
                    Leftover p () -> go leftFinal p right
                    HaveOutput left' leftFinal' o -> go leftFinal' left' (rp o)
                    NeedInput _ lc -> go leftFinal (lc ()) right
                    Done () -> go (return ()) (Done ()) (rc ())
                    PipeM mp -> mp >>= \left' -> go leftFinal left' right

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
    go _ (Done r) = Done r
    go ls (HaveOutput p c o) = HaveOutput (go ls p) c o
    go ls (PipeM mp) = PipeM (liftM (go ls) mp)
    go ls (Leftover p l) = go (l:ls) p
    go (l:ls) (NeedInput p _) = go ls $ p l
    go [] (NeedInput p c) = NeedInput (go [] . p) (go [] . c)

-- | Transform the monad that a @Pipe@ lives in.
--
-- Note that the monad transforming function will be run multiple times,
-- resulting in unintuitive behavior in some cases. For a fuller treatment,
-- please see:
--
-- <https://github.com/snoyberg/conduit/wiki/Dealing-with-monad-transformers>
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
mapOutput f (HaveOutput p c o) = HaveOutput (mapOutput f p) c (f o)
mapOutput f (NeedInput p c) = NeedInput (mapOutput f . p) (mapOutput f . c)
mapOutput _ (Done r) = Done r
mapOutput f (PipeM mp) = PipeM (liftM (mapOutput f) mp)
mapOutput f (Leftover p i) = Leftover (mapOutput f p) i

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> Pipe l i o1 u m r -> Pipe l i o2 u m r
mapOutputMaybe f (HaveOutput p c o) = maybe id (\o' p' -> HaveOutput p' c o') (f o) (mapOutputMaybe f p)
mapOutputMaybe f (NeedInput p c) = NeedInput (mapOutputMaybe f . p) (mapOutputMaybe f . c)
mapOutputMaybe _ (Done r) = Done r
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
mapInput _ _  (Done r)           = Done r
mapInput f f' (PipeM mp)         = PipeM (liftM (mapInput f f') mp)
mapInput f f' (Leftover p i)     = maybe id (flip Leftover) (f' i) $ mapInput f f' p

-- | The equivalent of @GHC.Exts.build@ for @Pipe@.
--
-- Since 0.4.2
build :: Monad m => (forall b. (o -> b -> b) -> b -> b) -> Pipe l i o u m ()
build g = g (\o p -> HaveOutput p (return ()) o) (return ())

{-# RULES
    "sourceList/build" forall (f :: (forall b. (a -> b -> b) -> b -> b)). sourceList (GHC.Exts.build f) = build f
  #-}

sourceToPipe :: Monad m => Pipe () () o () m () -> Pipe l i o u m ()
sourceToPipe (Done ()) = Done ()
sourceToPipe (PipeM mp) = PipeM (liftM sourceToPipe mp)
sourceToPipe (NeedInput _ c) = sourceToPipe $ c ()
sourceToPipe (HaveOutput p c o) = HaveOutput (sourceToPipe p) c o
sourceToPipe (Leftover p ()) = sourceToPipe p

sinkToPipe :: Monad m => Pipe i i Void () m r -> Pipe l i o u m r
sinkToPipe =
    go . injectLeftovers
  where
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM go mp)
    go (NeedInput p c) = NeedInput (go . p) (const $ go $ c ())
    go (HaveOutput _ _ o) = absurd o
    go (Leftover _ l) = absurd l

conduitToPipe :: Monad m => Pipe i i o () m () -> Pipe l i o u m ()
conduitToPipe =
    go . injectLeftovers
  where
    go (Done ()) = Done ()
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

infixr 9 <+<
infixl 9 >+>

-- | Fuse together two @Pipe@s, connecting the output from the left to the
-- input of the right.
--
-- Notice that the /leftover/ parameter for the @Pipe@s must be @Void@. This
-- ensures that there is no accidental data loss of leftovers during fusion. If
-- you have a @Pipe@ with leftovers, you must first call 'injectLeftovers'. For
-- example:
--
-- >>> :load Data.Conduit.List
-- >>> :set -XNoMonomorphismRestriction
-- >>> let pipe = peek >>= \x -> fold (Prelude.+) 0 >>= \y -> return (x, y)
-- >>> runPipe $ sourceList [1..10] >+> injectLeftovers pipe
-- (Just 1,55)
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

    await = awaitPipe
    {-# INLINE [1] await #-}

    leftover = leftoverPipe
    {-# INLINE [1] leftover #-}

    yield = yieldPipe
    {-# INLINE yield #-}

    yieldOr = yieldOrPipe
    {-# INLINE yieldOr #-}

    liftStreamMonad = lift

    -- | Add some code to be run when the given @Pipe@ cleans up.
    --
    -- Since 0.4.1
    addCleanup = addCleanupPipe

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
