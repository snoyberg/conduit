{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Conduit.Internal.Pipe
    ( -- ** Types
      Pipe (..)
      -- ** Primitives
    , await
    , awaitE
    , awaitForever
    , yield
    , yieldM
    , leftover
    , uncons
    , unconsE
      -- ** Finalization
    , bracketP
      -- ** Composition
    , idP
    , pipe
    , pipeL
    , runPipe
    , injectLeftovers
    , (>+>)
    , (<+<)
      -- ** Exceptions
    , catchP
    , handleP
    , tryP
      -- ** Utilities
    , transPipe
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , sourceList
    , withUpstream
    , Data.Conduit.Internal.Pipe.enumFromTo
    , generalizeUpstream
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), liftM, ap)
import Control.Monad.Error.Class(MonadError(..))
import Control.Monad.Reader.Class(MonadReader(..))
import Control.Monad.RWS.Class(MonadRWS())
import Control.Monad.Writer.Class(MonadWriter(..))
import Control.Monad.State.Class(MonadState(..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO, withRunInIO)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Data.Void (Void, absurd)
import Data.Monoid (Monoid (mappend, mempty))
import Data.Semigroup (Semigroup ((<>)))
import Control.Monad.Trans.Resource
import qualified GHC.Exts
import qualified Control.Exception as E

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
    -- | Provide new output to be sent downstream. This constructor has two
    -- fields: the next @Pipe@ to be used and the output value.
    HaveOutput (Pipe l i o u m r) o
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
    {-# INLINE fmap #-}

instance Monad m => Applicative (Pipe l i o u m) where
    pure = Done
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad m => Monad (Pipe l i o u m) where
    return = pure
    {-# INLINE return #-}

    HaveOutput p o   >>= fp = HaveOutput (p >>= fp)            o
    NeedInput p c    >>= fp = NeedInput  (p >=> fp)            (c >=> fp)
    Done x           >>= fp = fp x
    PipeM mp         >>= fp = PipeM      ((>>= fp) `liftM` mp)
    Leftover p i     >>= fp = Leftover   (p >>= fp)            i

instance MonadTrans (Pipe l i o u) where
    lift mr = PipeM (Done `liftM` mr)
    {-# INLINE [1] lift #-}

instance MonadIO m => MonadIO (Pipe l i o u m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadThrow m => MonadThrow (Pipe l i o u m) where
    throwM = lift . throwM
    {-# INLINE throwM #-}


instance Monad m => Semigroup (Pipe l i o u m ()) where
    (<>) = (>>)
    {-# INLINE (<>) #-}

instance Monad m => Monoid (Pipe l i o u m ()) where
    mempty = return ()
    {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
    {-# INLINE mappend #-}
#endif

instance PrimMonad m => PrimMonad (Pipe l i o u m) where
  type PrimState (Pipe l i o u m) = PrimState m
  primitive = lift . primitive

instance MonadResource m => MonadResource (Pipe l i o u m) where
    liftResourceT = lift . liftResourceT
    {-# INLINE liftResourceT #-}

instance MonadReader r m => MonadReader r (Pipe l i o u m) where
    ask = lift ask
    {-# INLINE ask #-}
    local f (HaveOutput p o) = HaveOutput (local f p) o
    local f (NeedInput p c) = NeedInput (\i -> local f (p i)) (\u -> local f (c u))
    local _ (Done x) = Done x
    local f (PipeM mp) = PipeM (liftM (local f) $ local f mp)
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

    listen (HaveOutput p o) = HaveOutput (listen p) o
    listen (NeedInput p c) = NeedInput (\i -> listen (p i)) (\u -> listen (c u))
    listen (Done x) = Done (x,mempty)
    listen (PipeM mp) =
      PipeM $
      do (p,w) <- listen mp
         return $ do (x,w') <- listen p
                     return (x, w `mappend` w')
    listen (Leftover p i) = Leftover (listen p) i

    pass (HaveOutput p o) = HaveOutput (pass p) o
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
    catchError (HaveOutput p o) f = HaveOutput (catchError p f) o
    catchError (NeedInput p c) f = NeedInput (\i -> catchError (p i) f) (\u -> catchError (c u) f)
    catchError (Done x) _ = Done x
    catchError (PipeM mp) f =
      PipeM $ catchError (liftM (flip catchError f) mp) (\e -> return (f e))
    catchError (Leftover p i) f = Leftover (catchError p f) i

-- | Wait for a single input value from upstream.
--
-- Since 0.5.0
await :: Pipe l i o u m (Maybe i)
await = NeedInput (Done . Just) (\_ -> Done Nothing)
{-# RULES "conduit: CI.await >>= maybe" forall x y. await >>= maybe x y = NeedInput y (const x) #-}
{-# INLINE [1] await #-}

-- | This is similar to @await@, but will return the upstream result value as
-- @Left@ if available.
--
-- Since 0.5.0
awaitE :: Pipe l i o u m (Either u i)
awaitE = NeedInput (Done . Right) (Done . Left)
{-# RULES "conduit: awaitE >>= either" forall x y. awaitE >>= either x y = NeedInput y x #-}
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
yield = HaveOutput (Done ())
{-# INLINE [1] yield #-}

yieldM :: Monad m => m o -> Pipe l i o u m ()
yieldM = PipeM . liftM (HaveOutput (Done ()))
{-# INLINE [1] yieldM #-}

{-# RULES
    "CI.yield o >> p" forall o (p :: Pipe l i o u m r). yield o >> p = HaveOutput p o
  #-}

  -- Rule does not fire due to inlining of lift
  -- ; "lift m >>= CI.yield" forall m. lift m >>= yield = yieldM m

  -- FIXME: Too much inlining on mapM_, can't enforce; "mapM_ CI.yield" mapM_ yield = sourceList
  -- Maybe we can get a rewrite rule on foldr instead? Need a benchmark to back this up.

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
{-# RULES "conduit: leftover l >> p" forall l (p :: Pipe l i o u m r). leftover l >> p = Leftover p l #-}

-- | Split a pipe into head and tail.
--
-- Since 1.3.3
uncons :: forall m o pipe. (Monad m, pipe ~ Pipe Void () o () m ())
       => pipe -> m (Maybe (o, pipe))
uncons = go
  where
    go (HaveOutput p o) = pure $ Just (o, p)
    go (NeedInput _ c) = go $ c ()
    go (Done ()) = pure Nothing
    go (PipeM mp) = mp >>= go
    go (Leftover _ i) = absurd i

-- | Split a pipe into head and tail or return its result if it is done.
--
-- Since 1.3.3
unconsE :: forall m o r pipe. (Monad m, pipe ~ Pipe Void () o () m r)
        => pipe -> m (Either r (o, pipe))
unconsE = go
  where
    go (HaveOutput p o) = pure $ Right (o, p)
    go (NeedInput _ c) = go $ c ()
    go (Done r) = pure $ Left r
    go (PipeM mp) = mp >>= go
    go (Leftover _ i) = absurd i

-- | Bracket a pipe computation between allocation and release of a resource.
-- We guarantee, via the @MonadResource@ context, that the resource
-- finalization is exception safe. However, it will not necessarily be
-- /prompt/, in that running a finalizer may wait until the @ResourceT@ block
-- exits.
--
-- Since 0.5.0
bracketP :: MonadResource m
         => IO a
            -- ^ computation to run first (\"acquire resource\")
         -> (a -> IO ())
            -- ^ computation to run last (\"release resource\")
         -> (a -> Pipe l i o u m r)
            -- ^ computation to run in-between
         -> Pipe l i o u m r
            -- returns the value from the in-between computation
bracketP alloc free inside = do
  (key, seed) <- allocate alloc free
  res <- inside seed
  release key
  return res

-- | The identity @Pipe@.
--
-- Since 0.5.0
idP :: Monad m => Pipe l a a r m r
idP = NeedInput (HaveOutput idP) Done

-- | Compose a left and right pipe together into a complete pipe.
--
-- Since 0.5.0
pipe :: Monad m => Pipe l a b r0 m r1 -> Pipe Void b c r1 m r2 -> Pipe l a c r0 m r2
pipe =
    goRight
  where
    goRight left right =
        case right of
            HaveOutput p o   -> HaveOutput (recurse p) o
            NeedInput rp rc  -> goLeft rp rc left
            Done r2          -> Done r2
            PipeM mp         -> PipeM (liftM recurse mp)
            Leftover _ i     -> absurd i
      where
        recurse = goRight left

    goLeft rp rc left =
        case left of
            HaveOutput left' o        -> goRight left' (rp o)
            NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
            Done r1                   -> goRight (Done r1) (rc r1)
            PipeM mp                  -> PipeM (liftM recurse mp)
            Leftover left' i          -> Leftover (recurse left') i
      where
        recurse = goLeft rp rc

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
    goRight
  where
    goRight left right =
        case right of
            HaveOutput p o    -> HaveOutput (recurse p) o
            NeedInput rp rc   -> goLeft rp rc left
            Done r2           -> Done r2
            PipeM mp          -> PipeM (liftM recurse mp)
            Leftover right' i -> goRight (HaveOutput left i) right'
      where
        recurse = goRight left

    goLeft rp rc left =
        case left of
            HaveOutput left' o        -> goRight left' (rp o)
            NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
            Done r1                   -> goRight (Done r1) (rc r1)
            PipeM mp                  -> PipeM (liftM recurse mp)
            Leftover left' i          -> Leftover (recurse left') i
      where
        recurse = goLeft rp rc

-- | Run a pipeline until processing completes.
--
-- Since 0.5.0
runPipe :: Monad m => Pipe Void () Void () m r -> m r
runPipe (HaveOutput _ o) = absurd o
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
    go ls (HaveOutput p o) = HaveOutput (go ls p) o
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
transPipe f (HaveOutput p o) = HaveOutput (transPipe f p) o
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
    go (HaveOutput p o) = HaveOutput (go p) (f o)
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
    go (HaveOutput p o) = maybe id (\o' p' -> HaveOutput p' o') (f o) (go p)
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
mapInput f f' (HaveOutput p o) = HaveOutput (mapInput f f' p) o
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
        | i == stop = HaveOutput (Done ()) i
        | otherwise = HaveOutput (loop (succ i)) i
{-# INLINE enumFromTo #-}

-- | Convert a list into a source.
--
-- Since 0.3.0
sourceList :: Monad m => [a] -> Pipe l i a u m ()
sourceList =
    go
  where
    go [] = Done ()
    go (o:os) = HaveOutput (go os) o
{-# INLINE [1] sourceList #-}

-- | The equivalent of @GHC.Exts.build@ for @Pipe@.
--
-- Since 0.4.2
build :: Monad m => (forall b. (o -> b -> b) -> b -> b) -> Pipe l i o u m ()
build g = g (\o p -> HaveOutput p o) (return ())

{-# RULES
    "sourceList/build" forall (f :: (forall b. (a -> b -> b) -> b -> b)). sourceList (GHC.Exts.build f) = build f #-}

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

-- | See 'catchC' for more details.
--
-- Since 1.0.11
catchP :: (MonadUnliftIO m, E.Exception e)
       => Pipe l i o u m r
       -> (e -> Pipe l i o u m r)
       -> Pipe l i o u m r
catchP p0 onErr =
    go p0
  where
    go (Done r) = Done r
    go (PipeM mp) = PipeM $ withRunInIO $ \run ->
      E.catch (run (liftM go mp)) (return . onErr)
    go (Leftover p i) = Leftover (go p) i
    go (NeedInput x y) = NeedInput (go . x) (go . y)
    go (HaveOutput p o) = HaveOutput (go p) o
{-# INLINABLE catchP #-}

-- | The same as @flip catchP@.
--
-- Since 1.0.11
handleP :: (MonadUnliftIO m, E.Exception e)
        => (e -> Pipe l i o u m r)
        -> Pipe l i o u m r
        -> Pipe l i o u m r
handleP = flip catchP
{-# INLINE handleP #-}

-- | See 'tryC' for more details.
--
-- Since 1.0.11
tryP :: (MonadUnliftIO m, E.Exception e)
     => Pipe l i o u m r
     -> Pipe l i o u m (Either e r)
tryP p = (fmap Right p) `catchP` (return . Left)
{-# INLINABLE tryP #-}

-- | Generalize the upstream return value for a @Pipe@ from unit to any type.
--
-- Since 1.1.5
generalizeUpstream :: Monad m => Pipe l i o () m r -> Pipe l i o u m r
generalizeUpstream =
    go
  where
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput x y) = NeedInput (go . x) (\_ -> go (y ()))
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p l) = Leftover (go p) l
{-# INLINE generalizeUpstream #-}

{- Rules don't fire due to inlining of lift
{-# RULES "conduit: Pipe: lift x >>= f" forall m f. lift m >>= f = PipeM (liftM f m) #-}
{-# RULES "conduit: Pipe: lift x >> f" forall m f. lift m >> f = PipeM (liftM (\_ -> f) m) #-}
-}
