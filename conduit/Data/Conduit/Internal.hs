-- FIXME add back the ConduitM type for better error messages
-- FIXME deprecate mapInput, mapOutput
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Conduit.Internal
    ( -- * Types
      Pipe (..)
    -- FIXME ConduitM (..)
      -- * Primitives
    , await
    , awaitForever
    , yield
    , tryYield
    , yieldOr
    , leftover
    , draw
    , checkDownstream
    , closeDownstream
      -- * Finalization
    , bracketP
    , addCleanup
      -- * Composition
    , idP
    , pipe
    , connectResume
    , runPipe
    , closePipe
    , (>+>)
    , (<+<)
    , haltPipe
    , fromDown
    , disallowTerm
      -- * Utilities
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , sourceList
    ) where

import Debug.Trace
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
import Unsafe.Coerce

data Pipe i o d t m r
    = Pure [i] r
    | M (m (Pipe i o d t m r))
    | Yield (Pipe i o d t m r) ([o] -> d -> Pipe i o d t m r) o
    | Empty ([o] -> d -> Pipe i o d t m r)
    | Await (i -> Pipe i o d t m r) (Pipe i o d t m r)
    | Check (Pipe i o d t m r) ([o] -> d -> Pipe i o d t m r)
    | Terminate [i] t

instance Monad m => Functor (Pipe i o d t m) where
    fmap = liftM

instance Monad m => Applicative (Pipe i o d t m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Pipe i o d t m) where
    return = Pure []

    Pure [] r >>= f = f r
    Pure is r >>= f = inject is (f r)
    M m >>= f = M (liftM (>>= f) m)
    Yield next done o >>= f = Yield (next >>= f) (\x y -> done x y >>= f) o
    Empty done >>= f = Empty (\x y -> done x y >>= f)
    Await next done >>= f = Await (next >=> f) (done >>= f)
    Check next done >>= f = Check (next >>= f) (\x y -> done x y >>= f)
    Terminate is t >>= _ = Terminate is t
    {-# INLINE (>>=) #-}

inject :: Monad m
       => [i]
       -> Pipe i o d t m r
       -> Pipe i o d t m r
inject [] = id
inject leftovers@(i:is) =
    go
  where
    go (Pure is' r) = Pure (is' ++ leftovers) r
    go (M m) = M (liftM go m)
    go (Yield more done o) = Yield (go more) (go .: done) o
    go (Empty done) = Empty (go .: done)
    go (Await more _) = inject is (more i)
    go (Check more done) = Check (go more) (go .: done)
    go (Terminate is' t) = Terminate (is' ++ leftovers) t

instance MonadBase base m => MonadBase base (Pipe i o d t m) where
    liftBase = lift . liftBase

instance MonadTrans (Pipe i o d t) where
    lift = M . liftM (Pure [])

instance MonadIO m => MonadIO (Pipe i o d t m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (Pipe i o d t m) where
    monadThrow = lift . monadThrow

instance MonadActive m => MonadActive (Pipe i o d t m) where
    monadActive = lift monadActive

instance Monad m => Monoid (Pipe i o d t m ()) where
    mempty = return ()
    mappend = (>>)

instance MonadResource m => MonadResource (Pipe i o d t m) where
    liftResourceT = lift . liftResourceT

instance MonadReader r m => MonadReader r (Pipe i o d t m) where
    ask = lift ask
    --local f (Yield p d o) = Yield (local f p) (local o
    local f (Await p d) = Await (local f . p) (local f d)
    local _ (Pure is x) = Pure is x
    local f (M mp) = M (local f mp)

-- Provided for doctest
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x, y, z) 0
#endif

instance MonadWriter w m => MonadWriter w (Pipe i o d t m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif

    tell = lift . tell

    listen (Yield p d o) = Yield (listen p) (listen .: d) o
    listen (Await p d) = Await (listen . p) (listen d)
    listen (Pure is x) = Pure is (x, mempty)
    listen (M mp) =
      M $
      do (p,w) <- listen mp
         return $ do (x,w') <- listen p
                     return (x, w `mappend` w')

    pass (Yield p d o) = Yield (pass p) (pass .: d) o
    pass (Await p d) = Await (pass . p) (pass d)
    pass (M mp) = M $ mp >>= (return . pass)
    pass (Pure is (x,w)) = M $ pass $ return (Pure is x, w)

instance MonadState s m => MonadState s (Pipe i o d t m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (Pipe i o d t m)

instance MonadError e m => MonadError e (Pipe i o d t m) where
    throwError = lift . throwError
    catchError (Yield p d o) f = Yield (catchError p f) (\x y -> catchError (d x y) f) o
    catchError (Check more done) f = Check (catchError more f) (\x y -> catchError (done x y) f)
    catchError (Await p d) f = Await (\i -> catchError (p i) f) (catchError d f)
    catchError (Pure is x) _ = Pure is x
    catchError (Terminate is t) _ = Terminate is t
    catchError (Empty done) f = Empty (\x y -> catchError (done x y) f)
    catchError (M mp) f =
      M $ catchError (liftM (flip catchError f) mp) (\e -> return (f e))

(.:) f g x y = f (g x y)

instance MFunctor (Pipe i o d t) where
    hoist f =
        go
      where
        go (Yield p d o) = Yield (go p) (go .: d) o
        go (Await p d) = Await (go . p) (go d)
        go (Pure is r) = Pure is r
        go (Check p d) = Check (go p) (go .: d)
        go (M mp) =
            M (f $ liftM go $ collapse mp)
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
                    M mpipe' -> collapse mpipe'
                    _ -> return pipe'

fuse :: Monad m
     => Pipe i j b a m a
     -> Pipe j k c b m b
     -> Pipe i k c a m a
fuse up0 down0 = error "fuse"
{-
    fuseStep' (down0 mc)
  where
    fuseStep' (Pure b) =
        case mc of
            Just _ -> go (up0 (Just b))
            Nothing -> Empty (const (go (up0 (Just b))))
      where
        go (Pure a) = Pure a
        go (M up) = M (liftM go up)
        go (Yield _ up _) = go (up b)
        go (Await up upDone) = Await (go . up) (go upDone)
    fuseStep' (M down) = M (liftM fuseStep' down)
    fuseStep' (Yield down empty k) = Yield (fuseStep' down) (fuseStep' . empty) k
    fuseStep' (Await more none) =
        go (up0 Nothing)
      where
        go (Pure a) = fuseStep (\_ -> Pure a) (const none) mc
        go (M up) = M (liftM go up)
        go (Yield up upDone j) = fuseStep (maybe upDone up) (const (more j)) mc
        go (Await up upDone) = Await (go up) (go . upDone)
        -}

haltPipe :: Monad m => Pipe i o d t m d
haltPipe = Empty $ const $ Pure []

fromDown :: Monad m
         => Pipe i o () t' m ()
         -> Pipe i o d t m d
fromDown p0 = do
    unsafeCoerce p0
    Empty $ \_ d -> Pure [] d
{-
    -- FIXME add comment explaining that this is about too much yielding
    --go (Check p0 $ \_ d -> Pure [] d)
    go p0
  where
    go (Pure is ()) = Empty $ \_os d -> Pure is d
    go (M m) = M (liftM go m)
    go (Yield more done o) = Yield (go more) (\os _ -> go (done os ())) o
    go (Empty done) = Empty $ \os _ -> go (done os ())
    go (Await more done) = Await (go . more) (go done)
    go (Check more done) = Check (go more) (\os _ -> go (done os ()))
    go (Terminate is _) = go (Pure is ())
    -}
{-
fromDown :: Monad m
         => Pipe i o () m ()
         -> Pipe i o d m d
fromDown (Pipe p) = Pipe $ \md -> do
    res <- ignoreD $ p $
        case md of
            Nothing -> Nothing
            Just (Endpoint o _) -> Just $ Endpoint o ()
    down@(Endpoint _ d) <- getDown md
    case res of
        PipeTerm (Endpoint is _) -> do
            down@(Endpoint _ d) <- getDown md
            return $ PipeCont (Endpoint is d) (Just down)
        PipeCont (Endpoint is _) md' -> do
            down@(Endpoint _ d) <- getDown md'
            return $ PipeCont (Endpoint is d) (Just down)
  where
    getDown md =
        case md of
            Just d -> return d
            Nothing -> Empty return

    ignoreD :: Monad m
            => Step i o (Endpoint o ()) m (PipeRes i o () ())
            -> Step i o (Endpoint o d) m (PipeRes i o d ())
    ignoreD (Pure (PipeTerm x)) = Pure (PipeTerm x)
    ignoreD (Pure (PipeCont (Endpoint is _) _)) = Pure $ PipeTerm $ Endpoint is ()
    ignoreD (M m) = M (liftM ignoreD m)
    ignoreD (Await f g) = Await (ignoreD . f) (ignoreD g)
    ignoreD (Yield f o) = Yield (ignoreD f) o
-}

-- | The identity @ConduitM@.
--
-- Since 0.5.0
idP :: Pipe i i r t m r
idP =
    Check pull Pure
  where
    pull = Await more done
    more = Yield pull Pure
    done = Empty Pure

-- | Compose a left and right pipe together into a complete pipe. The left pipe
-- will be automatically closed when the right pipe finishes.
--
-- Since 0.5.0
pipe :: Monad m
     => Pipe i j b t m a
     -> Pipe j k c b m b
     -> Pipe i k c t m a
pipe up (Pure js0 b) =
    -- This is the tricky bit. We need to ensure that downstream closes
    -- before we do. Ideally, this would be expressed in the type system
    -- itself, but such a construction is more clumsy to use.
    Empty $ \_ _ -> close js0 up
  where
    -- FIXME remove duplication with runPipe
    close _js (Pure is a) = Pure is a
    close js (M m) = M (liftM (close js) m)
    -- We need to make sure that the leftovers are only provided once.
    close js (Yield _ done _) = close [] (done js b)
    close js (Empty done) = close [] (done js b)
    close js (Await more done) = Await (close js . more) (close js done)
    close js (Check _ done) = close [] (done js b)
    close _js (Terminate is t) = Terminate is t
pipe up (M m) = M (liftM (pipe up) m)
pipe up (Yield more done o) = Yield (pipe up more) (pipe up .: done) o
pipe up (Empty done) = Empty (pipe up .: done)
pipe up0 (Await moreD doneD) =
    go up0
  where
    go up@Pure{} = pipe up doneD
    go (M m) = M (liftM go m)
    go (Yield moreU _ o) = pipe moreU (moreD o)
    go up@Empty{} = pipe up doneD
    go (Await moreU doneU) = Await (go . moreU) (go doneU)
    go (Check moreU _) = go moreU
    go up@Terminate{} = pipe up doneD
pipe up (Check more done) = Check (pipe up more) (pipe up .: done)
pipe up (Terminate is t) = pipe up (Pure is t)

-- | Send a single output value downstream. If the downstream @ConduitM@
-- terminates, this @ConduitM@ will terminate as well.
--
-- Since 0.5.0
yield :: Monad m => o -> Pipe i o d d m ()
yield = Yield (Pure [] ()) (const $ Terminate [])
{-# INLINE [1] yield #-}
{-# RULES "yield o >> p" forall o p. yield o >> p = Yield p (const $ Terminate []) o #-}

tryYield :: Monad m => o -> Pipe i o d t m (Maybe ([o], d))
tryYield = Yield (Pure [] Nothing) (\os d -> Pure [] $ Just (os, d))

-- | Similar to @yield@, but additionally takes a finalizer to be run if the
-- downstream @ConduitM@ terminates.
--
-- Since 0.5.0
yieldOr :: Monad m => o -> m () -> Pipe i o d d m ()
--yieldOr o f = Check (Yield (Pure [] ()) o) (\_ d -> M (f >> return (Terminate [] d))) -- FIXME analyze this more
yieldOr o f = Yield (Pure [] ()) (\_ d -> M (f >> return (Terminate [] d))) o

-- | Wait for a single input value from upstream.
--
-- Since 0.5.0
await :: Monad m => Pipe i o d t m (Maybe i)
await = Await (Pure [] . Just) (Pure [] Nothing)
{-# RULES "await >>= maybe" forall x y. await >>= maybe x y = Await y x #-}
{-# INLINE [1] await #-}

-- | Wait for input forever, calling the given inner @ConduitM@ for each piece of
-- new input. Returns the upstream result type.
--
-- Since 0.5.0
awaitForever :: Monad m => (i -> Pipe i o d t m r') -> Pipe i o d t m ()
awaitForever inner =
    loop
  where
    loop = Await (\i -> inner i >> loop) (Pure [] ())

-- | Provide a single piece of leftover input to be consumed by the next pipe
-- in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftover :: Monad m => i -> Pipe i o d t m ()
leftover i = Pure [i] ()

-- | Run a pipeline until processing completes.
--
-- Since 0.5.0
runPipe :: Monad m
        => Pipe i o () Void m r
        -> m r
runPipe (Pure _ r) = return r
runPipe (M m) = m >>= runPipe
runPipe (Yield _ done _) = runPipe (done [] ())
runPipe (Empty done) = runPipe (done [] ())
runPipe (Await _ done) = runPipe done
runPipe (Check _ done) = runPipe (done [] ())
runPipe (Terminate _ t) = absurd t

closePipe :: Monad m
        => Pipe i o () t m r
        -> m ()
closePipe (Pure _ _) = return ()
closePipe (M m) = m >>= closePipe
closePipe (Yield _ done _) = closePipe (done [] ())
closePipe (Empty done) = closePipe (done [] ())
closePipe (Await _ done) = closePipe done
closePipe (Check _ done) = closePipe (done [] ())
closePipe (Terminate _ _) = return ()

-- | Apply a function to all the output values of a @ConduitM@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days.
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> Pipe i o1 d t m r -> Pipe i o2 d t m r
mapOutput f = (`pipe` mapPipe f)

mapPipe :: Monad m => (a -> b) -> Pipe a b r t m r
mapPipe f =
    go
  where
    go = await >>= maybe haltPipe (\x -> tryYield (f x) >>= maybe go (return . snd))

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> Pipe i o1 d t m r -> Pipe i o2 d t m r
mapOutputMaybe f = (`pipe` mapMaybePipe f)

mapMaybePipe :: Monad m => (a -> Maybe b) -> Pipe a b r t m r
mapMaybePipe f =
    go
  where
    go = await >>= maybe haltPipe (maybe go (\x -> tryYield x >>= maybe go (return . snd)) . f)

-- | Apply a function to all the input values of a @ConduitM@.
--
-- Since 0.5.0
mapInput :: (Show i1, Show i2, Monad m)
         => (i1 -> i2) -- ^ map initial input to new input
         -> (i2 -> Maybe i1) -- ^ map new leftovers to initial leftovers
         -> Pipe i2 o d r m r
         -> Pipe i1 o d t m r
mapInput f g = pipe (mapLeftoverPipe f g)

mapLeftoverPipe :: (Show a, Show b) => Monad m => (a -> b) -> (b -> Maybe a) -> Pipe a b r t m r
mapLeftoverPipe f g =
    go
  where
    go = await >>= maybe haltPipe (\x -> tryYield (f x) >>= maybe go done)

    done (bs, result) = traceShow bs $ Pure (mapMaybe g bs) result

-- | Convert a list into a source.
--
-- Since 0.3.0
sourceList :: Monad m => [a] -> Pipe i a d t m ()
sourceList [] = return ()
sourceList (a:as) = tryYield a >>= maybe (sourceList as) (const $ return ())

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
(>+>) :: Monad m
      => Pipe i j b t m a
      -> Pipe j k c b m b
      -> Pipe i k c t m a
(>+>) = pipe
{-# INLINE (>+>) #-}

-- | Same as '>+>', but reverse the order of the arguments.
--
-- Since 0.5.0
(<+<) :: Monad m
      => Pipe j k c b m b
      -> Pipe i j b t m a
      -> Pipe i k c t m a
(<+<) = flip pipe
{-# INLINE (<+<) #-}

-- | Perform some allocation and run an inner @ConduitM@. Two guarantees are given
-- about resource finalization:
--
-- 1. It will be /prompt/. The finalization will be run as early as possible.
--
-- 2. It is exception safe. Due to usage of @resourcet@, the finalization will
--    be run in the event of any exceptions.
--
-- Since 0.5.0
bracketP :: MonadResource m -- FIXME use check to make sure that downstream is already done
         => IO a
         -> (a -> IO ())
         -> (a -> Pipe i o d t m r)
         -> Pipe i o d t m r
bracketP alloc free inside = do
    (key, seed) <- allocate alloc free
    addCleanup (const $ release key) (inside seed)

-- | Add some code to be run when the given @ConduitM@ cleans up.
--
-- Since 0.4.1
addCleanup :: Monad m
           => (Bool -> m ()) -- ^ @True@ if @ConduitM@ ran to completion, @False@ for early termination.
           -> Pipe i o d t m r
           -> Pipe i o d t m r
addCleanup f (Pure is r) = M (f True >> return (Pure is r))
addCleanup f (M m) = M (liftM (addCleanup f) m)
addCleanup f (Yield more done o) = Yield (addCleanup f more) (addCleanup f .: done) o
addCleanup f (Empty done) = Empty (addCleanup f .: done)
addCleanup f (Await more done) = Await (addCleanup f . more) (addCleanup f done)
addCleanup f (Check more done) = Check (addCleanup f more) (addCleanup f .: done)
addCleanup f (Terminate is t) = M (f False >> return (Terminate is t))

-- | Connect a @Source@ to a @Sink@ until the latter closes. Returns both the
-- most recent state of the @Source@ and the result of the @Sink@.
--
-- We use a @ResumableSource@ to keep track of the most recent finalizer
-- provided by the @Source@.
--
-- Since 0.5.0
connectResume :: Monad m
              => Pipe () o () () m ()
              -> Pipe o Void () () m r
              -> m (Pipe () o () () m (), r)
connectResume up =
    go
  where
    go (Pure [] r) = {-# SCC "Pure[]" #-} return (up, r)
    go (Pure is r) = {-# SCC "Pure_is" #-} return (mapM_ tryYield is >> up, r)
    go (M m) = {-# SCC "M" #-} m >>= go
    go (Yield _ _ o) = {-# SCC "Yield" #-} absurd o
    go (Empty done) = {-# SCC "Empty" #-} go $ done [] ()
    go (Await more done) = {-# SCC "Await" #-}
        draw
            (\up' o -> connectResume up' (more o))
            (connectResume (return ()) done)
            up
{-
connectResume src (Pipe f) =
    go $ f $ Just down
  where
    down = Endpoint [] ()

    go (Pure (PipeTerm (Endpoint _ ()))) = error "Data.Conduit.Internal.connectResume: early termination from sink"
    go (Pure (PipeCont (Endpoint os r) _)) = do
        let src' = mapM_ yield os >> src
        return (src', r)
    go (M m) = m >>= go
    go (Yield _ _ x) = absurd x
    go (Empty f) = go $ f down
    go (Await more none) = do
        mx <- draw src
        case mx of
            Nothing -> connectResume (return ()) (Pipe $ const none)
            Just (src', o) -> connectResume src' (Pipe $ const $ more o)
-}

draw :: Monad m
     => (Pipe () o () t m () -> o -> m a)
     -> m a
     -> Pipe () o () t m ()
     -> m a
draw provide done =
    go
  where
    go Pure{} = done
    go (M m) = m >>= go
    go (Yield more _ o) = provide more o -- FIXME
    go (Empty done) = go (done [] ())
    go (Check more _) = go more
    go Terminate{} = done
{-
draw (Pipe f) =
    go (f Nothing)
  where
    go (Pure _) = return Nothing
    go (M m) = m >>= go
    go (Await _ none) = go none
    go (Yield next done o) = return $ Just (Pipe $ maybe next done, o)
    go (Empty next) = do
        _ <- runPipe $ Pipe (const $ next $ Endpoint [] ()) `pipe` return ()
        return Nothing
-}

{-

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
-}

-- Using Void as the output witnesses that this is never called on a Pipe that
-- uses yield or any other terminating functions.
disallowTerm :: Monad m => Pipe i Void d t' m r -> Pipe i Void d t m r
disallowTerm = unsafeCoerce
{-
disallowTerm (Pure is r) = Pure is r
disallowTerm (M m) = M (liftM disallowTerm m)
disallowTerm (Yield more done o) = Yield (disallowTerm more) (disallowTerm .: done) o
disallowTerm (Empty done) = Empty (disallowTerm .: done)
disallowTerm (Await more done) = Await (disallowTerm . more) (disallowTerm done)
disallowTerm (Check more done) = Check (disallowTerm more) (disallowTerm .: done)
disallowTerm Terminate{} = error "Data.Conduit.Internal.disallowTerm: Invariant violated: disallowTerm called when terminator used"
-}

-- | Ensure that downstream is still active.
checkDownstream :: Monad m => Pipe i o d d m ()
checkDownstream = Check (Pure [] ()) (const $ Terminate [])

-- | Notify downstream that we're all done generating output.
closeDownstream :: Monad m => Pipe i o d t m ([o], d)
closeDownstream = Empty $ curry $ Pure []
