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
    , Step (..)
    , Endpoint (..)
    , PipeRes (..)
      -- * Primitives
    , await
    , awaitForever
    , yield
    , tryYield
    , yieldOr
    , leftover
      -- * Finalization
    , bracketP
    , addCleanup
      -- * Composition
    , idP
    , pipe
    , connectResume
    , runPipe
    , (>+>)
    , (<+<)
    , haltPipe
    , absurdTerm
      -- * Utilities
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , sourceList
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

data Step i o d m r
    = Pure r
    | M (m (Step i o d m r))
    | Yield (Maybe d -> Step i o d m r) (Maybe o)
    | Await (Maybe i -> Step i o d m r)

instance Monad m => Functor (Step i o d m) where
    fmap = liftM

instance Monad m => Applicative (Step i o d m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Step i o d m) where
    return = Pure

    Pure r >>= f = f r
    M m >>= f = M (liftM (>>= f) m)
    Yield next o >>= f = Yield (next >=> f) o
    Await next >>= f = Await (next >=> f)

instance MonadBase base m => MonadBase base (Step i o d m) where
    liftBase = lift . liftBase

instance MonadTrans (Step i o d) where
    lift = M . liftM Pure

instance MonadIO m => MonadIO (Step i o d m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (Step i o d m) where
    monadThrow = lift . monadThrow

instance MonadActive m => MonadActive (Step i o d m) where
    monadActive = lift monadActive

instance Monad m => Monoid (Step i o d m ()) where
    mempty = return ()
    mappend = (>>)

instance MonadResource m => MonadResource (Step i o d m) where
    liftResourceT = lift . liftResourceT

instance MonadReader r m => MonadReader r (Step i o d m) where
    ask = lift ask
    local f (Yield p o) = Yield (local f . p) o
    local f (Await p) = Await (local f . p)
    local _ (Pure x) = Pure x
    local f (M mp) = M (local f mp)

-- Provided for doctest
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x, y, z) 0
#endif

instance MonadWriter w m => MonadWriter w (Step i o d m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif

    tell = lift . tell

    listen (Yield p o) = Yield (listen . p) o
    listen (Await p) = Await (listen . p)
    listen (Pure x) = Pure (x,mempty)
    listen (M mp) =
      M $
      do (p,w) <- listen mp
         return $ do (x,w') <- listen p
                     return (x, w `mappend` w')

    pass (Yield p o) = Yield (pass . p) o
    pass (Await p) = Await (pass . p)
    pass (M mp) = M $ mp >>= (return . pass)
    pass (Pure (x,w)) = M $ pass $ return (Pure x, w)

instance MonadState s m => MonadState s (Step i o d m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (Step i o d m)

instance MonadError e m => MonadError e (Step i o d m) where
    throwError = lift . throwError
    catchError (Yield p o) f = Yield (\x -> catchError (p x) f) o
    catchError (Await p) f = Await (\i -> catchError (p i) f)
    catchError (Pure x) _ = Pure x
    catchError (M mp) f =
      M $ catchError (liftM (flip catchError f) mp) (\e -> return (f e))

instance MFunctor (Step i o d) where
    hoist f =
        go
      where
        go (Yield p o) = Yield (go . p) o
        go (Await p) = Await (go . p)
        go (Pure r) = Pure r
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

fuseStep :: Monad m
         => (Maybe b -> Step i j b m a)
         -> Step j k c m b
         -> Step i k c m a
fuseStep up0 (Pure b) =
    go (up0 (Just b))
  where
    go (Pure a) = Pure a
    go (M up) = M (liftM go up)
    go (Yield up _) = go (up (Just b))
    go (Await up) = Await (go . up)
fuseStep up (M down) = M (liftM (fuseStep up) down)
fuseStep up (Yield down k) = Yield (fuseStep up . down) k
fuseStep up0 (Await down) =
    go (up0 Nothing)
  where
    go (Pure a) = fuseStep (\_ -> Pure a) (down Nothing)
    go (M up) = M (liftM go up)
    go (Yield up j) = fuseStep up (down j)
    go (Await up) = Await (go . up)

idStep :: Maybe r -> Step i i r m r
idStep (Just r) = Pure r
idStep Nothing = Await (Yield idStep)

data Endpoint stream result = Endpoint
    { epLeftovers :: [stream]
    , epResult :: result
    }

data PipeRes i o d t r
    = PipeTerm (Endpoint i t)
    | PipeCont (Endpoint i r) (Maybe (Endpoint o d))

newtype Pipe i o d t m r = Pipe
    { unPipe :: Maybe (Endpoint o d)
             -> Step i o (Endpoint o d) m (PipeRes i o d t r)
    }

instance Monad m => Functor (Pipe i o d t m) where
    fmap = liftM

instance Monad m => Applicative (Pipe i o d t m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Pipe i o d t m) where
    return a = Pipe (return . PipeCont (Endpoint [] a))

    Pipe f >>= g = Pipe $ \down0 -> do
        res <- f down0
        case res of
            PipeTerm up -> return (PipeTerm up)
            PipeCont (Endpoint leftovers result) down -> do
                inject leftovers (unPipe (g result) down)
      where
        inject :: Monad m
               => [i]
               -> Step i o (Endpoint o d) m (PipeRes i o d t r)
               -> Step i o (Endpoint o d) m (PipeRes i o d t r)
        inject [] s = s
        inject is (Pure (PipeTerm (Endpoint is' t))) = Pure (PipeTerm (Endpoint (is' ++ is) t))
        inject is (Pure (PipeCont (Endpoint is' r) down)) = Pure (PipeCont (Endpoint (is' ++ is) r) down)
        inject is (M m) = M (liftM (inject is) m)
        inject is (Yield next o) = Yield (inject is . next) o
        inject (i:is) (Await next) = inject is (next (Just i))

instance MonadBase base m => MonadBase base (Pipe i o d t m) where
    liftBase = lift . liftBase

instance MonadTrans (Pipe i o d t) where
    lift m = Pipe $ \done -> do
        x <- lift m
        return $ PipeCont (Endpoint [] x) done

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
    local f (Pipe g) = Pipe (local f . g)

-- Provided for doctest
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x, y, z) 0
#endif

instance MonadWriter w m => MonadWriter w (Pipe i o d t m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif

    tell = lift . tell

    listen (Pipe p) = Pipe $ \x -> do
        (res, w) <- listen $ p x
        return $ case res of
            PipeTerm (Endpoint x y) -> PipeTerm (Endpoint x y)
            PipeCont (Endpoint x y) done -> PipeCont (Endpoint x (y, w)) done

    pass (Pipe p) = Pipe $ \x -> do
        res <- p x
        case res of
            PipeTerm x -> return $ PipeTerm x
            PipeCont (Endpoint ls (r, w)) done -> pass $ return (PipeCont (Endpoint ls r) done, w)

instance MonadState s m => MonadState s (Pipe i o d t m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (Pipe i o d t m)

instance MonadError e m => MonadError e (Pipe i o d t m) where
    throwError = lift . throwError
    catchError (Pipe p) f = Pipe (\x -> catchError (p x) (\y -> unPipe (f y) x))

instance MFunctor (Pipe i o d t) where
    hoist f (Pipe p) = Pipe (hoist f . p)

haltPipe :: Monad m => Pipe i o d t m d
haltPipe =
    Pipe go
  where
    go down@(Just (Endpoint _ d)) = Pure (PipeCont (Endpoint [] d) down)
    go Nothing = Yield go Nothing

-- | The identity @ConduitM@.
--
-- Since 0.5.0
idP :: Monad m => Pipe i i r t m r
idP =
    Pipe go
  where
    go down@(Just endpoint) = Pure (PipeCont endpoint down)
    go Nothing = Await (Yield go)

-- | Compose a left and right pipe together into a complete pipe. The left pipe
-- will be automatically closed when the right pipe finishes.
--
-- Since 0.5.0
pipe :: Monad m
     => Pipe i j b a m a
     -> Pipe j k c b m b
     -> Pipe i k c t m a
pipe (Pipe up) (Pipe down) =
    Pipe $ liftM dropDownstream . fuseStep up . liftM collapseRes . down
  where
    collapseRes :: PipeRes i o d r r -> Endpoint i r
    collapseRes (PipeTerm endpoint) = endpoint
    collapseRes (PipeCont endpoint _) = endpoint

    -- We no longer need the second field in the PipeCont constructor, and its presence
    -- causes typechecking to fail since the downstream endpoint is no longer valid.
    -- So we just drop it.
    dropDownstream :: PipeRes i o1 d1 r r -> PipeRes i o2 d2 t r
    dropDownstream (PipeTerm endpoint) = PipeCont endpoint Nothing
    dropDownstream (PipeCont endpoint _) = PipeCont endpoint Nothing

-- | Send a single output value downstream. If the downstream @ConduitM@
-- terminates, this @ConduitM@ will terminate as well.
--
-- Since 0.5.0
yield :: Monad m => o -> Pipe i o d d m ()
yield o =
    Pipe go
  where
    go (Just (Endpoint _leftovers result)) = Pure (PipeTerm (Endpoint [] result))
    go Nothing = Yield go' (Just o)

    go' (Just (Endpoint _leftovers result)) = Pure (PipeTerm (Endpoint [] result))
    go' Nothing = Pure (PipeCont (Endpoint [] ()) Nothing)

tryYield :: Monad m => o -> Pipe i o d t m (Maybe (Endpoint o d))
tryYield o =
    Pipe go
  where
    go x@(Just down) = Pure (PipeCont (Endpoint [] (Just down)) x)
    go Nothing = Yield go' (Just o)

    go' x@(Just down) = Pure (PipeCont (Endpoint [] (Just down)) x)
    go' Nothing = Pure (PipeCont (Endpoint [] Nothing) Nothing)

-- | Similar to @yield@, but additionally takes a finalizer to be run if the
-- downstream @ConduitM@ terminates.
--
-- Since 0.5.0
yieldOr :: Monad m => o -> m () -> Pipe i o d d m ()
yieldOr o f =
    Pipe go
  where
    go (Just (Endpoint _leftovers result)) = lift f >> Pure (PipeTerm (Endpoint [] result))
    go Nothing = Yield go' (Just o)

    go' (Just (Endpoint _leftovers result)) = lift f >> Pure (PipeTerm (Endpoint [] result))
    go' Nothing = Pure (PipeCont (Endpoint [] ()) Nothing)

-- | Wait for a single input value from upstream.
--
-- Since 0.5.0
await :: Monad m => Pipe i o d t m (Maybe i)
await =
    Pipe go
  where
    go down = Await $ \mi -> Pure (PipeCont (Endpoint [] mi) down)

-- | Wait for input forever, calling the given inner @ConduitM@ for each piece of
-- new input. Returns the upstream result type.
--
-- Since 0.5.0
awaitForever :: Monad m => (i -> Pipe i o d t m r') -> Pipe i o d t m ()
awaitForever inner =
    loop
  where
    loop = await >>= maybe (return ()) (\i -> inner i >> loop)

-- | Provide a single piece of leftover input to be consumed by the next pipe
-- in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftover :: Monad m => i -> Pipe i o d t m ()
leftover i =
    Pipe go
  where
    go down = Pure (PipeCont (Endpoint [i] ()) down)

-- | Run a pipeline until processing completes.
--
-- Since 0.5.0
runPipe :: Monad m
        => Pipe i o () r m r
        -> m r
runPipe =
    go . ($ down) . unPipe
  where
    down = Just $ Endpoint [] ()
    go (Pure (PipeCont (Endpoint _ r) _)) = return r
    go (Pure (PipeTerm (Endpoint _ r))) = return r
    go (M m) = m >>= go
    go (Yield next _) = go (next down)
    go (Await next) = go (next Nothing)

-- | Apply a function to all the output values of a @ConduitM@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days.
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> Pipe i o1 d r m r -> Pipe i o2 d r m r
mapOutput f = (`pipe` mapPipe f)

mapPipe :: Monad m => (a -> b) -> Pipe a b r t m r
mapPipe f =
    go
  where
    go = await >>= maybe haltPipe (\x -> tryYield (f x) >>= maybe go (return . epResult))

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> Pipe i o1 d r m r -> Pipe i o2 d r m r
mapOutputMaybe f = (`pipe` mapMaybePipe f)

mapMaybePipe :: Monad m => (a -> Maybe b) -> Pipe a b r t m r
mapMaybePipe f =
    go
  where
    go = await >>= maybe haltPipe (maybe go (\x -> tryYield x >>= maybe go (return . epResult)) . f)

-- | Apply a function to all the input values of a @ConduitM@.
--
-- Since 0.5.0
mapInput :: Monad m
         => (i1 -> i2) -- ^ map initial input to new input
         -> (i2 -> Maybe i1) -- ^ map new leftovers to initial leftovers
         -> Pipe i2 o d r m r
         -> Pipe i1 o d r m r
mapInput f g = pipe (mapLeftoverPipe f g)

mapLeftoverPipe :: Monad m => (a -> b) -> (b -> Maybe a) -> Pipe a b r t m r
mapLeftoverPipe f g =
    go
  where
    go = await >>= maybe haltPipe (\x -> tryYield (f x) >>= maybe go done)

    done (Endpoint bs result) = do
        mapM_ leftover $ mapMaybe g bs
        return result

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
      => Pipe i j b a m a
      -> Pipe j k c b m b
      -> Pipe i k c a m a
(>+>) = pipe
{-# INLINE (>+>) #-}

-- | Same as '>+>', but reverse the order of the arguments.
--
-- Since 0.5.0
(<+<) :: Monad m
      => Pipe j k c b m b
      -> Pipe i j b a m a
      -> Pipe i k c a m a
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
bracketP :: MonadResource m
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
addCleanup f (Pipe p) = Pipe $ \down -> do
    res <- p down
    case res of
        PipeTerm _ -> lift $ f False
        PipeCont _ _ -> lift $ f True
    return res
    {-
    setFinalizer (cleanup False) >> go c0
  where
    go (Done ls r) = clearCleanup >> lift (cleanup True) >> Done ls r
    go (HaveOutput src x) = HaveOutput (go src) x
    go (ConduitM msrc) = ConduitM (liftM go msrc)
    go (NeedInput p c) = NeedInput (go . p) (go c)
    go (Cleanup p c) = Cleanup (go p) (\ls -> c ls >> lift (cleanup False))
    -}

-- | Connect a @Source@ to a @Sink@ until the latter closes. Returns both the
-- most recent state of the @Source@ and the result of the @Sink@.
--
-- We use a @ResumableSource@ to keep track of the most recent finalizer
-- provided by the @Source@.
--
-- Since 0.5.0
connectResume :: Monad m
              => (forall d. Pipe () o d d m ())
              -> Pipe o Void () r m r
              -> m (forall d. Pipe () o d d m (), r)
connectResume left right = do
    error "connectResume"
    {-
    (cleanup, left') <- getCleanup left
    goRight cleanup left' right
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

absurdTerm :: Monad m
           => Pipe i o d Void m r
           -> Pipe i o d t m r
absurdTerm (Pipe p) = Pipe $ \done -> do
    x <- p done
    return $ case x of
        PipeTerm (Endpoint _ t) -> absurd t
        PipeCont y z -> PipeCont y z
