{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -O2 #-} -- necessary to avoid some space leaks
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Data.Conduit.Internal
    ( -- * Types
      Pipe (..)
    , Source
    , Sink
    , Conduit
    , Finalize (..)
      -- * Terminating pipes
    , TPipe
    , toPipe
    , await
    , yield
    , leftoverTP
    , bracketP
    , bracketTP
      -- * Functions
    , pipeClose
    , pipe
    , pipePush
    , pipePushStrip
    , pipeResume
    , runPipe
    , sinkToPipe
    , hasInput
    , transPipe
    , mapOutput
    , runFinalize
    , addCleanup
    , noInput
    , sourceList
    , tryAwait
    , leftover
    , tryYield
    ) where

import Control.Applicative (Applicative (..), (<$>))
import Control.Monad ((>=>), liftM, ap)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Base (MonadBase (liftBase))
import Data.Void (Void, absurd)
import Data.Monoid (Monoid (mappend, mempty))
import Control.Monad.Trans.Resource

-- | A cleanup action to be performed.
--
-- Previously, we just had a plain action. However, most @Pipe@s simply have
-- empty cleanup actions, and storing a large set of them wastes memory. But
-- having strict fields and distinguishing between pure and impure actions, we
-- can keep memory usage constant, and only allocate memory for the actual
-- actions we have to track.
--
-- Since 0.4.1
data Finalize m r = FinalizePure r
                  | FinalizeM (m r)

instance Monad m => Functor (Finalize m) where
    fmap f (FinalizePure r) = FinalizePure (f r)
    fmap f (FinalizeM mr) = FinalizeM (liftM f mr)

instance Monad m => Applicative (Finalize m) where
    pure = FinalizePure
    (<*>) = ap

instance Monad m => Monad (Finalize m) where
    return = FinalizePure
    FinalizePure x >>= f = f x
    FinalizeM mx >>= f = FinalizeM $ mx >>= runFinalize . f

    FinalizePure _ >> f = f
    FinalizeM x >> FinalizeM y = FinalizeM (x >> y)
    FinalizeM x >> FinalizePure y = FinalizeM (x >> return y)

instance MonadTrans Finalize where
    lift = FinalizeM

instance MonadThrow m => MonadThrow (Finalize m) where
    monadThrow = lift . monadThrow

instance MonadIO m => MonadIO (Finalize m) where
    liftIO = lift . liftIO

instance MonadResource m => MonadResource (Finalize m) where
    allocate a = lift . allocate a
    register = lift . register
    release = lift . release
    resourceMask = lift . resourceMask

-- | The underlying datatype for all the types in this package.  In has four
-- type parameters:
--
-- * /i/ is the type of values for this @Pipe@'s input stream.
--
-- * /o/ is the type of values for this @Pipe@'s output stream.
--
-- * /m/ is the underlying monad.
--
-- * /r/ is the result type.
--
-- Note that /o/ and /r/ are inherently different. /o/ is the type of the
-- stream of values this @Pipe@ will produce and send downstream. /r/ is the
-- final output of this @Pipe@.
--
-- Since 0.5.0
data Pipe i o m r =
    -- | Provide new output to be sent downstream. This constructor has three
    -- fields: the next @Pipe@ to be used, an early-closed function, and the
    -- output value.
    HaveOutput (Pipe i o m r) (Finalize m r) o
    -- | Request more input from upstream. The first field takes a new input
    -- value and provides a new @Pipe@. The second is for early termination. It
    -- gives a new @Pipe@ which takes no input from upstream. This allows a
    -- @Pipe@ to provide a final stream of output values after no more input is
    -- available from upstream.
  | NeedInput (i -> Pipe i o m r) (Pipe Void o m r)
    -- | Processing with this @Pipe@ is complete, providing the final result.
  | Done r
    -- | Require running of a monadic action to get the next @Pipe@. Second
    -- field is an early cleanup function. Technically, this second field
    -- could be skipped, but doing so would require extra operations to be
    -- performed in some cases. For example, for a @Pipe@ pulling data from a
    -- file, it may be forced to pull an extra, unneeded chunk before closing
    -- the @Handle@.
  | PipeM (m (Pipe i o m r)) (Finalize m r)
    -- | Return leftover input, which should be provided to future operations.
  | Leftover (Pipe i o m r) i

-- | A @Pipe@ which provides a stream of output values, without consuming any
-- input. The input parameter is set to @Void@ to indicate that this @Pipe@
-- takes no input.  A @Source@ is not used to produce a final result, and thus
-- the result parameter is set to @()@.
--
-- Since 0.4.0
type Source m a = Pipe Void a m ()

-- | A @Pipe@ which consumes a stream of input values and produces a final
-- result. It cannot produce any output values, and thus the output parameter
-- is set to @Void@.
--
-- Since 0.4.0
type Sink i m r = Pipe i Void m r

-- | A @Pipe@ which consumes a stream of input values and produces a stream of
-- output values. It does not produce a result value, and thus the result
-- parameter is set to @()@.
--
-- Since 0.4.0
type Conduit i m o = Pipe i o m ()

-- | Perform any close actions available for the given @Pipe@.
--
-- Since 0.4.0
pipeClose :: Monad m => Pipe i o m r -> Finalize m r
pipeClose (HaveOutput _ c _) = c
pipeClose (NeedInput _ p) = pipeClose p
pipeClose (Done r) = FinalizePure r
pipeClose (PipeM _ c) = c
pipeClose (Leftover p _) = pipeClose p

pipePush :: Monad m => i -> Pipe i o m r -> Pipe i o m r
pipePush i (HaveOutput p c o) = HaveOutput (pipePush i p) c o
pipePush i (NeedInput p _) = p i
pipePush i (Done r) = Leftover (Done r) i
pipePush i (PipeM mp c) = PipeM (pipePush i `liftM` mp) c
pipePush i p@Leftover{} = Leftover p i

pipePushStrip :: Monad m => i -> Pipe i o m r -> Pipe i o m r
pipePushStrip i p =
    case pipePush i p of
        Leftover p' _ -> p'
        p' -> p'

instance Monad m => Functor (Pipe i o m) where
    fmap f (HaveOutput p c o) = HaveOutput (f <$> p) (f `liftM` c) o
    fmap f (NeedInput p c) = NeedInput (fmap f . p) (f <$> c)
    fmap f (Done r) = Done (f r)
    fmap f (PipeM mp mr) = PipeM ((fmap f) `liftM` mp) (f `liftM` mr)
    fmap f (Leftover p i) = Leftover (f <$> p) i

instance Monad m => Applicative (Pipe i o m) where
    pure = Done

    Done f <*> px = f <$> px
    HaveOutput p c o <*> px = HaveOutput (p <*> px) (c `ap` pipeClose px) o
    NeedInput p c <*> px = NeedInput (\i -> p i <*> px) (c <*> noInput px)
    PipeM mp c <*> px = PipeM ((<*> px) `liftM` mp) (c `ap` pipeClose px)
    Leftover p i <*> px = Leftover (p <*> px) i

instance Monad m => Monad (Pipe i o m) where
    return = Done

    Done x >>= fp = fp x
    HaveOutput p c o >>= fp = HaveOutput (p >>= fp) (c >>= pipeClose . fp) o
    NeedInput p c >>= fp =
        NeedInput (p >=> fp) (c >>= noInput . fp)
        {-
         - Possible reimplementation of noInput which does not apply noInput to
         - the second field of NeedInput
      where
        go (Done x) = Done x
        go (HaveOutput p c o) = HaveOutput (go p) c o
        go (NeedInput _ c) = c
        go (PipeM mp c) = PipeM (liftM go mp) c
        go (Leftover p _) = go p
        -}
    PipeM mp c >>= fp = PipeM ((>>= fp) `liftM` mp) (c >>= pipeClose . fp)
    Leftover p i >>= fp = Leftover (p >>= fp) i

instance MonadBase base m => MonadBase base (Pipe i o m) where
    liftBase = lift . liftBase

instance MonadTrans (Pipe i o) where
    lift mr = PipeM (Done `liftM` mr) (FinalizeM mr)

instance MonadIO m => MonadIO (Pipe i o m) where
    liftIO = lift . liftIO

instance Monad m => Monoid (Pipe i o m ()) where
    mempty = return ()
    mappend = (>>)

-- | Compose a left and right pipe together into a complete pipe. The left pipe
-- will be automatically closed when the right pipe finishes, and any leftovers
-- from the right pipe will be discarded.
--
-- This is in fact a wrapper around 'pipeResume'. This function closes the left
-- @Pipe@ returns by @pipeResume@ and returns only the result.
--
-- Since 0.4.0
pipe :: Monad m => Pipe a b m () -> Pipe b c m r -> Pipe a c m r
pipe l r = do
    (l', res) <- pipeResume l r
    lift $ runFinalize $ pipeClose l'
    return res

-- | Same as 'pipe', but retain both the new left pipe and the leftovers from
-- the right pipe. The two components are combined together into a single pipe
-- and returned, together with the result of the right pipe.
--
-- Note: we're biased towards checking the right side first to avoid pulling
-- extra data which is not needed. Doing so could cause data loss.
--
-- Since 0.4.0
pipeResume :: Monad m => Pipe a b m () -> Pipe b c m r -> Pipe a c m (Pipe a b m (), r)
pipeResume left right =
    -- We're using a case statement instead of pattern matching in the function
    -- itself to make the logic explicit. We first check the right pipe, and
    -- only if the right pipe is asking for more input do we process the left
    -- pipe.
    case right of
        -- Right pipe is done, grab result and the left pipe
        Done r -> Done (left, r)

        -- Right pipe needs to run a monadic action.
        PipeM mp c -> PipeM (pipeResume left `liftM` mp) ((left,) <$> c)

        -- Right pipe has some output, provide it downstream and continue.
        HaveOutput p c o -> HaveOutput (pipeResume left p) ((left, ) <$> c) o

        Leftover p i -> pipeResume (HaveOutput left (pipeClose left) i) p

        -- Right pipe needs input, so let's get it
        NeedInput rp rc ->
            case left of
                Leftover lp i -> (\(p, r) -> (Leftover p i, r)) <$> pipeResume lp (NeedInput rp rc)
                -- Left pipe has output, right pipe wants it.
                HaveOutput lp _ a -> pipeResume lp $ rp a

                -- Left pipe needs more input, ask for it.
                NeedInput p c -> NeedInput
                    (\a -> pipeResume (p a) right)
                    (do
                        -- There is no more input available, so connect the
                        -- no-more-input field with the right.
                        (left', res) <- pipeResume c right

                        -- left' can no longer accept input, so close it
                        lift $ runFinalize $ pipeClose left'

                        -- left is closed, return the result
                        return (Done (), res)
                        )

                -- Left pipe is done, right pipe needs input. In such a case,
                -- tell the right pipe there is no more input.
                Done () -> (Done (),) `liftM` noInput rc

                -- Left pipe needs to run a monadic action.
                PipeM mp c -> PipeM
                    ((`pipeResume` right) `liftM` mp)
                    ((Done (),) <$> (c >> pipeClose right))

-- | Run a @Pipe@ without providing it any input. Since the resulting @Pipe@ no
-- longer depends on any input, the @i@ parameter may have any type.
noInput :: Monad m => Pipe i' o m r -> Pipe i o m r
noInput (Done r) = Done r
noInput (HaveOutput p c o) = HaveOutput (noInput p) c o

-- This function is only called on pipes when there is no more input available.
-- Therefore, we can ignore the push record.
noInput (NeedInput _ c) = noInput c

noInput (PipeM mp c) = PipeM (noInput `liftM` mp) c

-- In theory, this is better, but I believe it violates the Monad laws.
-- noInput (Leftover p i) = noInput $ pipePushStrip i p
noInput (Leftover p _) = noInput p

-- | Run a complete pipeline until processing completes.
--
-- Since 0.4.0
runPipe :: Monad m => Pipe Void Void m r -> m r
runPipe (HaveOutput _ c _) = runFinalize c
runPipe (NeedInput _ c) = runPipe c
runPipe (Done r) = return r
runPipe (PipeM mp _) = mp >>= runPipe
runPipe (Leftover p _) = runPipe p

-- | Perform any necessary finalization actions.
--
-- Since 0.4.1
runFinalize :: Monad m => Finalize m r -> m r
runFinalize (FinalizePure r) = return r
runFinalize (FinalizeM mr) = mr

-- | Send a single output value downstream.
--
-- /Note to self/: Make this explanation more user friendly.
--
-- Due to the nature of @TPipe@, the @yield@ function will automatically
-- terminate when downstream no longer wants input. This is because monadic
-- bind for @TPipe@ handles automatic termination.
--
-- By contrast, the monadic bind for @Pipe@ does no such automatic termination,
-- as that would (for one thing) prevent resource finalization. As a result, if
-- we had an equivalent @yield@ function for @Pipe@, the following code would
-- be an infinite loop:
--
-- > mapM_ yield [1..] $$ take 10
--
-- The solution is that @yield'@ takes two arguments: the value to send
-- downstream, and the next @Pipe@ to be run, /if downstream if still accepting
-- input/. If downstream is not accepting input, then the second argument to
-- this function will not be called.
--
-- Note that you can still cause infinite loops via monadic bind, but the type
-- signature of this function should make it significantly less likely. Basic
-- approach: @yield'@ should be the last call in any chain of monadic binding.
--
-- Since 0.5.0
tryYield :: o -- ^ output value
         -> Pipe i o m ()
         -- ^ next @Pipe@ to be run, if downstream is still accepting input
         -> Pipe i o m ()
tryYield o p = HaveOutput p (FinalizePure ()) o

-- | Wait for a single input value from upstream, and remove it from the
-- stream. Returns @Nothing@ if no more data is available.
--
-- Since 0.5.0
tryAwait :: Pipe i o m (Maybe i)
tryAwait = NeedInput (Done . Just) (Done Nothing)

-- | Provide a single piece of leftover input to be consumed by the next pipe
-- in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- Since 0.5.0
leftover :: i -> Pipe i o m ()
leftover = Leftover (Done ())

-- | Check if input is available from upstream. Will not remove the data from
-- the stream.
--
-- Since 0.4.0
hasInput :: Pipe i o m Bool
hasInput = NeedInput (Leftover (Done True)) (Done False)

-- | A @Sink@ has a @Void@ type parameter for the output, which makes it
-- difficult to compose with @Source@s and @Conduit@s. This function replaces
-- that parameter with a free variable. This function is essentially @id@; it
-- only modifies the types, not the actions performed.
--
-- Since 0.4.0
sinkToPipe :: Monad m => Sink i m r -> Pipe i o m r
sinkToPipe (HaveOutput _ _ o) = absurd o
sinkToPipe (NeedInput p c) = NeedInput (sinkToPipe . p) (sinkToPipe c)
sinkToPipe (Done r) = Done r
sinkToPipe (PipeM mp c) = PipeM (liftM sinkToPipe mp) c
sinkToPipe (Leftover p i) = Leftover (sinkToPipe p) i

-- | Transform the monad that a @Pipe@ lives in.
--
-- Since 0.4.0
transPipe :: Monad m => (forall a. m a -> n a) -> Pipe i o m r -> Pipe i o n r
transPipe f (HaveOutput p c o) = HaveOutput (transPipe f p) (transFinalize f c) o
transPipe f (NeedInput p c) = NeedInput (transPipe f . p) (transPipe f c)
transPipe _ (Done r) = Done r
transPipe f (PipeM mp c) = PipeM (f $ liftM (transPipe f) mp) (transFinalize f c)
transPipe f (Leftover p i) = Leftover (transPipe f p) i

transFinalize :: (forall a. m a -> n a) -> Finalize m r -> Finalize n r
transFinalize _ (FinalizePure r) = FinalizePure r
transFinalize f (FinalizeM mr) = FinalizeM $ f mr

-- | Apply a function to all the output values of a `Pipe`.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days.
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> Pipe i o1 m r -> Pipe i o2 m r
mapOutput f (HaveOutput p c o) = HaveOutput (mapOutput f p) c (f o)
mapOutput f (NeedInput p c) = NeedInput (mapOutput f . p) (mapOutput f c)
mapOutput _ (Done r) = Done r
mapOutput f (PipeM mp c) = PipeM (liftM (mapOutput f) mp) c
mapOutput f (Leftover p i) = Leftover (mapOutput f p) i

-- | Add some code to be run when the given @Pipe@ cleans up.
--
-- Since 0.4.1
addCleanup :: Monad m
           => (Bool -> m ()) -- ^ @True@ if @Pipe@ ran to completion, @False@ for early termination.
           -> Pipe i o m r
           -> Pipe i o m r
addCleanup cleanup (Done r) = PipeM
    (cleanup True >> return (Done r))
    (lift (cleanup True) >> return r)
addCleanup cleanup (HaveOutput src close x) = HaveOutput
    (addCleanup cleanup src)
    (lift (cleanup False) >> close)
    x
addCleanup cleanup (PipeM msrc close) = PipeM
    (liftM (addCleanup cleanup) msrc)
    (lift (cleanup False) >> close)
addCleanup cleanup (NeedInput p c) = NeedInput
    (addCleanup cleanup . p)
    (addCleanup cleanup c)
addCleanup cleanup (Leftover p i) = Leftover (addCleanup cleanup p) i

-- | Convert a list into a source.
--
-- Since 0.3.0
sourceList :: Monad m => [a] -> Pipe i a m ()
sourceList [] = Done ()
sourceList (x:xs) = HaveOutput (sourceList xs) (return ()) x

-- | A terminating pipe. As opposed to normal @Pipe@s, @TPipe@s will stop
-- running as soon as either their upstream or downstream components terminate.
-- While this approach cannot model all use cases (e.g., folds cannot be
-- expressed), it can simplify certain other cases (e.g., infinite producers).
--
-- Since 0.5.0
data TPipe i o m r =
    THaveOutput (TPipe i o m r) o
  | TNeedInput (i -> TPipe i o m r)
  | TDone r
  | TPipeM (m (TPipe i o m r))
  | TLeftover (TPipe i o m r) i

-- | Converting a terminating pipe to a regular pipe.
--
-- Since 0.5.0
toPipe :: Monad m => TPipe i o m () -> Pipe i o m ()
toPipe = toPipeFinalize (FinalizePure ())

toPipeFinalize :: Monad m
               => Finalize m ()
               -> TPipe i o m ()
               -> Pipe i o m ()
toPipeFinalize final =
    go
  where
    go (THaveOutput p o) = HaveOutput (go p) final o
    go (TNeedInput p) = NeedInput (go . p) done
    go (TDone ()) = done
    go (TPipeM mp) = PipeM (liftM go mp) final
    go (TLeftover p i) = Leftover (go p) i

    done =
        case final of
            FinalizeM f -> PipeM (liftM Done f) final
            FinalizePure () -> Done ()

-- | Wait for a single input value from upstream, terminating immediately if no
-- data is available.
--
-- Since 0.5.0
await :: TPipe i o m i
await = TNeedInput TDone

-- | Yield a single value downstream, terminating immediately if downstream is
-- no longer accepting input.
--
-- Since 0.5.0
yield :: o -> TPipe i o m ()
yield = THaveOutput (TDone ())

-- | Same as 'leftover', but for terminating pipes.
--
-- Since 0.5.0
leftoverTP :: i -> TPipe i o m ()
leftoverTP = TLeftover (TDone ())

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
         -> (a -> Pipe i o m ())
         -> Pipe i o m ()
bracketP alloc free inside =
    PipeM start (FinalizePure ())
  where
    start = do
        (key, seed) <- allocate alloc free
        return $ addCleanup (const $ release key) (inside seed)

-- | The same as 'bracketP', but runs a terminating pipe instead of a regular
-- pipe. This is equivalent to:
--
-- > bracketTP alloc free inner = bracketP alloc free (toPipe . inner)
--
-- Since 0.5.0
bracketTP :: MonadResource m
          => IO a
          -> (a -> IO ())
          -> (a -> TPipe i o m ())
          -> Pipe i o m ()
bracketTP alloc free inside =
    PipeM start (FinalizePure ())
  where
    start = do
        (key, seed) <- allocate alloc free
        return $ toPipeFinalize (FinalizeM (release key)) (inside seed)

instance Monad m => Functor (TPipe i o m) where
    fmap = liftM
instance Monad m => Applicative (TPipe i o m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (TPipe i o m) where
    return = TDone
    THaveOutput p o >>= fp = THaveOutput (p >>= fp) o
    TNeedInput push >>= fp = TNeedInput (push >=> fp)
    TDone r >>= fp = fp r
    TPipeM mp >>= fp = TPipeM (liftM (>>= fp) mp)
    TLeftover p i >>= fp = TLeftover (p >>= fp) i
instance MonadBase base m => MonadBase base (TPipe i o m) where
    liftBase = lift . liftBase
instance MonadTrans (TPipe i o) where
    lift = TPipeM . liftM TDone
instance MonadIO m => MonadIO (TPipe i o m) where
    liftIO = lift . liftIO
instance Monad m => Monoid (TPipe i o m ()) where
    mempty = return ()
    mappend = (>>)
