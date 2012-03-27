{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Data.Conduit.Internal
    ( -- * Types
      Pipe (..)
    , Source
    , Sink
    , Conduit
      -- * Functions
    , pipeClose
    , pipe
    , pipeResume
    , runPipe
    , sinkToPipe
    , await
    , yield
    , hasInput
    ) where

import Control.Applicative (Applicative (..), (<|>), (<$>))
import Control.Monad ((>=>), liftM, ap)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Base (MonadBase (liftBase))
import Data.Void (Void)
import Data.Monoid (Monoid (mappend, mempty))

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
-- @Pipe@s can be composed via the 'pipe' function. To do so, the output type
-- of the left pipe much match the input type of the left pipe, and the result
-- type of the left pipe must be unit @()@. This is due to the fact that any
-- result produced by the left pipe must be discarded in favor of the result of
-- the right pipe.
--
-- Since 0.4.0
data Pipe i o m r =
    -- | Provide new output to be sent downstream. This constructor has three
    -- records: the next @Pipe@ to be used, an early-closed function, and the
    -- output value.
    HaveOutput (Pipe i o m r) (m r) o
    -- | Request more input from upstream. The first record takes a new input
    -- value and provides a new @Pipe@. The second is for early termination. It
    -- gives a new @Pipe@ which takes no input from upstream. This allows a
    -- @Pipe@ to provide a final stream of output values after no more input is
    -- available from upstream.
  | NeedInput (i -> Pipe i o m r) (Pipe Void o m r)
    -- | Processing with this @Pipe@ is complete. Provides an optional leftover
    -- input value and and result.
  | Done (Maybe i) r
    -- | Require running of a monadic action to get the next @Pipe@. Second
    -- record is an early cleanup function. Technically, this second record
    -- could be skipped, but doing so would require extra operations to be
    -- performed in some cases. For example, for a @Pipe@ pulling data from a
    -- file, it may be forced to pull an extra, unneeded chunk before closing
    -- the @Handle@.
  | PipeM (m (Pipe i o m r)) (m r)

-- | A @Pipe@ which provides a stream of output values, without consuming any
-- input. The input parameter is set to @()@ instead of @Void@ since there is
-- no way to statically guarantee that the @NeedInput@ constructor will not be
-- used. A @Source@ is not used to produce a final result, and thus the result
-- parameter is set to @()@ as well.
--
-- Since 0.4.0
type Source m a = Pipe Void a m ()

-- | A @Pipe@ which consumes a stream of input values and produces a final
-- result. It cannot produce any output values, and thus the output parameter
-- is set to @Void@. In other words, it is impossible to create a @HaveOutput@
-- constructor for a @Sink@.
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
pipeClose :: Monad m => Pipe i o m r -> m r
pipeClose = liftM snd . pipeCloseL

pipeCloseL :: Monad m => Pipe i o m r -> m (Maybe i, r)
pipeCloseL (HaveOutput _ c _) = ((,) Nothing) `liftM` c
pipeCloseL (NeedInput _ p)= ((,) Nothing) `liftM` pipeClose p
pipeCloseL (Done l r) = return (l, r)
pipeCloseL (PipeM _ c) = ((,) Nothing) `liftM` c

noInput :: Monad m => Pipe i o m r -> Pipe Void o m r
noInput (HaveOutput p r o) = HaveOutput (noInput p) r o
noInput (NeedInput _ c) = c
noInput (Done _ r) = Done Nothing r
noInput (PipeM mp c) = PipeM (noInput `liftM` mp) c

pipePush :: Monad m => i -> Pipe i o m r -> Pipe i o m r
pipePush i (HaveOutput p c o) = HaveOutput (pipePush i p) c o
pipePush i (NeedInput p _) = p i
pipePush i (Done _ r) = Done (Just i) r
pipePush i (PipeM mp c) = PipeM (pipePush i `liftM` mp) c

instance Monad m => Functor (Pipe i o m) where
    fmap f (HaveOutput p c o) = HaveOutput (f <$> p) (f `liftM` c) o
    fmap f (NeedInput p c) = NeedInput (fmap f . p) (f <$> c)
    fmap f (Done l r) = Done l (f r)
    fmap f (PipeM mp mr) = PipeM ((fmap f) `liftM` mp) (f `liftM` mr)

instance Monad m => Applicative (Pipe i o m) where
    pure = Done Nothing

    Done il f <*> Done ir x = Done (il <|> ir) (f x)

    PipeM mp mr <*> right = PipeM
        ((<*> right) `liftM` mp)
        (mr `ap` pipeClose right)
    HaveOutput p c o <*> right = HaveOutput
        (p <*> right)
        (c `ap` pipeClose right)
        o
    NeedInput p c <*> right = NeedInput
        (\i -> p i <*> right)
        (c `ap` noInput right)

    left@(Done _ f) <*> PipeM mp mr = PipeM
        ((left <*>) `liftM` mp)
        (f `liftM` mr)
    left@(Done _ f) <*> HaveOutput p c o = HaveOutput
        (left <*> p)
        (f `liftM` c)
        o
    left@(Done _ f) <*> NeedInput p c = NeedInput
        (\i -> left <*> p i)
        (liftM f c)

instance Monad m => Monad (Pipe i o m) where
    return = Done Nothing

    Done Nothing x >>= fp = fp x
    Done (Just i) x >>= fp = pipePush i $ fp x
    HaveOutput p c o >>= fp = HaveOutput (p >>= fp) (c >>= pipeClose . fp) o
    NeedInput p c >>= fp = NeedInput (p >=> fp) (c >>= noInput . fp)
    PipeM mp c >>= fp = PipeM ((>>= fp) `liftM` mp) (c >>= pipeClose . fp)

instance MonadBase base m => MonadBase base (Pipe i o m) where
    liftBase = lift . liftBase

instance MonadTrans (Pipe i o) where
    lift mr = PipeM (Done Nothing `liftM` mr) mr

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
pipe l r = pipeResume l r >>= \(l', res) -> lift (pipeClose l') >> return res

-- | Same as 'pipe', but retain both the new left pipe and the leftovers from
-- the right pipe. The two components are combined together into a single pipe
-- and returned, together with the result of the right pipe.
--
-- Note: we're biased towards checking the right side first to avoid pulling
-- extra data which is not needed. Doing so could cause data loss.
--
-- Since 0.4.0
pipeResume :: Monad m => Pipe a b m () -> Pipe b c m r -> Pipe a c m (Pipe a b m (), r)

pipeResume (Done leftoverl ()) (Done leftoverr r) =
    Done leftoverl (left, r)
  where
    left =
        case leftoverr of
            Nothing -> mempty
            Just i -> HaveOutput (Done Nothing ()) (return ()) i

pipeResume left (Done leftoverr r) =
    Done Nothing (left', r)
  where
    left' =
        case leftoverr of
            Nothing -> left
            Just i -> HaveOutput left (pipeClose left) i

-- Left pipe needs more input, ask for it.
pipeResume (NeedInput p c) right = NeedInput
    (\a -> pipeResume (p a) right)
    (do
        (left, res) <- pipeResume c right
        lift $ pipeClose left
        return (mempty, res)
        )

-- Left pipe has output, right pipe wants it.
pipeResume (HaveOutput lp _ a) (NeedInput rp _) = pipeResume lp (rp a)

-- Right pipe needs to run a monadic action.
pipeResume left (PipeM mp c) = PipeM
    (pipeResume left `liftM` mp)
    (((,) left) `liftM` c)

-- Right pipe has some output, provide it downstream and continue.
pipeResume left (HaveOutput p c o) = HaveOutput
    (pipeResume left p)
    (((,) left) `liftM` c)
    o

-- Left pipe is done, right pipe needs input. In such a case, tell the right
-- pipe there is no more input, and eventually replace its leftovers with the
-- left pipe's leftover.
pipeResume (Done l ()) (NeedInput _ c) = ((,) mempty) `liftM` replaceLeftover l c


-- Left pipe needs to run a monadic action.
pipeResume (PipeM mp c) right = PipeM
    ((`pipeResume` right) `liftM` mp)
    (c >> pipeCloseL right >>= \(_, res) -> return (mempty, res))

replaceLeftover :: Monad m => Maybe i -> Pipe Void o m r -> Pipe i o m r
replaceLeftover l (Done _ r) = Done l r
replaceLeftover l (HaveOutput p c o) = HaveOutput (replaceLeftover l p) c o

-- This function is only called on pipes when there is no more input available.
-- Therefore, we can ignore the push record.
replaceLeftover l (NeedInput _ c) = replaceLeftover l c

replaceLeftover l (PipeM mp c) = PipeM (replaceLeftover l `liftM` mp) c

-- | Run a complete pipeline until processing completes.
--
-- Since 0.4.0
runPipe :: Monad m => Pipe Void Void m r -> m r
runPipe (HaveOutput _ c _) = c
runPipe (NeedInput _ c) = runPipe c
runPipe (Done _ r) = return r
runPipe (PipeM mp _) = mp >>= runPipe

-- | Send a single output value downstream.
--
-- Since 0.4.0
yield :: Monad m => o -> Pipe i o m ()
yield = HaveOutput (Done Nothing ()) (return ())

-- | Wait for a single input value from upstream, and remove it from the
-- stream. Returns @Nothing@ if no more data is available.
--
-- Since 0.4.0
await :: Pipe i o m (Maybe i)
await = NeedInput (Done Nothing . Just) (Done Nothing Nothing)

-- | Check if input is available from upstream. Will not remove the data from
-- the stream.
--
-- Since 0.4.0
hasInput :: Pipe i o m Bool
hasInput = NeedInput (\i -> Done (Just i) False) (Done Nothing True)

-- | A @Sink@ has a @Void@ type parameter for the output, which makes it
-- difficult to compose with @Source@s and @Conduit@s. This function replaces
-- that parameter with a free variable. This function is essentially @id@; it
-- only modifies the types, not the actions performed.
--
-- Since 0.4.0
sinkToPipe :: Monad m => Sink i m r -> Pipe i o m r
sinkToPipe (HaveOutput _ c _) = lift c
sinkToPipe (NeedInput p c) = NeedInput (sinkToPipe . p) (sinkToPipe c)
sinkToPipe (Done i r) = Done i r
sinkToPipe (PipeM mp c) = PipeM (liftM sinkToPipe mp) c
