module Data.Conduit.Internal.Primitives
    ( -- * Identity
      idP
      -- * Producing
    , yield
    , yieldOr
    , tryYield
      -- * Consuming
    , await
    , awaitTerm
    , awaitForever
    , leftover
      -- * Termination
    , checkDownstream
    , closeDownstream
    , terminatePipe
    , checkTerminate
      -- * Finalization
    , addCleanup
    , bracketP
    , bracketPNoCheck
    ) where

import Data.Conduit.Internal.Pipe
import Control.Monad (liftM)
import Control.Monad.Trans.Resource (MonadResource, allocate, release)

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

-- | Send a single output value downstream. If the downstream @ConduitM@
-- terminates, this @ConduitM@ will terminate as well.
--
-- Since 0.5.0
yield :: Monad m => o -> Pipe i o d d m ()
yield = Yield (Pure [] ()) (const $ Terminate [])
{-# INLINE [1] yield #-}
{-# RULES "yield o >> p" forall o p. yield o >> p = Yield p (const $ Terminate []) o #-}

-- | Similar to @yield@, but additionally takes a finalizer to be run if the
-- downstream @ConduitM@ terminates.
--
-- Since 0.5.0
yieldOr :: Monad m => o -> m () -> Pipe i o d d m ()
yieldOr o f = Yield (Pure [] ()) (\_ d -> M (f >> return (Terminate [] d))) o

-- | Yield a value downstream, but instead of automatically terminating if
-- downstream closes, return the downstream leftovers and finalizer.
--
-- Since 2.0.0
tryYield :: Monad m => o -> Pipe i o d t m (Maybe ([o], d))
tryYield = Yield (Pure [] Nothing) (\os d -> Pure [] $ Just (os, d))

-- | Wait for a single input value from upstream.
--
-- Since 0.5.0
await :: Monad m => Pipe i o d t m (Maybe i)
await = Await (Pure [] . Just) (Pure [] Nothing)
{-# RULES "await >>= maybe" forall x y. await >>= maybe x y = Await y x #-}
{-# INLINE [1] await #-}

-- | A terminating await: if upstream provides no values, terminates with the
-- downstream return value.
awaitTerm :: Monad m => Pipe i o d d m i
awaitTerm = await >>= maybe (closeDownstream >>= terminatePipe . snd) return

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

-- | Ensure that downstream is still active.
--
-- Since 2.0.0
checkDownstream :: Monad m => Pipe i o d t m (Maybe ([o], d))
checkDownstream = Check (Pure [] Nothing) (\os d -> Pure [] $ Just (os, d))

-- | Notify downstream that we're all done generating output.
--
-- Since 2.0.0
closeDownstream :: Monad m => Pipe i o d t m ([o], d)
closeDownstream = Empty $ curry $ Pure []

-- | Terminate the current pipe with the given value.
--
-- Since 2.0.0
terminatePipe :: Monad m => t -> Pipe i o d t m r
terminatePipe = Terminate []

-- | Check if downstream is closed. If so, terminate with the downstream return
-- value.
--
-- Since 2.0.0
checkTerminate :: Monad m => Pipe i o d d m ()
checkTerminate = checkDownstream >>= maybe (return ()) (terminatePipe . snd)

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

-- | Perform some allocation and run an inner @ConduitM@. Two guarantees are given
-- about resource finalization:
--
-- 1. It will be /prompt/. The finalization will be run as early as possible.
--
-- 2. It is exception safe. Due to usage of @resourcet@, the finalization will
--    be run in the event of any exceptions.
--
-- Before running, this function will check if downstream has closed and, if
-- so, will immediately terminate. Note that, as a result, this function cannot
-- be used in creating @Sink@s, which do not have automatic termination.
--
-- Since 0.5.0
bracketP :: MonadResource m
         => IO a
         -> (a -> IO ())
         -> (a -> Pipe i o d d m r)
         -> Pipe i o d d m r
bracketP alloc free inside = do
    checkDownstream >>= maybe (bracketPNoCheck alloc free inside) (terminatePipe . snd)

-- | Same as @bracketP@, but does not perform a check on downstream for
-- aliveness before running.
--
-- Since 2.0.0
bracketPNoCheck
    :: MonadResource m
    => IO a
    -> (a -> IO ())
    -> (a -> Pipe i o d t m r)
    -> Pipe i o d t m r
bracketPNoCheck alloc free inside = do
    (key, seed) <- allocate alloc free
    addCleanup (const $ release key) (inside seed)
