{-# LANGUAGE RankNTypes #-}
-- | Allow monad transformers to be run\/eval\/exec in a section of conduit
-- rather then needing to run across the whole conduit.  The circumvents many
-- of the problems with breaking the monad transformer laws.  For more
-- information, see the announcement blog post:
-- <http://www.yesodweb.com/blog/2014/01/conduit-transformer-exception>
--
-- This module was added in conduit 1.0.11.
module Data.Conduit.Lift (
    -- * ExceptT
    exceptC,
    runExceptC,
    catchExceptC,

    -- * CatchC
    runCatchC,

    -- * MaybeT
    maybeC,
    runMaybeC,

    -- * ReaderT
    readerC,
    runReaderC,

    -- * StateT, lazy
    stateLC,
    runStateLC,
    evalStateLC,
    execStateLC,

    -- ** Strict
    stateC,
    runStateC,
    evalStateC,
    execStateC,

    -- * WriterT, lazy
    writerLC,
    runWriterLC,
    execWriterLC,

    -- ** Strict
    writerC,
    runWriterC,
    execWriterC,

    -- * RWST, lazy
    rwsLC,
    runRWSLC,
    evalRWSLC,
    execRWSLC,

    -- ** Strict
    rwsC,
    runRWSC,
    evalRWSC,
    execRWSC
    ) where

import Data.Conduit
import Data.Conduit.Internal (ConduitT (..), Pipe (..))

import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Monoid (Monoid(..))


import qualified Control.Monad.Trans.Except as Ex
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R

import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.Writer.Strict as WS
import qualified Control.Monad.Trans.RWS.Strict as RWSS

import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.RWS.Lazy as RWSL

import Control.Monad.Catch.Pure (CatchT (runCatchT))
import Control.Exception (SomeException)

-- | Wrap the base monad in 'Ex.ExceptT'
--
-- Since 1.2.12
exceptC
  :: Monad m =>
     ConduitT i o m (Either e a) -> ConduitT i o (Ex.ExceptT e m) a
exceptC p = do
    x <- transPipe lift p
    lift $ Ex.ExceptT (return x)

-- | Run 'Ex.ExceptT' in the base monad
--
-- Since 1.2.12
runExceptC
  :: Monad m =>
     ConduitT i o (Ex.ExceptT e m) r -> ConduitT i o m (Either e r)
runExceptC (ConduitT c0) =
    ConduitT $ \rest ->
        let go (Done r) = rest (Right r)
            go (PipeM mp) = PipeM $ do
                eres <- Ex.runExceptT mp
                return $ case eres of
                    Left e -> rest $ Left e
                    Right p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p o) = HaveOutput (go p) o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go (c0 Done)
{-# INLINABLE runExceptC #-}

-- | Catch an error in the base monad
--
-- Since 1.2.12
catchExceptC
  :: Monad m =>
     ConduitT i o (Ex.ExceptT e m) r
     -> (e -> ConduitT i o (Ex.ExceptT e m) r)
     -> ConduitT i o (Ex.ExceptT e m) r
catchExceptC c0 h =
    ConduitT $ \rest ->
        let go (Done r) = rest r
            go (PipeM mp) = PipeM $ do
                eres <- lift $ Ex.runExceptT mp
                return $ case eres of
                    Left e -> unConduitT (h e) rest
                    Right p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p o) = HaveOutput (go p) o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go $ unConduitT c0 Done
  where
{-# INLINABLE catchExceptC #-}

-- | Run 'CatchT' in the base monad
--
-- Since 1.1.0
runCatchC
  :: Monad m =>
     ConduitT i o (CatchT m) r -> ConduitT i o m (Either SomeException r)
runCatchC c0 =
    ConduitT $ \rest ->
        let go (Done r) = rest (Right r)
            go (PipeM mp) = PipeM $ do
                eres <- runCatchT mp
                return $ case eres of
                    Left e -> rest $ Left e
                    Right p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p o) = HaveOutput (go p) o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go $ unConduitT c0 Done
{-# INLINABLE runCatchC #-}

-- | Wrap the base monad in 'M.MaybeT'
--
-- Since 1.0.11
maybeC
  :: Monad m =>
     ConduitT i o m (Maybe a) -> ConduitT i o (M.MaybeT m) a
maybeC p = do
    x <- transPipe lift p
    lift $ M.MaybeT (return x)
{-# INLINABLE maybeC #-}

-- | Run 'M.MaybeT' in the base monad
--
-- Since 1.0.11
runMaybeC
  :: Monad m =>
     ConduitT i o (M.MaybeT m) r -> ConduitT i o m (Maybe r)
runMaybeC (ConduitT c0) =
    ConduitT $ \rest ->
        let go (Done r) = rest (Just r)
            go (PipeM mp) = PipeM $ do
                mres <- M.runMaybeT mp
                return $ case mres of
                    Nothing -> rest Nothing
                    Just p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p o) = HaveOutput (go p) o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go (c0 Done)
{-# INLINABLE runMaybeC #-}

-- | Wrap the base monad in 'R.ReaderT'
--
-- Since 1.0.11
readerC
  :: Monad m =>
     (r -> ConduitT i o m a) -> ConduitT i o (R.ReaderT r m) a
readerC k = do
    i <- lift R.ask
    transPipe lift (k i)
{-# INLINABLE readerC #-}

-- | Run 'R.ReaderT' in the base monad
--
-- Since 1.0.11
runReaderC
  :: Monad m =>
     r -> ConduitT i o (R.ReaderT r m) res -> ConduitT i o m res
runReaderC r = transPipe (`R.runReaderT` r)
{-# INLINABLE runReaderC #-}


-- | Wrap the base monad in 'SL.StateT'
--
-- Since 1.0.11
stateLC
  :: Monad m =>
     (s -> ConduitT i o m (a, s)) -> ConduitT i o (SL.StateT s m) a
stateLC k = do
    s <- lift SL.get
    (r, s') <- transPipe lift (k s)
    lift (SL.put s')
    return r
{-# INLINABLE stateLC #-}

thread :: Monad m
       => (r -> s -> res)
       -> (forall a. t m a -> s -> m (a, s))
       -> s
       -> ConduitT i o (t m) r
       -> ConduitT i o m res
thread toRes runM s0 (ConduitT c0) =
    ConduitT $ \rest ->
        let go s (Done r) = rest (toRes r s)
            go s (PipeM mp) = PipeM $ do
                (p, s') <- runM mp s
                return $ go s' p
            go s (Leftover p i) = Leftover (go s p) i
            go s (NeedInput x y) = NeedInput (go s . x) (go s . y)
            go s (HaveOutput p o) = HaveOutput (go s p) o
         in go s0 (c0 Done)
{-# INLINABLE thread #-}

-- | Run 'SL.StateT' in the base monad
--
-- Since 1.0.11
runStateLC
  :: Monad m =>
     s -> ConduitT i o (SL.StateT s m) r -> ConduitT i o m (r, s)
runStateLC = thread (,) SL.runStateT
{-# INLINABLE runStateLC #-}

-- | Evaluate 'SL.StateT' in the base monad
--
-- Since 1.0.11
evalStateLC
  :: Monad m =>
     s -> ConduitT i o (SL.StateT s m) r -> ConduitT i o m r
evalStateLC s p = fmap fst $ runStateLC s p
{-# INLINABLE evalStateLC #-}

-- | Execute 'SL.StateT' in the base monad
--
-- Since 1.0.11
execStateLC
  :: Monad m =>
     s -> ConduitT i o (SL.StateT s m) r -> ConduitT i o m s
execStateLC s p = fmap snd $ runStateLC s p
{-# INLINABLE execStateLC #-}


-- | Wrap the base monad in 'SS.StateT'
--
-- Since 1.0.11
stateC
  :: Monad m =>
     (s -> ConduitT i o m (a, s)) -> ConduitT i o (SS.StateT s m) a
stateC k = do
    s <- lift SS.get
    (r, s') <- transPipe lift (k s)
    lift (SS.put s')
    return r
{-# INLINABLE stateC #-}

-- | Run 'SS.StateT' in the base monad
--
-- Since 1.0.11
runStateC
  :: Monad m =>
     s -> ConduitT i o (SS.StateT s m) r -> ConduitT i o m (r, s)
runStateC = thread (,) SS.runStateT
{-# INLINABLE runStateC #-}

-- | Evaluate 'SS.StateT' in the base monad
--
-- Since 1.0.11
evalStateC
  :: Monad m =>
     s -> ConduitT i o (SS.StateT s m) r -> ConduitT i o m r
evalStateC s p = fmap fst $ runStateC s p
{-# INLINABLE evalStateC #-}

-- | Execute 'SS.StateT' in the base monad
--
-- Since 1.0.11
execStateC
  :: Monad m =>
     s -> ConduitT i o (SS.StateT s m) r -> ConduitT i o m s
execStateC s p = fmap snd $ runStateC s p
{-# INLINABLE execStateC #-}


-- | Wrap the base monad in 'WL.WriterT'
--
-- Since 1.0.11
writerLC
  :: (Monad m, Monoid w) =>
     ConduitT i o m (b, w) -> ConduitT i o (WL.WriterT w m) b
writerLC p = do
    (r, w) <- transPipe lift p
    lift $ WL.tell w
    return r
{-# INLINABLE writerLC #-}

-- | Run 'WL.WriterT' in the base monad
--
-- Since 1.0.11
runWriterLC
  :: (Monad m, Monoid w) =>
     ConduitT i o (WL.WriterT w m) r -> ConduitT i o m (r, w)
runWriterLC = thread (,) run mempty
  where
    run m w = do
        (a, w') <- WL.runWriterT m
        return (a, w `mappend` w')
{-# INLINABLE runWriterLC #-}

-- | Execute 'WL.WriterT' in the base monad
--
-- Since 1.0.11
execWriterLC
  :: (Monad m, Monoid w) =>
     ConduitT i o (WL.WriterT w m) r -> ConduitT i o m w
execWriterLC p = fmap snd $ runWriterLC p
{-# INLINABLE execWriterLC #-}


-- | Wrap the base monad in 'WS.WriterT'
--
-- Since 1.0.11
writerC
  :: (Monad m, Monoid w) =>
     ConduitT i o m (b, w) -> ConduitT i o (WS.WriterT w m) b
writerC p = do
    (r, w) <- transPipe lift p
    lift $ WS.tell w
    return r
{-# INLINABLE writerC #-}

-- | Run 'WS.WriterT' in the base monad
--
-- Since 1.0.11
runWriterC
  :: (Monad m, Monoid w) =>
     ConduitT i o (WS.WriterT w m) r -> ConduitT i o m (r, w)
runWriterC = thread (,) run mempty
  where
    run m w = do
        (a, w') <- WS.runWriterT m
        return (a, w `mappend` w')
{-# INLINABLE runWriterC #-}

-- | Execute 'WS.WriterT' in the base monad
--
-- Since 1.0.11
execWriterC
  :: (Monad m, Monoid w) =>
     ConduitT i o (WS.WriterT w m) r -> ConduitT i o m w
execWriterC p = fmap snd $ runWriterC p
{-# INLINABLE execWriterC #-}


-- | Wrap the base monad in 'RWSL.RWST'
--
-- Since 1.0.11
rwsLC
  :: (Monad m, Monoid w) =>
     (r -> s -> ConduitT i o m (a, s, w)) -> ConduitT i o (RWSL.RWST r w s m) a
rwsLC k = do
    i <- lift RWSL.ask
    s <- lift RWSL.get
    (r, s', w) <- transPipe lift (k i s)
    lift $ do
        RWSL.put s'
        RWSL.tell w
    return r
{-# INLINABLE rwsLC #-}

-- | Run 'RWSL.RWST' in the base monad
--
-- Since 1.0.11
runRWSLC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitT i o (RWSL.RWST r w s m) res
     -> ConduitT i o m (res, s, w)
runRWSLC r s0 = thread toRes run (s0, mempty)
  where
    toRes a (s, w) = (a, s, w)
    run m (s, w) = do
        (res, s', w') <- RWSL.runRWST m r s
        return (res, (s', w `mappend` w'))
{-# INLINABLE runRWSLC #-}

-- | Evaluate 'RWSL.RWST' in the base monad
--
-- Since 1.0.11
evalRWSLC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitT i o (RWSL.RWST r w s m) res
     -> ConduitT i o m (res, w)
evalRWSLC i s p = fmap f $ runRWSLC i s p
  where f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSLC #-}

-- | Execute 'RWSL.RWST' in the base monad
--
-- Since 1.0.11
execRWSLC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitT i o (RWSL.RWST r w s m) res
     -> ConduitT i o m (s, w)
execRWSLC i s p = fmap f $ runRWSLC i s p
  where f x = let (_, s2, w2) = x in (s2, w2)
{-# INLINABLE execRWSLC #-}

-- | Wrap the base monad in 'RWSS.RWST'
--
-- Since 1.0.11
rwsC
  :: (Monad m, Monoid w) =>
     (r -> s -> ConduitT i o m (a, s, w)) -> ConduitT i o (RWSS.RWST r w s m) a
rwsC k = do
    i <- lift RWSS.ask
    s <- lift RWSS.get
    (r, s', w) <- transPipe lift (k i s)
    lift $ do
        RWSS.put s'
        RWSS.tell w
    return r
{-# INLINABLE rwsC #-}

-- | Run 'RWSS.RWST' in the base monad
--
-- Since 1.0.11
runRWSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitT i o (RWSS.RWST r w s m) res
     -> ConduitT i o m (res, s, w)
runRWSC r s0 = thread toRes run (s0, mempty)
  where
    toRes a (s, w) = (a, s, w)
    run m (s, w) = do
        (res, s', w') <- RWSS.runRWST m r s
        return (res, (s', w `mappend` w'))
{-# INLINABLE runRWSC #-}

-- | Evaluate 'RWSS.RWST' in the base monad
--
-- Since 1.0.11
evalRWSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitT i o (RWSS.RWST r w s m) res
     -> ConduitT i o m (res, w)
evalRWSC i s p = fmap f $ runRWSC i s p
  where f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSC #-}

-- | Execute 'RWSS.RWST' in the base monad
--
-- Since 1.0.11
execRWSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitT i o (RWSS.RWST r w s m) res
     -> ConduitT i o m (s, w)
execRWSC i s p = fmap f $ runRWSC i s p
  where f x = let (_, s2, w2) = x in (s2, w2)
{-# INLINABLE execRWSC #-}
