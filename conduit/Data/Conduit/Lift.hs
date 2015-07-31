{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
-- | Allow monad transformers to be run\/eval\/exec in a section of conduit
-- rather then needing to run across the whole conduit.  The circumvents many
-- of the problems with breaking the monad transformer laws.  For more
-- information, see the announcement blog post:
-- <http://www.yesodweb.com/blog/2014/01/conduit-transformer-exception>
--
-- This module was added in conduit 1.0.11.
module Data.Conduit.Lift (
    -- * ErrorT
    errorC,
    runErrorC,
    catchErrorC,
--    liftCatchError,

    -- * CatchT
    runCatchC,
    catchCatchC,

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
    execRWSC,

    -- * Utilities

    distribute
    ) where

import Data.Conduit
import Data.Conduit.Internal (ConduitM (..), Pipe (..))

import Control.Monad.Morph (hoist, lift, MFunctor(..), )
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Exception (SomeException)

import Data.Monoid (Monoid(..))


import qualified Control.Monad.Trans.Error as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R

import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.Writer.Strict as WS
import qualified Control.Monad.Trans.RWS.Strict as RWSS

import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import Control.Monad.Catch.Pure (CatchT (runCatchT))


catAwaitLifted
  :: (Monad (t (ConduitM o1 o m)), Monad m, MonadTrans t) =>
     ConduitM i o1 (t (ConduitM o1 o m)) ()
catAwaitLifted = go
  where
    go = do
        x <- lift . lift $ await
        case x of
            Nothing -> return ()
            Just x2 -> do
                yield x2
                go

catYieldLifted
  :: (Monad (t (ConduitM i o1 m)), Monad m, MonadTrans t) =>
     ConduitM o1 o (t (ConduitM i o1 m)) ()
catYieldLifted = go
  where
    go = do
        x <- await
        case x of
            Nothing -> return ()
            Just x2 -> do
                lift . lift $ yield x2
                go


distribute
  :: (Monad (t (ConduitM b o m)), Monad m, Monad (t m), MonadTrans t,
      MFunctor t) =>
     ConduitM b o (t m) () -> t (ConduitM b o m) ()
distribute p = catAwaitLifted =$= hoist (hoist lift) p $$ catYieldLifted

-- | Wrap the base monad in 'E.ErrorT'
--
-- Since 1.0.11
errorC
  :: (Monad m, Monad (t (E.ErrorT e m)), MonadTrans t, E.Error e,
      MFunctor t) =>
     t m (Either e b) -> t (E.ErrorT e m) b
errorC p = do
    x <- hoist lift p
    lift $ E.ErrorT (return x)

-- | Run 'E.ErrorT' in the base monad
--
-- Since 1.0.11
runErrorC
  :: (Monad m, E.Error e) =>
     ConduitM i o (E.ErrorT e m) r -> ConduitM i o m (Either e r)
runErrorC (ConduitM c0) =
    ConduitM $ \rest ->
        let go (Done r) = rest (Right r)
            go (PipeM mp) = PipeM $ do
                eres <- E.runErrorT mp
                return $ case eres of
                    Left e -> rest $ Left e
                    Right p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p f o) = HaveOutput (go p) (E.runErrorT f >> return ()) o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go (c0 Done)
{-# INLINABLE runErrorC #-}

-- | Catch an error in the base monad
--
-- Since 1.0.11
catchErrorC
  :: (Monad m, E.Error e) =>
     ConduitM i o (E.ErrorT e m) r
     -> (e -> ConduitM i o (E.ErrorT e m) r)
     -> ConduitM i o (E.ErrorT e m) r
catchErrorC c0 h =
    ConduitM $ \rest ->
        let go (Done r) = rest r
            go (PipeM mp) = PipeM $ do
                eres <- lift $ E.runErrorT mp
                return $ case eres of
                    Left e -> unConduitM (h e) rest
                    Right p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p f o) = HaveOutput (go p) f o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go $ unConduitM c0 Done
  where
{-# INLINABLE catchErrorC #-}

-- | Run 'CatchT' in the base monad
--
-- Since 1.1.0
runCatchC
  :: Monad m =>
     ConduitM i o (CatchT m) r -> ConduitM i o m (Either SomeException r)
runCatchC c0 =
    ConduitM $ \rest ->
        let go (Done r) = rest (Right r)
            go (PipeM mp) = PipeM $ do
                eres <- runCatchT mp
                return $ case eres of
                    Left e -> rest $ Left e
                    Right p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p f o) = HaveOutput (go p) (runCatchT f >> return ()) o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go $ unConduitM c0 Done
{-# INLINABLE runCatchC #-}

-- | Catch an exception in the base monad
--
-- Since 1.1.0
catchCatchC
  :: Monad m =>
     ConduitM i o (CatchT m) r
     -> (SomeException -> ConduitM i o (CatchT m) r)
     -> ConduitM i o (CatchT m) r
catchCatchC (ConduitM c0) h =
    ConduitM $ \rest ->
        let go (Done r) = rest r
            go (PipeM mp) = PipeM $ do
                eres <- lift $ runCatchT mp
                return $ case eres of
                    Left e -> unConduitM (h e) rest
                    Right p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p f o) = HaveOutput (go p) f o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go (c0 Done)
{-# INLINABLE catchCatchC #-}

-- | Wrap the base monad in 'M.MaybeT'
--
-- Since 1.0.11
maybeC
  :: (Monad m, Monad (t (M.MaybeT m)),
      MonadTrans t,
      MFunctor t) =>
     t m (Maybe b) -> t (M.MaybeT m) b
maybeC p = do
    x <- hoist lift p
    lift $ M.MaybeT (return x)
{-# INLINABLE maybeC #-}

-- | Run 'M.MaybeT' in the base monad
--
-- Since 1.0.11
runMaybeC
  :: Monad m =>
     ConduitM i o (M.MaybeT m) r -> ConduitM i o m (Maybe r)
runMaybeC (ConduitM c0) =
    ConduitM $ \rest ->
        let go (Done r) = rest (Just r)
            go (PipeM mp) = PipeM $ do
                mres <- M.runMaybeT mp
                return $ case mres of
                    Nothing -> rest Nothing
                    Just p -> go p
            go (Leftover p i) = Leftover (go p) i
            go (HaveOutput p c o) = HaveOutput (go p) (M.runMaybeT c >> return ()) o
            go (NeedInput x y) = NeedInput (go . x) (go . y)
         in go (c0 Done)
{-# INLINABLE runMaybeC #-}

-- | Wrap the base monad in 'R.ReaderT'
--
-- Since 1.0.11
readerC
  :: (Monad m, Monad (t1 (R.ReaderT t m)),
      MonadTrans t1,
      MFunctor t1) =>
     (t -> t1 m b) -> t1 (R.ReaderT t m) b
readerC k = do
    i <- lift R.ask
    hoist lift (k i)
{-# INLINABLE readerC #-}

-- | Run 'R.ReaderT' in the base monad
--
-- Since 1.0.11
runReaderC
  :: Monad m =>
     r -> ConduitM i o (R.ReaderT r m) res -> ConduitM i o m res
runReaderC r = hoist (`R.runReaderT` r)
{-# INLINABLE runReaderC #-}


-- | Wrap the base monad in 'SL.StateT'
--
-- Since 1.0.11
stateLC
  :: (Monad m, Monad (t1 (SL.StateT t m)),
      MonadTrans t1,
      MFunctor t1) =>
     (t -> t1 m (b, t)) -> t1 (SL.StateT t m) b
stateLC k = do
    s <- lift SL.get
    (r, s') <- hoist lift (k s)
    lift (SL.put s')
    return r
{-# INLINABLE stateLC #-}

thread :: Monad m
       => (r -> s -> res)
       -> (forall a. t m a -> s -> m (a, s))
       -> s
       -> ConduitM i o (t m) r
       -> ConduitM i o m res
thread toRes runM s0 (ConduitM c0) =
    ConduitM $ \rest ->
        let go s (Done r) = rest (toRes r s)
            go s (PipeM mp) = PipeM $ do
                (p, s') <- runM mp s
                return $ go s' p
            go s (Leftover p i) = Leftover (go s p) i
            go s (NeedInput x y) = NeedInput (go s . x) (go s . y)
            go s (HaveOutput p f o) = HaveOutput (go s p) (runM f s >> return ()) o
         in go s0 (c0 Done)
{-# INLINABLE thread #-}

-- | Run 'SL.StateT' in the base monad
--
-- Since 1.0.11
runStateLC
  :: Monad m =>
     s -> ConduitM i o (SL.StateT s m) r -> ConduitM i o m (r, s)
runStateLC = thread (,) SL.runStateT
{-# INLINABLE runStateLC #-}

-- | Evaluate 'SL.StateT' in the base monad
--
-- Since 1.0.11
evalStateLC
  :: Monad m =>
     s -> ConduitM i o (SL.StateT s m) r -> ConduitM i o m r
evalStateLC s p = fmap fst $ runStateLC s p
{-# INLINABLE evalStateLC #-}

-- | Execute 'SL.StateT' in the base monad
--
-- Since 1.0.11
execStateLC
  :: Monad m =>
     s -> ConduitM i o (SL.StateT s m) r -> ConduitM i o m s
execStateLC s p = fmap snd $ runStateLC s p
{-# INLINABLE execStateLC #-}


-- | Wrap the base monad in 'SS.StateT'
--
-- Since 1.0.11
stateC
  :: (Monad m, Monad (t1 (SS.StateT t m)),
      MonadTrans t1,
      MFunctor t1) =>
     (t -> t1 m (b, t)) -> t1 (SS.StateT t m) b
stateC k = do
    s <- lift SS.get
    (r, s') <- hoist lift (k s)
    lift (SS.put s')
    return r
{-# INLINABLE stateC #-}

-- | Run 'SS.StateT' in the base monad
--
-- Since 1.0.11
runStateC
  :: Monad m =>
     s -> ConduitM i o (SS.StateT s m) r -> ConduitM i o m (r, s)
runStateC = thread (,) SS.runStateT
{-# INLINABLE runStateC #-}

-- | Evaluate 'SS.StateT' in the base monad
--
-- Since 1.0.11
evalStateC
  :: Monad m =>
     s -> ConduitM i o (SS.StateT s m) r -> ConduitM i o m r
evalStateC s p = fmap fst $ runStateC s p
{-# INLINABLE evalStateC #-}

-- | Execute 'SS.StateT' in the base monad
--
-- Since 1.0.11
execStateC
  :: Monad m =>
     s -> ConduitM i o (SS.StateT s m) r -> ConduitM i o m s
execStateC s p = fmap snd $ runStateC s p
{-# INLINABLE execStateC #-}


-- | Wrap the base monad in 'WL.WriterT'
--
-- Since 1.0.11
writerLC
  :: (Monad m, Monad (t (WL.WriterT w m)), MonadTrans t, Monoid w,
      MFunctor t) =>
     t m (b, w) -> t (WL.WriterT w m) b
writerLC p = do
    (r, w) <- hoist lift p
    lift $ WL.tell w
    return r
{-# INLINABLE writerLC #-}

-- | Run 'WL.WriterT' in the base monad
--
-- Since 1.0.11
runWriterLC
  :: (Monad m, Monoid w) =>
     ConduitM i o (WL.WriterT w m) r -> ConduitM i o m (r, w)
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
     ConduitM i o (WL.WriterT w m) r -> ConduitM i o m w
execWriterLC p = fmap snd $ runWriterLC p
{-# INLINABLE execWriterLC #-}


-- | Wrap the base monad in 'WS.WriterT'
--
-- Since 1.0.11
writerC
  :: (Monad m, Monad (t (WS.WriterT w m)), MonadTrans t, Monoid w,
      MFunctor t) =>
     t m (b, w) -> t (WS.WriterT w m) b
writerC p = do
    (r, w) <- hoist lift p
    lift $ WS.tell w
    return r
{-# INLINABLE writerC #-}

-- | Run 'WS.WriterT' in the base monad
--
-- Since 1.0.11
runWriterC
  :: (Monad m, Monoid w) =>
     ConduitM i o (WS.WriterT w m) r -> ConduitM i o m (r, w)
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
     ConduitM i o (WS.WriterT w m) r -> ConduitM i o m w
execWriterC p = fmap snd $ runWriterC p
{-# INLINABLE execWriterC #-}


-- | Wrap the base monad in 'RWSL.RWST'
--
-- Since 1.0.11
rwsLC
  :: (Monad m, Monad (t1 (RWSL.RWST t w t2 m)), MonadTrans t1,
      Monoid w, MFunctor t1) =>
     (t -> t2 -> t1 m (b, t2, w)) -> t1 (RWSL.RWST t w t2 m) b
rwsLC k = do
    i <- lift RWSL.ask
    s <- lift RWSL.get
    (r, s', w) <- hoist lift (k i s)
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
     -> ConduitM i o (RWSL.RWST r w s m) res
     -> ConduitM i o m (res, s, w)
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
     -> ConduitM i o (RWSL.RWST r w s m) res
     -> ConduitM i o m (res, w)
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
     -> ConduitM i o (RWSL.RWST r w s m) res
     -> ConduitM i o m (s, w)
execRWSLC i s p = fmap f $ runRWSLC i s p
  where f x = let (_, s2, w2) = x in (s2, w2)
{-# INLINABLE execRWSLC #-}


-- | Wrap the base monad in 'RWSS.RWST'
--
-- Since 1.0.11
rwsC
  :: (Monad m, Monad (t1 (RWSS.RWST t w t2 m)), MonadTrans t1,
      Monoid w, MFunctor t1) =>
     (t -> t2 -> t1 m (b, t2, w)) -> t1 (RWSS.RWST t w t2 m) b
rwsC k = do
    i <- lift RWSS.ask
    s <- lift RWSS.get
    (r, s', w) <- hoist lift (k i s)
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
     -> ConduitM i o (RWSS.RWST r w s m) res
     -> ConduitM i o m (res, s, w)
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
     -> ConduitM i o (RWSS.RWST r w s m) res
     -> ConduitM i o m (res, w)
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
     -> ConduitM i o (RWSS.RWST r w s m) res
     -> ConduitM i o m (s, w)
execRWSC i s p = fmap f $ runRWSC i s p
  where f x = let (_, s2, w2) = x in (s2, w2)
{-# INLINABLE execRWSC #-}
