{-# LANGUAGE RankNTypes #-}
{- | Allow monad transformers to be run/eval/exec in a section of conduit
 rather then needing to run across the whole conduit.
The circumvents many of the problems with breaking the monad transformer laws.
Read more about when the monad transformer laws are broken:
<https://github.com/snoyberg/conduit/wiki/Dealing-with-monad-transformers>

This method has a considerable number of advantages over the other two 
recommended methods.

* Run the monad transformer outisde of the conduit
* Use a mutable varible inside a readerT to retain side effects.

This functionality has existed for awhile in the pipes ecosystem and my recent
improvement to the Pipes.Lift module has allowed it to almost mechanically 
translated for conduit.

-}


module Data.Conduit.Lift (
    -- * ErrorT
    errorC,
    runErrorC,
    catchErrorC,
--    liftCatchError,

    -- * MaybeT
    maybeC,
    runMaybeC,

    -- * ReaderT
    readerC,
    runReaderC,

    -- * StateT
    stateC,
    runStateC,
    evalStateC,
    execStateC,

    -- ** Strict
    stateSC,
    runStateSC,
    evalStateSC,
    execStateSC,

    -- * WriterT
    writerC,
    runWriterC,
    execWriterC,

    -- ** Strict
    writerSC,
    runWriterSC,
    execWriterSC,

    -- * RWST
    rwsC,
    runRWSC,
    evalRWSC,
    execRWSC,

    -- ** Strict
    rwsSC,
    runRWSSC,
    evalRWSSC,
    execRWSSC,

    -- * Utilities

    distribute
    ) where

import Data.Conduit
import Data.Conduit.Internal (ConduitM (..), Pipe (..))

import Control.Monad.Morph (hoist, lift, MFunctor(..), )
import Control.Monad.Trans.Class (MonadTrans(..))

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

-- | Run 'E.ErrorT' in the base monad
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
runErrorC =
    ConduitM . go . unConduitM
  where
    go (Done r) = Done (Right r)
    go (PipeM mp) = PipeM $ do
        eres <- E.runErrorT mp
        return $ case eres of
            Left e -> Done $ Left e
            Right p -> go p
    go (Leftover p i) = Leftover (go p) i
    go (HaveOutput p f o) = HaveOutput (go p) (E.runErrorT f >> return ()) o
    go (NeedInput x y) = NeedInput (go . x) (go . y)
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
    ConduitM $ go $ unConduitM c0
  where
    go (Done r) = Done r
    go (PipeM mp) = PipeM $ do
        eres <- lift $ E.runErrorT mp
        return $ case eres of
            Left e -> unConduitM $ h e
            Right p -> go p
    go (Leftover p i) = Leftover (go p) i
    go (HaveOutput p f o) = HaveOutput (go p) f o
    go (NeedInput x y) = NeedInput (go . x) (go . y)
{-# INLINABLE catchErrorC #-}

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
runMaybeC =
    ConduitM . go . unConduitM
  where
    go (Done r) = Done (Just r)
    go (PipeM mp) = PipeM $ do
        mres <- M.runMaybeT mp
        return $ case mres of
            Nothing -> Done Nothing
            Just p -> go p
    go (Leftover p i) = Leftover (go p) i
    go (HaveOutput p c o) = HaveOutput (go p) (M.runMaybeT c >> return ()) o
    go (NeedInput x y) = NeedInput (go . x) (go . y)
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
stateC
  :: (Monad m, Monad (t1 (SL.StateT t m)),
      MonadTrans t1,
      MFunctor t1) =>
     (t -> t1 m (b, t)) -> t1 (SL.StateT t m) b
stateC k = do
    s <- lift SL.get
    (r, s') <- hoist lift (k s)
    lift (SL.put s')
    return r
{-# INLINABLE stateC #-}

thread :: Monad m
       => (r -> s -> res)
       -> (forall a. t m a -> s -> m (a, s))
       -> s
       -> ConduitM i o (t m) r
       -> ConduitM i o m res
thread toRes runM s0 =
    ConduitM . go s0 . unConduitM
  where
    go s (Done r) = Done (toRes r s)
    go s (PipeM mp) = PipeM $ do
        (p, s') <- runM mp s
        return $ go s' p
    go s (Leftover p i) = Leftover (go s p) i
    go s (NeedInput x y) = NeedInput (go s . x) (go s . y)
    go s (HaveOutput p f o) = HaveOutput (go s p) (runM f s >> return ()) o
{-# INLINABLE thread #-}

-- | Run 'SL.StateT' in the base monad
--
-- Since 1.0.11
runStateC
  :: Monad m =>
     s -> ConduitM i o (SL.StateT s m) r -> ConduitM i o m (r, s)
runStateC = thread (,) SL.runStateT
{-# INLINABLE runStateC #-}

-- | Evaluate 'SL.StateT' in the base monad
--
-- Since 1.0.11
evalStateC
  :: Monad m =>
     s -> ConduitM i o (SL.StateT s m) r -> ConduitM i o m r
evalStateC s p = fmap fst $ runStateC s p
{-# INLINABLE evalStateC #-}

-- | Execute 'SL.StateT' in the base monad
--
-- Since 1.0.11
execStateC
  :: Monad m =>
     s -> ConduitM i o (SL.StateT s m) r -> ConduitM i o m s
execStateC s p = fmap snd $ runStateC s p
{-# INLINABLE execStateC #-}


-- | Wrap the base monad in 'SS.StateT'
--
-- Since 1.0.11
stateSC
  :: (Monad m, Monad (t1 (SS.StateT t m)),
      MonadTrans t1,
      MFunctor t1) =>
     (t -> t1 m (b, t)) -> t1 (SS.StateT t m) b
stateSC k = do
    s <- lift SS.get
    (r, s') <- hoist lift (k s)
    lift (SS.put s')
    return r
{-# INLINABLE stateSC #-}

-- | Run 'SS.StateT' in the base monad
--
-- Since 1.0.11
runStateSC
  :: Monad m =>
     s -> ConduitM i o (SS.StateT s m) r -> ConduitM i o m (r, s)
runStateSC = thread (,) SS.runStateT
{-# INLINABLE runStateSC #-}

-- | Evaluate 'SS.StateT' in the base monad
--
-- Since 1.0.11
evalStateSC
  :: Monad m =>
     s -> ConduitM i o (SS.StateT s m) r -> ConduitM i o m r
evalStateSC s p = fmap fst $ runStateSC s p
{-# INLINABLE evalStateSC #-}

-- | Execute 'SS.StateT' in the base monad
--
-- Since 1.0.11
execStateSC
  :: Monad m =>
     s -> ConduitM i o (SS.StateT s m) r -> ConduitM i o m s
execStateSC s p = fmap snd $ runStateSC s p
{-# INLINABLE execStateSC #-}


-- | Wrap the base monad in 'WL.WriterT'
--
-- Since 1.0.11
writerC
  :: (Monad m, Monad (t (WL.WriterT w m)), MonadTrans t, Monoid w,
      MFunctor t) =>
     t m (b, w) -> t (WL.WriterT w m) b
writerC p = do
    (r, w) <- hoist lift p
    lift $ WL.tell w
    return r
{-# INLINABLE writerC #-}

-- | Run 'WL.WriterT' in the base monad
--
-- Since 1.0.11
runWriterC
  :: (Monad m, Monoid w) =>
     ConduitM i o (WL.WriterT w m) r -> ConduitM i o m (r, w)
runWriterC = thread (,) run mempty
  where
    run m w = do
        (a, w') <- WL.runWriterT m
        return (a, w `mappend` w')
{-# INLINABLE runWriterC #-}

-- | Execute 'WL.WriterT' in the base monad
--
-- Since 1.0.11
execWriterC
  :: (Monad m, Monoid w) =>
     ConduitM i o (WL.WriterT w m) r -> ConduitM i o m w
execWriterC p = fmap snd $ runWriterC p
{-# INLINABLE execWriterC #-}


-- | Wrap the base monad in 'WS.WriterT'
--
-- Since 1.0.11
writerSC
  :: (Monad m, Monad (t (WS.WriterT w m)), MonadTrans t, Monoid w,
      MFunctor t) =>
     t m (b, w) -> t (WS.WriterT w m) b
writerSC p = do
    (r, w) <- hoist lift p
    lift $ WS.tell w
    return r
{-# INLINABLE writerSC #-}

-- | Run 'WS.WriterT' in the base monad
--
-- Since 1.0.11
runWriterSC
  :: (Monad m, Monoid w) =>
     ConduitM i o (WS.WriterT w m) r -> ConduitM i o m (r, w)
runWriterSC = thread (,) run mempty
  where
    run m w = do
        (a, w') <- WS.runWriterT m
        return (a, w `mappend` w')
{-# INLINABLE runWriterSC #-}

-- | Execute 'WS.WriterT' in the base monad
--
-- Since 1.0.11
execWriterSC
  :: (Monad m, Monoid w) =>
     ConduitM i o (WS.WriterT w m) r -> ConduitM i o m w
execWriterSC p = fmap snd $ runWriterSC p
{-# INLINABLE execWriterSC #-}


-- | Wrap the base monad in 'RWSL.RWST'
--
-- Since 1.0.11
rwsC
  :: (Monad m, Monad (t1 (RWSL.RWST t w t2 m)), MonadTrans t1,
      Monoid w, MFunctor t1) =>
     (t -> t2 -> t1 m (b, t2, w)) -> t1 (RWSL.RWST t w t2 m) b
rwsC k = do
    i <- lift RWSL.ask
    s <- lift RWSL.get
    (r, s', w) <- hoist lift (k i s)
    lift $ do
        RWSL.put s'
        RWSL.tell w
    return r
{-# INLINABLE rwsC #-}

-- | Run 'RWSL.RWST' in the base monad
--
-- Since 1.0.11
runRWSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitM i o (RWSL.RWST r w s m) res
     -> ConduitM i o m (res, s, w)
runRWSC r s0 = thread toRes run (s0, mempty)
  where
    toRes a (s, w) = (a, s, w)
    run m (s, w) = do
        (res, s', w') <- RWSL.runRWST m r s
        return (res, (s', w `mappend` w'))
{-# INLINABLE runRWSC #-}

-- | Evaluate 'RWSL.RWST' in the base monad
--
-- Since 1.0.11
evalRWSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitM i o (RWSL.RWST r w s m) res
     -> ConduitM i o m (res, w)
evalRWSC i s p = fmap f $ runRWSC i s p
  where f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSC #-}

-- | Execute 'RWSL.RWST' in the base monad
--
-- Since 1.0.11
execRWSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitM i o (RWSL.RWST r w s m) res
     -> ConduitM i o m (s, w)
execRWSC i s p = fmap f $ runRWSC i s p
  where f x = let (_, s2, w2) = x in (s2, w2)
{-# INLINABLE execRWSC #-}


-- | Wrap the base monad in 'RWSS.RWST'
--
-- Since 1.0.11
rwsSC
  :: (Monad m, Monad (t1 (RWSS.RWST t w t2 m)), MonadTrans t1,
      Monoid w, MFunctor t1) =>
     (t -> t2 -> t1 m (b, t2, w)) -> t1 (RWSS.RWST t w t2 m) b
rwsSC k = do
    i <- lift RWSS.ask
    s <- lift RWSS.get
    (r, s', w) <- hoist lift (k i s)
    lift $ do
        RWSS.put s'
        RWSS.tell w
    return r
{-# INLINABLE rwsSC #-}

-- | Run 'RWSS.RWST' in the base monad
--
-- Since 1.0.11
runRWSSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitM i o (RWSS.RWST r w s m) res
     -> ConduitM i o m (res, s, w)
runRWSSC r s0 = thread toRes run (s0, mempty)
  where
    toRes a (s, w) = (a, s, w)
    run m (s, w) = do
        (res, s', w') <- RWSS.runRWST m r s
        return (res, (s', w `mappend` w'))
{-# INLINABLE runRWSSC #-}

-- | Evaluate 'RWSS.RWST' in the base monad
--
-- Since 1.0.11
evalRWSSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitM i o (RWSS.RWST r w s m) res
     -> ConduitM i o m (res, w)
evalRWSSC i s p = fmap f $ runRWSSC i s p
  where f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSSC #-}

-- | Execute 'RWSS.RWST' in the base monad
--
-- Since 1.0.11
execRWSSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitM i o (RWSS.RWST r w s m) res
     -> ConduitM i o m (s, w)
execRWSSC i s p = fmap f $ runRWSSC i s p
  where f x = let (_, s2, w2) = x in (s2, w2)
{-# INLINABLE execRWSSC #-}

