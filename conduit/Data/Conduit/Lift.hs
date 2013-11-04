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
    catchError,
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

    -- * WriterT
    -- $writert
    writerC,
    runWriterC,
    execWriterC,

    -- * RWST
    rwsC,
    runRWSC,
    evalRWSC,
    execRWSC,

    distribute
    ) where

import Data.Conduit

import Control.Monad.Morph (hoist, lift, MFunctor(..), )
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Monoid (Monoid(..))


import qualified Control.Monad.Trans.Error as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Control.Monad.Trans.RWS.Strict as RWS


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
errorC
  :: (Monad m, Monad (t (E.ErrorT e m)), MonadTrans t, E.Error e,
      MFunctor t) =>
     t m (Either e b) -> t (E.ErrorT e m) b
errorC p = do
    x <- hoist lift p
    lift $ E.ErrorT (return x)

-- | Run 'E.ErrorT' in the base monad
runErrorC
  :: (Monad m, E.Error e) =>
     ConduitM b o (E.ErrorT e m) () -> ConduitM b o m (Either e ())
runErrorC    = E.runErrorT . distribute
{-# INLINABLE runErrorC #-}

-- | Catch an error in the base monad
catchError
  :: (Monad m, E.Error e) =>
     ConduitM i o (E.ErrorT e m) ()
     -> (e -> ConduitM i o (E.ErrorT e m) ())
     -> ConduitM i o (E.ErrorT e m) ()
catchError e h = errorC $ E.runErrorT $
    E.catchError (distribute e) (distribute . h)
{-# INLINABLE catchError #-}

-- | Wrap the base monad in 'M.MaybeT'
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
runMaybeC
  :: Monad m =>
     ConduitM b o (M.MaybeT m) () -> ConduitM b o m (Maybe ())
runMaybeC p = M.runMaybeT $ distribute p
{-# INLINABLE runMaybeC #-}

-- | Wrap the base monad in 'R.ReaderT'
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
runReaderC
  :: Monad m =>
     r -> ConduitM b o (R.ReaderT r m) () -> ConduitM b o m ()
runReaderC r p = (`R.runReaderT` r) $ distribute p
{-# INLINABLE runReaderC #-}


-- | Wrap the base monad in 'S.StateT'
stateC
  :: (Monad m, Monad (t1 (S.StateT t m)),
      MonadTrans t1,
      MFunctor t1) =>
     (t -> t1 m (b, t)) -> t1 (S.StateT t m) b
stateC k = do
    s <- lift S.get
    (r, s') <- hoist lift (k s)
    lift (S.put s')
    return r
{-# INLINABLE stateC #-}

-- | Run 'S.StateT' in the base monad
runStateC
  :: Monad m =>
     s -> ConduitM b o (S.StateT s m) () -> ConduitM b o m ((), s)
runStateC s p = (`S.runStateT` s) $ distribute p
{-# INLINABLE runStateC #-}

-- | Evaluate 'S.StateT' in the base monad
evalStateC
  :: Monad m =>
     b -> ConduitM b1 o (S.StateT b m) () -> ConduitM b1 o m ()
evalStateC s p = fmap fst $ runStateC s p
{-# INLINABLE evalStateC #-}

-- | Execute 'S.StateT' in the base monad
execStateC
  :: Monad m =>
     b -> ConduitM b1 o (S.StateT b m) () -> ConduitM b1 o m b
execStateC s p = fmap snd $ runStateC s p
{-# INLINABLE execStateC #-}


-- | Wrap the base monad in 'W.WriterT'
writerC
  :: (Monad m, Monad (t (W.WriterT w m)), MonadTrans t, Monoid w,
      MFunctor t) =>
     t m (b, w) -> t (W.WriterT w m) b
writerC p = do
    (r, w) <- hoist lift p
    lift $ W.tell w
    return r
{-# INLINABLE writerC #-}

-- | Run 'W.WriterT' in the base monad
runWriterC
  :: (Monad m, Monoid w) =>
     ConduitM b o (W.WriterT w m) () -> ConduitM b o m ((), w)
runWriterC p = W.runWriterT $ distribute p
{-# INLINABLE runWriterC #-}

-- | Execute 'W.WriterT' in the base monad
execWriterC
  :: (Monad m, Monoid b) =>
     ConduitM b1 o (W.WriterT b m) () -> ConduitM b1 o m b
execWriterC p = fmap snd $ runWriterC p
{-# INLINABLE execWriterC #-}


-- | Wrap the base monad in 'RWS.RWST'
rwsC
  :: (Monad m, Monad (t1 (RWS.RWST t w t2 m)), MonadTrans t1,
      Monoid w, MFunctor t1) =>
     (t -> t2 -> t1 m (b, t2, w)) -> t1 (RWS.RWST t w t2 m) b
rwsC k = do
    i <- lift RWS.ask
    s <- lift RWS.get
    (r, s', w) <- hoist lift (k i s)
    lift $ do
        RWS.put s'
        RWS.tell w
    return r
{-# INLINABLE rwsC #-}

-- | Run 'RWS.RWST' in the base monad
runRWSC
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> ConduitM b o (RWS.RWST r w s m) ()
     -> ConduitM b o m ((), s, w)
runRWSC  i s p = (\b -> RWS.runRWST b i s) $ distribute p
{-# INLINABLE runRWSC #-}

-- | Evaluate 'RWS.RWST' in the base monad
evalRWSC
  :: (Monad m, Monoid t1) =>
     r
     -> t
     -> ConduitM b o (RWS.RWST r t1 t m) ()
     -> ConduitM b o m ((), t1)
evalRWSC i s p = fmap f $ runRWSC i s p
  where f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSC #-}

-- | Execute 'RWS.RWST' in the base monad
execRWSC
  :: (Monad m, Monoid t1) =>
     r
     -> t
     -> ConduitM b o (RWS.RWST r t1 t m) ()
     -> ConduitM b o m (t, t1)
execRWSC i s p = fmap f $ runRWSC i s p
  where f x = let (_, s2, w2) = x in (s2, w2)
{-# INLINABLE execRWSC #-}

