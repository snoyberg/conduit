{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Defines the types for a source, which is a producer of data.
module Data.Conduit.Types
    ( Source
    , Sink
    , Conduit
    , Pipe (..)
    , pipeClose
    , pipe
    , runPipe
    ) where

import Control.Applicative (Applicative (..), (<|>), (<$>))
import Control.Monad ((>=>), liftM, ap)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Base (MonadBase (liftBase))
import Data.Void (Void, absurd)
import Data.Monoid (Monoid (mappend, mempty))

data Pipe i o m r =
    HaveOutput (Pipe i o m r) (m r) o
  | NeedInput (i -> Pipe i o m r) (Pipe () o m r)
  | Done (Maybe i) r
  | PipeM (m (Pipe i o m r)) (m r)

pipeClose :: Monad m => Pipe i o m r -> m r
pipeClose = liftM snd . pipeCloseL

pipeCloseL :: Monad m => Pipe i o m r -> m (Maybe i, r)
pipeCloseL (HaveOutput _ c _) = ((,) Nothing) `liftM` c
pipeCloseL (NeedInput _ p)= ((,) Nothing) `liftM` pipeClose p
pipeCloseL (Done l r) = return (l, r)
pipeCloseL (PipeM _ c) = ((,) Nothing) `liftM` c

noInput :: Monad m => Pipe i o m r -> Pipe () o m r
noInput (HaveOutput p r o) = HaveOutput (noInput p) r o
noInput (NeedInput _ c) = c
noInput (Done _ r) = Done Nothing r
noInput (PipeM mp c) = PipeM (noInput `liftM` mp) c

pipePush :: Monad m => i -> Pipe i o m r -> Pipe i o m r
pipePush i (HaveOutput p c o) = HaveOutput (pipePush i p) c o
pipePush i (NeedInput p _) = p i
pipePush i (Done _ r) = Done (Just i) r
pipePush i (PipeM mp c) = PipeM (pipePush i `liftM` mp) c

type Source m a = Pipe () a m ()
type Sink i m o = Pipe i Void m o
type Conduit i m o = Pipe i o m ()

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

pipe :: Monad m => Pipe a b m () -> Pipe b c m r -> Pipe a c m r
pipe l r = snd `liftM` pipeL l r

pipeL :: Monad m => Pipe a b m () -> Pipe b c m r -> Pipe a c m (Maybe b, r)

-- Simplest case: both pipes are done. Discard the right pipe's leftovers.
pipeL (Done leftoverl ()) (Done leftoverr r) = Done leftoverl (leftoverr, r)

-- Left pipe needs to run a monadic action.
pipeL (PipeM mp c) right = PipeM
    ((`pipeL` right) `liftM` mp)
    (c >> pipeCloseL right)

-- Left pipe needs more input, ask for it.
pipeL (NeedInput p c) right = NeedInput
    (\a -> pipeL (p a) right)
    (pipeL c right)

-- Left pipe has output, right pipe wants it.
pipeL (HaveOutput lp _ a) (NeedInput rp _) = pipeL lp (rp a)

-- Right pipe needs to run a monadic action.
pipeL left (PipeM mp c) = PipeM
    (pipeL left `liftM` mp)
    (pipeClose left >> ((,) Nothing) `liftM` c)

-- Right pipe has some output, provide it downstream and continue.
pipeL left (HaveOutput p c o) = HaveOutput
    (pipeL left p)
    (pipeClose left >> ((,) Nothing) `liftM` c)
    o

-- Left pipe is done, right pipe needs input. In such a case, tell the right
-- pipe there is no more input, and eventually replace its leftovers with the
-- left pipe's leftover.
pipeL (Done l ()) (NeedInput _ c) = ((,) Nothing) `liftM` replaceLeftover l c

-- Left pipe has more output, but right pipe doesn't want it.
pipeL (HaveOutput _ c _) (Done leftoverr r) = PipeM
    (c >> return (Done Nothing (leftoverr, r)))
    (c >> return (leftoverr, r))

replaceLeftover :: Monad m => Maybe i -> Pipe () o m r -> Pipe i o m r
replaceLeftover l (Done _ r) = Done l r
replaceLeftover l (HaveOutput p c o) = HaveOutput (replaceLeftover l p) c o

-- This function is only called on pipes when there is no more input available.
-- Therefore, we can ignore the push record.
replaceLeftover l (NeedInput _ c) = replaceLeftover l c

replaceLeftover l (PipeM mp c) = PipeM (replaceLeftover l `liftM` mp) c

runPipe :: Monad m => Pipe () Void m r -> m r
runPipe (HaveOutput _ _ o) = absurd o
runPipe (NeedInput p _) = runPipe (p ())
runPipe (Done Nothing r) = return r
runPipe (Done (Just ()) r) = return r
runPipe (PipeM mp _) = mp >>= runPipe
