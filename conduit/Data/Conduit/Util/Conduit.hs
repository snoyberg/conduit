{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing and covnerting conduits. Please see
-- "Data.Conduit.Types.Conduit" for more information on the base types.
module Data.Conduit.Util.Conduit
    ( haveMore
    , conduitState
    , ConduitStateResult (..)
    , conduitIO
    , ConduitIOResult (..)
    , transConduit
      -- *** Sequencing
    , SequencedSink
    , sequenceSink
    , sequence
    , SequencedSinkResponse (..)
    ) where

import Prelude hiding (sequence)
import Control.Monad.Trans.Resource
import Data.Conduit.Types.Conduit
import Data.Conduit.Types.Sink
import Data.Conduit.Types.Source
import Data.Conduit.Util.Source
import Control.Monad (liftM)
import Data.Monoid (mempty)

haveMore :: Monad m => ConduitResult a m b -> m () -> [b] -> ConduitResult a m b
haveMore res _ [] = res
haveMore res close (x:xs) = HaveMore (return $ haveMore res close xs) close x

haveMoreM :: Monad m => m (ConduitResult a m b) -> m () -> [b] -> m (ConduitResult a m b)
haveMoreM res _ [] = res
haveMoreM res close (x:xs) = return $ HaveMore (haveMoreM res close xs) close x

-- | A helper type for @conduitState@, indicating the result of being pushed
-- to.  It can either indicate that processing is done, or to continue with the
-- updated state.
--
-- Since 0.2.0
data ConduitStateResult state input output =
    StateFinished (Maybe input) [output]
  | StateProducing state [output]

instance Functor (ConduitStateResult state input) where
    fmap f (StateFinished a b) = StateFinished a (map f b)
    fmap f (StateProducing a b) = StateProducing a (map f b)

-- | Construct a 'Conduit' with some stateful functions. This function addresses
-- threading the state value for you.
--
-- Since 0.2.0
conduitState
    :: Monad m
    => state -- ^ initial state
    -> (state -> input -> m (ConduitStateResult state input output)) -- ^ Push function.
    -> (state -> m [output]) -- ^ Close function. The state need not be returned, since it will not be used again.
    -> Conduit input m output
conduitState state0 push0 close0 =
    Conduit (push state0) (close state0)
  where
    push state input = liftM goRes' $ state `seq` push0 state input

    close state = SourceM (do
        os <- close0 state
        return $ fromList os) (return ())

    goRes' (StateFinished leftover output) = haveMore
        (Finished leftover)
        (return ())
        output
    goRes' (StateProducing state output) = haveMore
        (Running (push state) (close state))
        (return ())
        output

-- | A helper type for @conduitIO@, indicating the result of being pushed to.
-- It can either indicate that processing is done, or to continue.
--
-- Since 0.2.0
data ConduitIOResult input output =
    IOFinished (Maybe input) [output]
  | IOProducing [output]

instance Functor (ConduitIOResult input) where
    fmap f (IOFinished a b) = IOFinished a (map f b)
    fmap f (IOProducing b) = IOProducing (map f b)

-- | Construct a 'Conduit'.
--
-- Since 0.2.0
conduitIO :: MonadResource m
           => IO state -- ^ resource and/or state allocation
           -> (state -> IO ()) -- ^ resource and/or state cleanup
           -> (state -> input -> m (ConduitIOResult input output)) -- ^ Push function. Note that this need not explicitly perform any cleanup.
           -> (state -> m [output]) -- ^ Close function. Note that this need not explicitly perform any cleanup.
           -> Conduit input m output
conduitIO alloc cleanup push0 close0 = Conduit
    { conduitPush = \input -> do
        (key, state) <- allocate alloc cleanup
        push key state input
    , conduitClose = SourceM (do
        (key, state) <- allocate alloc cleanup
        os <- close0 state
        release key
        return $ fromList os) (return ())
    }
  where
    push key state input = do
        res <- push0 state input
        case res of
            IOProducing output -> return $ haveMore
                (Running (push key state) (close key state))
                (release key >> return ())
                output
            IOFinished leftover output -> do
                release key
                return $ haveMore
                    (Finished leftover)
                    (return ())
                    output

    close key state = SourceM (do
        output <- close0 state
        release key
        return $ fromList output) (release key)

fromList :: Monad m => [a] -> Source m a
fromList [] = Closed
fromList (x:xs) = Open (fromList xs) (return ()) x

-- | Transform the monad a 'Conduit' lives in.
--
-- See @transSource@ for more information.
--
-- Since 0.2.0
transConduit :: Monad m
             => (forall a. m a -> n a)
             -> Conduit input m output
             -> Conduit input n output
transConduit f c = c
    { conduitPush = f . liftM (transConduitPush f) . conduitPush c
    , conduitClose = transSource f (conduitClose c)
    }

transConduitPush :: Monad m
                 => (forall a. m a -> n a)
                 -> ConduitResult input m output
                 -> ConduitResult input n output
transConduitPush _ (Finished a) = Finished a
transConduitPush f (Running push close) = Running
    (f . liftM (transConduitPush f) . push)
    (transSource f close)
transConduitPush f (HaveMore pull close output) = HaveMore
    (f $ liftM (transConduitPush f) pull)
    (f close)
    output

-- | Return value from a 'SequencedSink'.
--
-- Since 0.2.0
data SequencedSinkResponse state input m output =
    Emit state [output] -- ^ Set a new state, and emit some new output.
  | Stop -- ^ End the conduit.
  | StartConduit (Conduit input m output) -- ^ Pass control to a new conduit.

-- | Helper type for constructing a @Conduit@ based on @Sink@s. This allows you
-- to write higher-level code that takes advantage of existing conduits and
-- sinks, and leverages a sink's monadic interface.
--
-- Since 0.2.0
type SequencedSink state input m output =
    state -> Sink input m (SequencedSinkResponse state input m output)

-- | Convert a 'SequencedSink' into a 'Conduit'.
--
-- Since 0.2.0
sequenceSink
    :: Monad m
    => state -- ^ initial state
    -> SequencedSink state input m output
    -> Conduit input m output
sequenceSink state0 fsink = Conduit (scPush fsink $ fsink state0) mempty

scPush :: Monad m
       => SequencedSink state input m output
       -> Sink input m (SequencedSinkResponse state input m output)
       -> ConduitPush input m output
scPush fsink (SinkData pushI _) input = pushI input >>= scGoRes fsink
scPush fsink (SinkNoData res) input = scGoRes fsink (Done (Just input) res)
scPush fsink (SinkLift msink) input = do
    sink <- msink
    scPush fsink sink input

scGoRes :: Monad m
        => SequencedSink state input m output
        -> SinkResult input m (SequencedSinkResponse state input m output)
        -> m (ConduitResult input m output)
scGoRes fsink (Done (Just leftover) (Emit state os)) = haveMoreM
    (scPush fsink (fsink state) leftover)
    (return ())
    os
scGoRes fsink (Done Nothing (Emit state os)) = return $ haveMore
    (Running p c)
    (return ())
    os
  where
    Conduit p c = sequenceSink state fsink
scGoRes fsink (Processing pushI closeI) = return $ Running
    (scPush fsink (SinkData pushI closeI))
    (SourceM (do
        res <- closeI
        case res of
            Emit _ os -> return $ fromList os
            Stop -> return Closed
            StartConduit (Conduit _ closeC) -> return closeC)
        (closeI >> return ()))
scGoRes _ (Done mleftover Stop) = return $ Finished mleftover
scGoRes _ (Done Nothing (StartConduit (Conduit p c))) = return $ Running p c
scGoRes _ (Done (Just leftover) (StartConduit (Conduit p _))) = p leftover

-- | Specialised version of 'sequenceSink'
--
-- Note that this function will return an infinite stream if provided a
-- @SinkNoData@ constructor. In other words, you probably don\'t want to do
-- @sequence . return@.
--
-- Since 0.2.1
sequence :: Monad m => Sink input m output -> Conduit input m output
sequence (SinkData spush0 sclose0) =
    Conduit (push spush0) (close sclose0)
  where
    push spush input = spush input >>= goRes

    goRes res =
        case res of
            Processing spush'' sclose'' ->
                return $ Running (push spush'') (close sclose'')
            Done Nothing output -> return $ HaveMore
                (return $ Running (push spush0) (close sclose0))
                (return ())
                output
            Done (Just input') output -> return $ HaveMore
                (spush0 input' >>= goRes)
                (return ())
                output

    close sclose = SourceM (do
        output <- sclose
        return $ Open Closed (return ()) output) (return ())

sequence (SinkNoData output) = Conduit
    { conduitPush = \_input ->
        let x = return $ HaveMore x (return ()) output
         in x
    , conduitClose =
        let src = Open src (return ()) output
         in src
    }
sequence (SinkLift msink) = Conduit
    { conduitPush = \input -> do
        sink <- msink
        conduitPush (sequence sink) input
    , conduitClose = mempty
    }
