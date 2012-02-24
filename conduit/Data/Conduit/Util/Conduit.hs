{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing and covnerting conduits. Please see
-- "Data.Conduit.Types.Conduit" for more information on the base types.
module Data.Conduit.Util.Conduit
    ( conduitState
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
import Control.Monad (liftM)

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
    Conduit (push state0) (close0 state0)
  where
    push state input = do
        res <- state `seq` push0 state input
        return $ case res of
            StateFinished a b -> Finished a b
            StateProducing state' output -> Producing
                (Conduit (push state') (close0 state'))
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
        (key, state) <- with alloc cleanup
        push key state input
    , conduitClose = do
        (key, state) <- with alloc cleanup
        close key state
    }
  where
    push key state input = do
        res <- push0 state input
        case res of
            IOProducing output -> return $ Producing
                (Conduit (push key state) (close key state))
                output
            IOFinished a b -> do
                release key
                return $ Finished a b
    close key state = do
        output <- close0 state
        release key
        return output

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
    , conduitClose = f (conduitClose c)
    }

transConduitPush :: Monad m
                 => (forall a. m a -> n a)
                 -> ConduitResult input m output
                 -> ConduitResult input n output
transConduitPush _ (Finished a b) = Finished a b
transConduitPush f (Producing conduit output) = Producing
    (transConduit f conduit)
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

data SCState state input m output =
    SCNewState state
  | SCConduit (Conduit input m output)
  | SCSink (input -> m (SinkResult input m (SequencedSinkResponse state input m output)))
           (m (SequencedSinkResponse state input m output))

-- | Convert a 'SequencedSink' into a 'Conduit'.
--
-- Since 0.2.0
sequenceSink
    :: Monad m
    => state -- ^ initial state
    -> SequencedSink state input m output
    -> Conduit input m output
sequenceSink state0 fsink = conduitState
    (SCNewState state0)
    (scPush id fsink)
    scClose

goRes :: Monad m
      => SequencedSinkResponse state input m output
      -> Maybe input
      -> ([output] -> [output])
      -> SequencedSink state input m output
      -> m (ConduitStateResult (SCState state input m output) input output)
goRes (Emit state output) (Just input) front fsink =
    scPush (front . (output++)) fsink (SCNewState state) input
goRes (Emit state output) Nothing front _ =
    return $ StateProducing (SCNewState state) $ front output
goRes Stop minput front _ =
    return $ StateFinished minput $ front []
goRes (StartConduit c) Nothing front _ =
    return $ StateProducing (SCConduit c) $ front []
goRes (StartConduit c) (Just input) front fsink =
    scPush front fsink (SCConduit c) input

scPush :: Monad m
     => ([output] -> [output])
     -> SequencedSink state input m output
     -> SCState state input m output
     -> input
     -> m (ConduitStateResult (SCState state input m output) input output)
scPush front fsink (SCNewState state) input =
    go (fsink state)
  where
    go (SinkData push' close') = scPush front fsink (SCSink push' close') input
    go (SinkNoData res) = goRes res (Just input) front fsink
    go (SinkLift msink) = msink >>= go
scPush front _ (SCConduit conduit) input = do
    res <- conduitPush conduit input
    return $ case res of
        Producing c x -> StateProducing (SCConduit c) $ front x
        Finished x y -> StateFinished x $ front y
scPush front fsink (SCSink push _) input = do
    mres <- push input
    case mres of
        Done minput res -> goRes res minput front fsink
        Processing push' close' -> return (StateProducing (SCSink push' close') $ front [])

scClose :: Monad m => SCState state inptu m output -> m [output]
scClose (SCNewState _) = return []
scClose (SCConduit conduit) = conduitClose conduit
scClose (SCSink _ close) = do
    res <- close
    case res of
        Emit _ os -> return os
        Stop -> return []
        StartConduit c -> conduitClose c

-- | Specialised version of 'sequenceSink'
--
-- Note that this function will return an infinite stream if provided a
-- @SinkNoData@ constructor. In other words, you probably don\'t want to do
-- @sequence . return@.
--
-- Since 0.2.1
sequence :: Monad m => Sink input m output -> Conduit input m output
sequence (SinkData spush sclose) = Conduit (push spush) (close sclose)
  where
    push spush' input = do
        res <- spush' input
        case res of
            Processing spush'' sclose'' ->
                return $ Producing (Conduit (push spush'') (close sclose'')) []
            Done Nothing output ->
                return $ Producing (Conduit (push spush) (close sclose)) [output]
            Done (Just input') output -> do
                res' <- push spush input'
                case res' of
                    Producing conduit' output' ->
                        return $ Producing conduit' (output:output')
                    Finished _ _ -> error "impossible [sequence]"
    close sclose' = liftM (:[]) sclose'

sequence (SinkNoData output) = Conduit
    { conduitPush = \input -> return $ Finished (Just input) (repeat output)
    , conduitClose = return $ repeat output
    }
sequence (SinkLift msink) = Conduit
    { conduitPush = \input -> do
        sink <- msink
        conduitPush (sequence sink) input
    , conduitClose = return []
    }
