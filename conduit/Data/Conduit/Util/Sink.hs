{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing 'Sink's. Please see "Data.Conduit.Types.Sink"
-- for more information on the base types.
module Data.Conduit.Util.Sink
    ( sinkState
    , SinkStateResult (..)
    , sinkIO
    , SinkIOResult (..)
    , transSink
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Types.Sink
import Control.Monad (liftM)

-- | A helper type for @sinkState@, indicating the result of being pushed to.
-- It can either indicate that processing is done, or to continue with the
-- updated state.
--
-- Since 0.2.0
data SinkStateResult state input output =
    StateDone (Maybe input) output
  | StateProcessing state

-- | Construct a 'Sink' with some stateful functions. This function addresses
-- threading the state value for you.
--
-- Since 0.2.0
sinkState
    :: Resource m
    => state -- ^ initial state
    -> (state -> input -> ResourceT m (SinkStateResult state input output)) -- ^ push
    -> (state -> ResourceT m output) -- ^ Close. Note that the state is not returned, as it is not needed.
    -> Sink input m output
sinkState state0 push0 close0 =
    SinkData (push state0) (close0 state0)
  where
    push state input = do
        res <- state `seq` push0 state input
        case res of
            StateProcessing state' -> return $ Processing (push state') (close0 state')
            StateDone mleftover output -> return $ Done mleftover output

-- | A helper type for @sinkIO@, indicating the result of being pushed to. It
-- can either indicate that processing is done, or to continue.
--
-- Since 0.2.0
data SinkIOResult input output = IODone (Maybe input) output | IOProcessing

-- | Construct a 'Sink'. Note that your push and close functions need not
-- explicitly perform any cleanup.
--
-- Since 0.2.0
sinkIO :: ResourceIO m
        => IO state -- ^ resource and/or state allocation
        -> (state -> IO ()) -- ^ resource and/or state cleanup
        -> (state -> input -> m (SinkIOResult input output)) -- ^ push
        -> (state -> m output) -- ^ close
        -> Sink input m output
sinkIO alloc cleanup push0 close0 = SinkData
    { sinkPush = \input -> do
        (key, state) <- withIO alloc cleanup
        push key state input
    , sinkClose = do
        (key, state) <- withIO alloc cleanup
        close key state
    }
  where
    push key state input = do
        res <- lift $ push0 state input
        case res of
            IODone a b -> do
                release key
                return $ Done a b
            IOProcessing -> return $ Processing
                (push key state)
                (close key state)
    close key state = do
        res <- lift $ close0 state
        release key
        return res

-- | Transform the monad a 'Sink' lives in.
--
-- See @transSource@ for more information.
--
-- Since 0.2.0
transSink :: (Base m ~ Base n, Monad m)
          => (forall a. m a -> n a)
          -> Sink input m output
          -> Sink input n output
transSink _ (SinkNoData x) = SinkNoData x
transSink f (SinkLift msink) = SinkLift (transResourceT f (liftM (transSink f) msink))
transSink f (SinkData push close) = SinkData
    (transResourceT f . fmap (transSinkPush f) . push)
    (transResourceT f close)

transSinkPush :: (Base m ~ Base n, Monad m)
              => (forall a. m a -> n a)
              -> SinkResult input m output
              -> SinkResult input n output
transSinkPush _ (Done a b) = Done a b
transSinkPush f (Processing push close) = Processing
    (transResourceT f . fmap (transSinkPush f) . push)
    (transResourceT f close)
