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
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Internal

-- | A helper type for @sinkState@, indicating the result of being pushed to.
-- It can either indicate that processing is done, or to continue with the
-- updated state.
--
-- Since 0.3.0
data SinkStateResult state input output =
    StateDone (Maybe input) output
  | StateProcessing state

-- | Construct a 'Sink' with some stateful functions. This function addresses
-- threading the state value for you.
--
-- Since 0.3.0
sinkState
    :: Monad m
    => state -- ^ initial state
    -> (state -> input -> m (SinkStateResult state input output)) -- ^ push
    -> (state -> m output) -- ^ Close. Note that the state is not returned, as it is not needed.
    -> Sink input m output
sinkState state0 push0 close0 =
    NeedInput (push state0) (close state0)
  where
    push state input = PipeM
        (do
            res <- state `seq` push0 state input
            case res of
                StateProcessing state' -> return $ NeedInput (push state') (close state')
                StateDone mleftover output -> return $ Done mleftover output)
        (close0 state)

    close = lift . close0

-- | A helper type for @sinkIO@, indicating the result of being pushed to. It
-- can either indicate that processing is done, or to continue.
--
-- Since 0.3.0
data SinkIOResult input output = IODone (Maybe input) output | IOProcessing

-- | Construct a 'Sink'. Note that your push and close functions need not
-- explicitly perform any cleanup.
--
-- Since 0.3.0
sinkIO :: MonadResource m
       => IO state -- ^ resource and/or state allocation
       -> (state -> IO ()) -- ^ resource and/or state cleanup
       -> (state -> input -> m (SinkIOResult input output)) -- ^ push
       -> (state -> m output) -- ^ close
       -> Sink input m output
sinkIO alloc cleanup push0 close0 = NeedInput
    (\input -> PipeM (do
        (key, state) <- allocate alloc cleanup
        push key state input) (do
            (key, state) <- allocate alloc cleanup
            close key state))
    (do
        (key, state) <- lift $ allocate alloc cleanup
        lift $ close key state)
  where
    push key state input = do
        res <- push0 state input
        case res of
            IODone a b -> do
                release key
                return $ Done a b
            IOProcessing -> return $ NeedInput
                (\i ->
                    let mpipe = push key state i
                     in PipeM mpipe (mpipe >>= pipeClose))
                (lift $ close key state)
    close key state = do
        res <- close0 state
        release key
        return res
