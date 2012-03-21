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
    , sinkClose
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Types.Sink
import Control.Monad (liftM)

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
    Processing (push state0) (close0 state0)
  where
    push state input = SinkM $ do
        res <- state `seq` push0 state input
        case res of
            StateProcessing state' -> return $ Processing (push state') (close0 state')
            StateDone mleftover output -> return $ Done mleftover output

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
sinkIO alloc cleanup push0 close0 = Processing
    (\input -> SinkM $ do
        (key, state) <- allocate alloc cleanup
        push key state input)
    (do
        (key, state) <- allocate alloc cleanup
        close key state)
  where
    push key state input = do
        res <- push0 state input
        case res of
            IODone a b -> do
                release key
                return $ Done a b
            IOProcessing -> return $ Processing
                (SinkM . push key state)
                (close key state)
    close key state = do
        res <- close0 state
        release key
        return res

-- | Transform the monad a 'Sink' lives in.
--
-- See @transSource@ for more information.
--
-- Since 0.3.0
transSink :: Monad m
          => (forall a. m a -> n a)
          -> Sink input m output
          -> Sink input n output
transSink _ (Done a b) = Done a b
transSink f (Processing push close) = Processing (transSink f . push) (f close)
transSink f (SinkM msink) = SinkM (f (liftM (transSink f) msink))

-- | Close a @Sink@ if it is still open, discarding any output it produces.
--
-- Since 0.3.0
sinkClose :: Monad m => Sink input m output -> m ()
sinkClose (SinkM msink) = msink >>= sinkClose
sinkClose Done{} = return ()
sinkClose (Processing _ close) = close >> return ()
