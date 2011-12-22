{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Utilities for constructing 'Sink's. Please see "Data.Conduit.Types.Sink"
-- for more information on the base types.
module Data.Conduit.Util.Sink
    ( sinkIO
    , sinkState
    , transSink
    , result
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Types.Sink
import Control.Monad (liftM)

-- | Construct a 'Sink' with some stateful functions. This function address
-- all mutable state for you.
sinkState
    :: Resource m
    => state -- ^ initial state
    -> (state -> [input] -> ResourceT m (state, Result (SinkResult input output))) -- ^ push
    -> (state -> [input] -> ResourceT m (SinkResult input output)) -- ^ Close. Note that the state is not returned, as it is not needed.
    -> Sink input m output
sinkState state0 push close = Sink $ do
    istate <- newRef state0
    return SinkData
        { sinkPush = \input -> do
            state <- readRef istate
            (state', res) <- push state input
            writeRef istate state'
            return res
        , sinkClose = \input -> readRef istate >>= flip close input
        }

-- | Construct a 'Sink'. Note that your push and close functions need not
-- explicitly perform any cleanup.
sinkIO :: ResourceIO m
        => IO state -- ^ resource and/or state allocation
        -> (state -> IO ()) -- ^ resource and/or state cleanup
        -> (state -> [input] -> m (Result (SinkResult input output))) -- ^ push
        -> (state -> [input] -> m (SinkResult input output)) -- ^ close
        -> Sink input m output
sinkIO alloc cleanup push close = Sink $ do
    (key, state) <- withIO alloc cleanup
    return SinkData
        { sinkPush = \input -> do
            res <- lift $ push state input
            result (return ()) (const $ release key) res
            return res
        , sinkClose = \input -> do
            res <- lift $ close state input
            release key
            return res
        }

-- | Transform the monad a 'Sink' lives in.
transSink :: (Base m ~ Base n, Monad m)
           => (forall a. m a -> n a)
           -> Sink input m output
           -> Sink input n output
transSink f (Sink mc) =
    Sink (transResourceT f (liftM go mc))
  where
    go c = c
        { sinkPush = transResourceT f . sinkPush c
        , sinkClose = transResourceT f . (sinkClose c)
        }

result :: b -> (a -> b) -> Result a -> b
result b _ Processing = b
result _ f (Done a) = f a
