{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Utilities for constructing 'SinkM's. Please see "Data.Conduit.Types.Sink"
-- for more information on the base types.
module Data.Conduit.Util.Sink
    ( sinkM
    , sinkMState
    , transSinkM
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Types.Sink
import Control.Monad (liftM)

-- | Construct a 'SinkM' with some stateful functions. This function address
-- all mutable state for you.
sinkMState
    :: Resource m
    => state -- ^ initial state
    -> (state -> [input] -> ResourceT m (state, Maybe (SinkResult input output))) -- ^ push
    -> (state -> [input] -> ResourceT m (SinkResult input output)) -- ^ Close. Note that the state is not returned, as it is not needed.
    -> SinkM input m output
sinkMState state0 push close = sinkM
    (newRef state0)
    (const $ return ())
    (\istate input -> do
        state <- readRef istate
        (state', res) <- push state input
        writeRef istate state'
        return res)
    (\istate input -> readRef istate >>= flip close input)

-- | Construct a 'SinkM'. Note that your push and close functions need not
-- explicitly perform any cleanup.
sinkM :: Monad m
      => ResourceT m state -- ^ resource and/or state allocation
      -> (state -> ResourceT m ()) -- ^ resource and/or state cleanup
      -> (state -> [input] -> ResourceT m (Maybe (SinkResult input output))) -- ^ push
      -> (state -> [input] -> ResourceT m (SinkResult input output)) -- ^ close
      -> SinkM input m output
sinkM alloc cleanup push close = SinkM $ do
    state <- alloc
    return SinkData
        { sinkPush = \input -> do
            res <- push state input
            maybe (return ()) (const $ cleanup state) res
            return res
        , sinkClose = \input -> do
            res <- close state input
            cleanup state
            return res
        }

-- | Transform the monad a 'SinkM' lives in.
transSinkM :: (Base m ~ Base n, Monad m)
           => (forall a. m a -> n a)
           -> SinkM input m output
           -> SinkM input n output
transSinkM f (SinkM mc) =
    SinkM (transResourceT f (liftM go mc))
  where
    go c = c
        { sinkPush = transResourceT f . sinkPush c
        , sinkClose = transResourceT f . (sinkClose c)
        }
