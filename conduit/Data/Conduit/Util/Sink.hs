{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing 'Sink's. Please see "Data.Conduit.Types.Sink"
-- for more information on the base types.
module Data.Conduit.Util.Sink
    ( sinkState
    , sinkIO
    , transSink
    ) where

import Control.Monad.Trans.Resource
--import Control.Monad.Trans.Class (lift)
import Data.Conduit.Types.Sink
--import Control.Monad (liftM)

-- | Construct a 'Sink' with some stateful functions. This function address
-- all mutable state for you.
--
-- Since 0.0.0
sinkState
    :: Resource m
    => state -- ^ initial state
    -> (state -> input -> ResourceT m (Either state (Maybe input, output))) -- ^ push
    -> (state -> ResourceT m output) -- ^ Close. Note that the state is not returned, as it is not needed.
    -> Sink input m output
sinkState state0 push0 close0 =
    Sink $ return $ SinkData (push state0) (close0 state0)
  where
    push state input = do
        res <- state `seq` push0 state input
        case res of
            Left state' -> return $ Processing (push state') (close0 state')
            Right (mleftover, output) -> return $ Done mleftover output

-- | Construct a 'Sink'. Note that your push and close functions need not
-- explicitly perform any cleanup.
--
-- Since 0.0.0
sinkIO :: ResourceIO m
        => IO state -- ^ resource and/or state allocation
        -> (state -> IO ()) -- ^ resource and/or state cleanup
        -> (state -> input -> m (SinkResult input m output)) -- ^ push
        -> (state -> m output) -- ^ close
        -> Sink input m output
sinkIO = error "sinkIO" {- FIXME
sinkIO alloc cleanup push close = Sink $ do
    (key, state) <- withIO alloc cleanup
#if DEBUG
    iclosed <- newRef False
#endif
    return SinkData
        { sinkPush = \input -> do
#if DEBUG
            False <- readRef iclosed
#endif
            res <- lift $ push state input
            case res of
                Done{} -> do
                    release key
#if DEBUG
                    writeRef iclosed True
#endif
                Processing{} -> return ()
            return res
        , sinkClose = do
#if DEBUG
            False <- readRef iclosed
            writeRef iclosed True
#endif
            res <- lift $ close state
            release key
            return res
        }
    -}

-- | Transform the monad a 'Sink' lives in.
--
-- Since 0.0.0
transSink :: (Base m ~ Base n, Monad m)
           => (forall a. m a -> n a)
           -> Sink input m output
           -> Sink input n output
transSink _f (Sink _mc) =
    error "transSink"
    {-
    Sink (transResourceT f (liftM go mc))
  where
    go c = c
        { sinkPush = transResourceT f . sinkPush c
        , sinkClose = transResourceT f (sinkClose c)
        }
    -}
