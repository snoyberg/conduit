{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Utilities for constructing and covnerting conduits. Please see
-- "Data.Conduit.Types.Conduit" for more information on the base types.
module Data.Conduit.Util.Conduit
    ( conduitMIO
    , conduitMState
    , transConduitM
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class
import Data.Conduit.Types.Conduit
import Data.Conduit.Types.Sink (Result (..))
import Control.Monad (liftM)

-- | Construct a 'ConduitM' with some stateful functions. This function address
-- all mutable state for you.
conduitMState
    :: Resource m
    => state -- ^ initial state
    -> (state -> [input] -> ResourceT m (state, ConduitResult (Result [input]) output)) -- ^ Push function.
    -> (state -> [input] -> ResourceT m (ConduitResult [input] output)) -- ^ Close function. The state need not be returned, since it will not be used again.
    -> ConduitM input m output
conduitMState state0 push close = ConduitM $ do
    istate <- newRef state0
    return Conduit
        { conduitPush = \input -> do
            state <- readRef istate
            (state', res) <- push state input
            writeRef istate state'
            return res
        , conduitClose = \input -> readRef istate >>= flip close input
        }

-- | Construct a 'ConduitM'.
conduitMIO :: ResourceIO m
           => IO state -- ^ resource and/or state allocation
           -> (state -> IO ()) -- ^ resource and/or state cleanup
           -> (state -> [input] -> m (ConduitResult (Result [input]) output)) -- ^ Push function. Note that this need not explicitly perform any cleanup.
           -> (state -> [input] -> m (ConduitResult [input] output)) -- ^ Close function. Note that this need not explicitly perform any cleanup.
           -> ConduitM input m output
conduitMIO alloc cleanup push close = ConduitM $ do
    (key, state) <- withIO alloc cleanup
    return Conduit
        { conduitPush = \input -> do
            res@(ConduitResult mleft _) <- lift $ push state input
            case mleft of
                Processing -> return ()
                Done _ -> release key
            return res
        , conduitClose = \input -> do
            output <- lift $ close state input
            release key
            return output
        }

-- | Transform the monad a 'ConduitM' lives in.
transConduitM :: (Monad m, Base m ~ Base n)
              => (forall a. m a -> n a)
              -> ConduitM input m output
              -> ConduitM input n output
transConduitM f (ConduitM mc) =
    ConduitM (transResourceT f (liftM go mc))
  where
    go c = c
        { conduitPush = transResourceT f . conduitPush c
        , conduitClose = transResourceT f . conduitClose c
        }
