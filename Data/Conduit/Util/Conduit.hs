{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Utilities for constructing and covnerting conduits. Please see
-- "Data.Conduit.Types.Conduit" for more information on the base types.
module Data.Conduit.Util.Conduit
    ( conduitM
    , conduitMState
    , transConduitM
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Types.Conduit
import Data.Conduit.Types.Source (StreamState (..))
import Control.Monad (liftM)

-- | Construct a 'ConduitM' with some stateful functions. This function address
-- all mutable state for you.
conduitMState
    :: Resource m
    => state -- ^ initial state
    -> (state -> [input] -> ResourceT m (state, ConduitResult (Maybe [input]) output)) -- ^ Push function.
    -> (state -> [input] -> ResourceT m (ConduitResult [input] output)) -- ^ Close function. The state need not be returned, since it will not be used again.
    -> ConduitM input m output
conduitMState state0 push close = conduitM
    (newRef state0)
    (const $ return ())
    (\istate input -> do
        state <- readRef istate
        (state', res) <- push state input
        writeRef istate state'
        return res)
    (\istate input -> readRef istate >>= flip close input)

-- | Construct a 'ConduitM'.
conduitM :: Monad m
         => ResourceT m state -- ^ resource and/or state allocation
         -> (state -> ResourceT m ()) -- ^ resource and/or state cleanup
         -> (state -> [input] -> ResourceT m (ConduitResult (Maybe [input]) output)) -- ^ Push function. Note that this need not explicitly perform any cleanup.
         -> (state -> [input] -> ResourceT m (ConduitResult [input] output)) -- ^ Close function. Note that this need not explicitly perform any cleanup.
         -> ConduitM input m output
conduitM alloc cleanup push close = ConduitM $ do
    state <- alloc
    return Conduit
        { conduitPush = \input -> do
            res@(ConduitResult mleft _) <- push state input
            case mleft of
                Nothing -> return ()
                Just _ -> cleanup state
            return res
        , conduitClose = \input -> do
            output <- close state input
            cleanup state
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
