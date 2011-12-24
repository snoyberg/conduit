{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing and covnerting conduits. Please see
-- "Data.Conduit.Types.Conduit" for more information on the base types.
module Data.Conduit.Util.Conduit
    ( conduitIO
    , conduitState
    , transConduit
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class
import Data.Conduit.Types.Conduit
import Control.Monad (liftM)

-- | Construct a 'Conduit' with some stateful functions. This function address
-- all mutable state for you.
conduitState
    :: Resource m
    => state -- ^ initial state
    -> (state -> input -> ResourceT m (state, ConduitResult input output)) -- ^ Push function.
    -> (state -> ResourceT m [output]) -- ^ Close function. The state need not be returned, since it will not be used again.
    -> Conduit input m output
conduitState state0 push close = Conduit $ do
#if DEBUG
    iclosed <- newRef False
#endif
    istate <- newRef state0
    return PreparedConduit
        { conduitPush = \input -> do
#if DEBUG
            False <- readRef iclosed
#endif
            state <- readRef istate
            (state', res) <- push state input
            writeRef istate state'
#if DEBUG
            case res of
                Finished _ _ -> writeRef iclosed True
                Producing _ -> return ()
#endif
            return res
        , conduitClose = do
#if DEBUG
            False <- readRef iclosed
            writeRef iclosed True
#endif
            readRef istate >>= close
        }

-- | Construct a 'Conduit'.
conduitIO :: ResourceIO m
           => IO state -- ^ resource and/or state allocation
           -> (state -> IO ()) -- ^ resource and/or state cleanup
           -> (state -> input -> m (ConduitResult input output)) -- ^ Push function. Note that this need not explicitly perform any cleanup.
           -> (state -> m [output]) -- ^ Close function. Note that this need not explicitly perform any cleanup.
           -> Conduit input m output
conduitIO alloc cleanup push close = Conduit $ do
#if DEBUG
    iclosed <- newRef False
#endif
    (key, state) <- withIO alloc cleanup
    return PreparedConduit
        { conduitPush = \input -> do
#if DEBUG
            False <- readRef iclosed
#endif
            res <- lift $ push state input
            case res of
                Producing{} -> return ()
                Finished{} -> do
#if DEBUG
                    writeRef iclosed True
#endif
                    release key
            return res
        , conduitClose = do
#if DEBUG
            False <- readRef iclosed
            writeRef iclosed True
#endif
            output <- lift $ close state
            release key
            return output
        }

-- | Transform the monad a 'Conduit' lives in.
transConduit :: (Monad m, Base m ~ Base n)
              => (forall a. m a -> n a)
              -> Conduit input m output
              -> Conduit input n output
transConduit f (Conduit mc) =
    Conduit (transResourceT f (liftM go mc))
  where
    go c = c
        { conduitPush = transResourceT f . conduitPush c
        , conduitClose = transResourceT f (conduitClose c)
        }
