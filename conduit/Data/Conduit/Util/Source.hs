{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing and converting 'Source', 'Source' and
-- 'BSource' types. Please see "Data.Conduit.Types.Source" for more information
-- on the base types.
module Data.Conduit.Util.Source
    ( sourceState
    , sourceIO
    , transSource
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Types.Source
import Control.Monad (liftM)

-- | Construct a 'Source' with some stateful functions. This function address
-- all mutable state for you.
sourceState
    :: Resource m
    => state -- ^ Initial state
    -> (state -> ResourceT m (state, SourceResult output)) -- ^ Pull function
    -> Source m output
sourceState state0 pull = Source $ do
    istate <- newRef state0
#if DEBUG
    iclosed <- newRef False
#endif
    return PreparedSource
        { sourcePull = do
#if DEBUG
            False <- readRef iclosed
#endif
            state <- readRef istate
            (state', res) <- pull state
#if DEBUG
            let isClosed =
                    case res of
                        Closed -> True
                        Open _ -> False
            writeRef iclosed isClosed
#endif
            writeRef istate state'
            return res
        , sourceClose = do
#if DEBUG
            False <- readRef iclosed
            writeRef iclosed True
#else
            return ()
#endif
        }

-- | Construct a 'Source' based on some IO actions for alloc/release.
sourceIO :: ResourceIO m
          => IO state -- ^ resource and/or state allocation
          -> (state -> IO ()) -- ^ resource and/or state cleanup
          -> (state -> m (SourceResult output)) -- ^ Pull function. Note that this need not explicitly perform any cleanup.
          -> Source m output
sourceIO alloc cleanup pull = Source $ do
    (key, state) <- withIO alloc cleanup
#if DEBUG
    iclosed <- newRef False
#endif
    return PreparedSource
        { sourcePull = do
#if DEBUG
            False <- readRef iclosed
#endif
            res <- lift $ pull state
            case res of
                Closed -> do
#if DEBUG
                    writeRef iclosed True
#endif
                    release key
                _ -> return ()
            return res
        , sourceClose = do
#if DEBUG
            False <- readRef iclosed
            writeRef iclosed True
#endif
            release key
        }

-- | Transform the monad a 'Source' lives in.
transSource :: (Base m ~ Base n, Monad m)
             => (forall a. m a -> n a)
             -> Source m output
             -> Source n output
transSource f (Source mc) =
    Source (transResourceT f (liftM go mc))
  where
    go c = c
        { sourcePull = transResourceT f (sourcePull c)
        , sourceClose = transResourceT f (sourceClose c)
        }
