{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Utilities for constructing and converting 'Source', 'SourceM' and
-- 'BSource' types. Please see "Data.Conduit.Types.Source" for more information
-- on the base types.
module Data.Conduit.Util.Source
    ( sourceMIO
    , transSourceM
    , sourceMState
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Types.Source
import Control.Monad (liftM)

-- | Construct a 'SourceM' with some stateful functions. This function address
-- all mutable state for you.
sourceMState
    :: Resource m
    => state -- ^ Initial state
    -> (state -> ResourceT m (state, SourceResult output)) -- ^ Pull function
    -> SourceM m output
sourceMState state0 pull = SourceM $ do
    istate <- newRef state0
    return Source
        { sourcePull = do
            state <- readRef istate
            (state', res) <- pull state
            writeRef istate state'
            return res
        , sourceClose = return ()
        }

-- | Construct a 'SourceM' based on some IO actions for alloc/release.
sourceMIO :: ResourceIO m
          => IO state -- ^ resource and/or state allocation
          -> (state -> IO ()) -- ^ resource and/or state cleanup
          -> (state -> m (SourceResult output)) -- ^ Pull function. Note that this need not explicitly perform any cleanup.
          -> SourceM m output
sourceMIO alloc cleanup pull = SourceM $ do
    (key, state) <- withIO alloc cleanup
    return Source
        { sourcePull = do
            res@(SourceResult s _) <- lift $ pull state
            case s of
                StreamClosed -> release key
                _ -> return ()
            return res
        , sourceClose = release key
        }

-- | Transform the monad a 'SourceM' lives in.
transSourceM :: (Base m ~ Base n, Monad m)
             => (forall a. m a -> n a)
             -> SourceM m output
             -> SourceM n output
transSourceM f (SourceM mc) =
    SourceM (transResourceT f (liftM go mc))
  where
    go c = c
        { sourcePull = transResourceT f (sourcePull c)
        , sourceClose = transResourceT f (sourceClose c)
        }
