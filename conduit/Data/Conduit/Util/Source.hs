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
    , SourceStateResult (..)
    , SourceIOResult (..)
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Types.Source

data SourceStateResult state output = StateOpen state output | StateClosed

-- | Construct a 'Source' with some stateful functions. This function address
-- all mutable state for you.
--
-- Since 0.0.0
sourceState
    :: Resource m
    => state -- ^ Initial state
    -> (state -> ResourceT m (SourceStateResult state output)) -- ^ Pull function
    -> Source m output
sourceState state0 pull0 =
    src state0
  where
    src state = Source (pull state) close

    pull state = do
        res <- pull0 state
        return $ case res of
            StateOpen state' val -> Open (src state') val
            StateClosed -> Closed

    close = return ()

data SourceIOResult output = IOOpen output | IOClosed

-- | Construct a 'Source' based on some IO actions for alloc/release.
--
-- Since 0.0.0
sourceIO :: ResourceIO m
          => IO state -- ^ resource and/or state allocation
          -> (state -> IO ()) -- ^ resource and/or state cleanup
          -> (state -> m (SourceIOResult output)) -- ^ Pull function. Note that this need not explicitly perform any cleanup.
          -> Source m output
sourceIO alloc cleanup pull0 =
    Source
        { sourcePull = do
            (key, state) <- withIO alloc cleanup
            pull key state
        , sourceClose = return ()
        }
  where
    src key state = Source (pull key state) (release key)

    pull key state = do
        res <- lift $ pull0 state
        case res of
            IOClosed -> do
                release key
                return Closed
            IOOpen val -> return $ Open (src key state) val

-- | Transform the monad a 'Source' lives in.
--
-- Since 0.0.0
transSource :: (Base m ~ Base n, Monad m)
             => (forall a. m a -> n a)
             -> Source m output
             -> Source n output
transSource f c = c
    { sourcePull = transResourceT f (fmap go2 $ sourcePull c)
    , sourceClose = transResourceT f (sourceClose c)
    }
  where
    go2 (Open p a) = Open (transSource f p) a
    go2 Closed = Closed
