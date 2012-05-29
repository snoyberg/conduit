{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing and converting 'Source', 'Source' and
-- 'BSource' types. Please see "Data.Conduit.Types.Source" for more information
-- on the base types.
module Data.Conduit.Util.Source
    ( sourceState
    , sourceStateIO
    , SourceStateResult (..)
    , sourceIO
    , SourceIOResult (..)
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Internal

-- | The return value when pulling in the @sourceState@ function. Either
-- indicates no more data, or the next value and an updated state.
--
-- Since 0.3.0
data SourceStateResult state output = StateOpen state output | StateClosed

-- | Construct a 'Source' with some stateful functions. This function addresses
-- threading the state value for you.
--
-- Since 0.3.0
sourceState
    :: Monad m
    => state -- ^ Initial state
    -> (state -> m (SourceStateResult state output)) -- ^ Pull function
    -> Source m output
sourceState state0 pull0 =
    src state0
  where
    src state = PipeM (pull state) (return ())

    pull state = do
        res <- pull0 state
        return $ case res of
            StateOpen state' val -> HaveOutput (src state') (return ()) val
            StateClosed -> Done ()

-- | The return value when pulling in the @sourceIO@ function. Either indicates
-- no more data, or the next value.
--
-- Since 0.3.0
data SourceIOResult output = IOOpen output | IOClosed

-- | Construct a 'Source' based on some IO actions for alloc/release.
--
-- Since 0.3.0
sourceIO :: MonadResource m
          => IO state -- ^ resource and/or state allocation
          -> (state -> IO ()) -- ^ resource and/or state cleanup
          -> (state -> m (SourceIOResult output)) -- ^ Pull function. Note that this should not perform any cleanup.
          -> Source m output
sourceIO alloc cleanup pull0 =
    PipeM (do
        (key, state) <- allocate alloc cleanup
        pull key state) (return ())
  where
    src key state = PipeM (pull key state) (release key)

    pull key state = do
        res <- pull0 state
        case res of
            IOClosed -> do
                release key
                return $ Done ()
            IOOpen val -> return $ HaveOutput (src key state) (release key) val

-- | A combination of 'sourceIO' and 'sourceState'.
--
-- Since 0.3.0
sourceStateIO :: MonadResource m
              => IO state -- ^ resource and/or state allocation
              -> (state -> IO ()) -- ^ resource and/or state cleanup
              -> (state -> m (SourceStateResult state output)) -- ^ Pull function. Note that this need not explicitly perform any cleanup.
              -> Source m output
sourceStateIO alloc cleanup pull0 =
    PipeM (do
        (key, state) <- allocate alloc cleanup
        pull key state) (return ())
  where
    src key state = PipeM (pull key state) (release key)

    pull key state = do
        res <- pull0 state
        case res of
            StateClosed -> do
                release key
                return $ Done ()
            StateOpen state' val -> return $ HaveOutput (src key state') (release key) val

-- FIXME transPipe
