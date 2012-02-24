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
    , transSource
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Types.Source
import Control.Monad (liftM)

-- | The return value when pulling in the @sourceState@ function. Either
-- indicates no more data, or the next value and an updated state.
--
-- Since 0.2.0
data SourceStateResult state output = StateOpen state output | StateClosed

-- | Construct a 'Source' with some stateful functions. This function addresses
-- threading the state value for you.
--
-- Since 0.2.0
sourceState
    :: Monad m
    => state -- ^ Initial state
    -> (state -> m (SourceStateResult state output)) -- ^ Pull function
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

-- | The return value when pulling in the @sourceIO@ function. Either indicates
-- no more data, or the next value.
data SourceIOResult output = IOOpen output | IOClosed

-- | Construct a 'Source' based on some IO actions for alloc/release.
--
-- Since 0.2.0
sourceIO :: MonadResource m
          => IO state -- ^ resource and/or state allocation
          -> (state -> IO ()) -- ^ resource and/or state cleanup
          -> (state -> m (SourceIOResult output)) -- ^ Pull function. Note that this need not explicitly perform any cleanup.
          -> Source m output
sourceIO alloc cleanup pull0 =
    Source
        { sourcePull = do
            (key, state) <- with alloc cleanup
            pull key state
        , sourceClose = return ()
        }
  where
    src key state = Source (pull key state) (release key)

    pull key state = do
        res <- pull0 state
        case res of
            IOClosed -> do
                release key
                return Closed
            IOOpen val -> return $ Open (src key state) val

-- | A combination of 'sourceIO' and 'sourceState'.
--
-- Since 0.2.1
sourceStateIO :: MonadResource m
              => IO state -- ^ resource and/or state allocation
              -> (state -> IO ()) -- ^ resource and/or state cleanup
              -> (state -> m (SourceStateResult state output)) -- ^ Pull function. Note that this need not explicitly perform any cleanup.
              -> Source m output
sourceStateIO alloc cleanup pull0 =
    Source
        { sourcePull = do
            (key, state) <- with alloc cleanup
            pull key state
        , sourceClose = return ()
        }
  where
    src key state = Source (pull key state) (release key)

    pull key state = do
        res <- pull0 state
        case res of
            StateClosed -> do
                release key
                return Closed
            StateOpen state' val -> return $ Open (src key state') val

-- | Transform the monad a 'Source' lives in.
--
-- Note that this will /not/ thread the individual monads together, meaning
-- side effects will be lost. This function is most useful for transformers
-- only providing context and not producing side-effects, such as @ReaderT@.
--
-- Since 0.2.0
transSource :: Monad m
            => (forall a. m a -> n a)
            -> Source m output
            -> Source n output
transSource f c = c
    { sourcePull = f (liftM go2 $ sourcePull c)
    , sourceClose = f (sourceClose c)
    }
  where
    go2 (Open p a) = Open (transSource f p) a
    go2 Closed = Closed
