{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing and covnerting conduits. Please see
-- "Data.Conduit.Types.Conduit" for more information on the base types.
module Data.Conduit.Util.Conduit
    ( haveMore
    , conduitState
    , ConduitStateResult (..)
    , conduitIO
    , ConduitIOResult (..)
      -- *** Sequencing
    , SequencedSink
    , sequenceSink
    , sequence
    , SequencedSinkResponse (..)
    ) where

import Prelude hiding (sequence)
import Control.Monad.Trans.Resource
import Data.Conduit.Internal
import Control.Monad (liftM, when)

-- | A helper function for returning a list of values from a @Conduit@.
--
-- Since 0.3.0
haveMore :: Conduit a m b -- ^ The next @Conduit@ to return after the list has been exhausted.
         -> m () -- ^ A close action for early termination.
         -> [b] -- ^ The values to send down the stream.
         -> Conduit a m b
haveMore res _ [] = res
haveMore res close (x:xs) = HaveOutput (haveMore res close xs) (FinalizeM close) x

-- | A helper type for @conduitState@, indicating the result of being pushed
-- to.  It can either indicate that processing is done, or to continue with the
-- updated state.
--
-- Since 0.3.0
data ConduitStateResult state input output =
    StateFinished (Maybe input) [output]
  | StateProducing state [output]

instance Functor (ConduitStateResult state input) where
    fmap f (StateFinished a b) = StateFinished a (map f b)
    fmap f (StateProducing a b) = StateProducing a (map f b)

-- | Construct a 'Conduit' with some stateful functions. This function addresses
-- threading the state value for you.
--
-- Since 0.3.0
conduitState
    :: Monad m
    => state -- ^ initial state
    -> (state -> input -> m (ConduitStateResult state input output)) -- ^ Push function.
    -> (state -> m [output]) -- ^ Close function. The state need not be returned, since it will not be used again.
    -> Conduit input m output
conduitState state0 push0 close0 =
    NeedInput (push state0) (close state0)
  where
    push state input = PipeM (liftM goRes' $ state `seq` push0 state input) (return ())

    close state = PipeM (do
        os <- close0 state
        return $ fromList os) (return ())

    goRes' (StateFinished leftover output) = maybe id pipePush leftover $ haveMore
        (Done ())
        (return ())
        output
    goRes' (StateProducing state output) = haveMore
        (NeedInput (push state) (close state))
        (return ())
        output

-- | A helper type for @conduitIO@, indicating the result of being pushed to.
-- It can either indicate that processing is done, or to continue.
--
-- Since 0.3.0
data ConduitIOResult input output =
    IOFinished (Maybe input) [output]
  | IOProducing [output]

instance Functor (ConduitIOResult input) where
    fmap f (IOFinished a b) = IOFinished a (map f b)
    fmap f (IOProducing b) = IOProducing (map f b)

-- | Construct a 'Conduit'.
--
-- Since 0.3.0
conduitIO :: MonadResource m
           => IO state -- ^ resource and/or state allocation
           -> (state -> IO ()) -- ^ resource and/or state cleanup
           -> (state -> input -> m (ConduitIOResult input output)) -- ^ Push function. Note that this need not explicitly perform any cleanup.
           -> (state -> m [output]) -- ^ Close function. Note that this need not explicitly perform any cleanup.
           -> Conduit input m output
conduitIO alloc cleanup push0 close0 = NeedInput
    (\input -> flip PipeM (return ()) $ do
        (key, state) <- allocate alloc cleanup
        push key state input)
    (PipeM (do
        (key, state) <- allocate alloc cleanup
        os <- close0 state
        release key
        return $ fromList os) (return ()))
  where
    push key state input = do
        res <- push0 state input
        case res of
            IOProducing output -> return $ haveMore
                (NeedInput (flip PipeM (FinalizeM $ release key) . push key state) (close key state))
                (release key >> return ())
                output
            IOFinished leftover output -> do
                release key
                return $ maybe id pipePush leftover $ haveMore
                    (Done ())
                    (return ())
                    output

    close key state = PipeM (do
        output <- close0 state
        release key
        return $ fromList output) (FinalizeM $ release key)

fromList :: Monad m => [a] -> Pipe i a m ()
fromList [] = Done ()
fromList (x:xs) = HaveOutput (fromList xs) (return ()) x

-- | Return value from a 'SequencedSink'.
--
-- Since 0.3.0
data SequencedSinkResponse state input m output =
    Emit state [output] -- ^ Set a new state, and emit some new output.
  | Stop -- ^ End the conduit.
  | StartConduit (Conduit input m output) -- ^ Pass control to a new conduit.

-- | Helper type for constructing a @Conduit@ based on @Sink@s. This allows you
-- to write higher-level code that takes advantage of existing conduits and
-- sinks, and leverages a sink's monadic interface.
--
-- Since 0.3.0
type SequencedSink state input m output =
    state -> Sink input m (SequencedSinkResponse state input m output)

-- | Convert a 'SequencedSink' into a 'Conduit'.
--
-- Since 0.3.0
sequenceSink
    :: Monad m
    => state -- ^ initial state
    -> SequencedSink state input m output
    -> Conduit input m output
sequenceSink state0 fsink = do
    x <- hasInput
    if x
        then do
            res <- sinkToPipe $ fsink state0
            case res of
                Emit state os -> do
                    fromList os
                    sequenceSink state fsink
                Stop -> return ()
                StartConduit c -> c
        else return ()

-- | Specialised version of 'sequenceSink'
--
-- Note that this function will return an infinite stream if provided a
-- @Sink@ which does not consume data. In other words, you probably don\'t want to do
-- @sequence . return@.
--
-- Since 0.3.0
sequence :: Monad m => Sink input m output -> Conduit input m output
sequence sink = self
  where
    self = do
        x <- hasInput
        when x $ do
          sinkToPipe sink >>= yield
          self
