{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- | Utilities for constructing and covnerting conduits. Please see
-- "Data.Conduit.Types.Conduit" for more information on the base types.
module Data.Conduit.Util.Conduit
    ( conduitState
    , conduitIO
    , transConduit
      -- *** Sequencing
    , SequencedSink
    , sequenceSink
    , SequencedSinkResponse (..)
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class
import Data.Conduit.Types.Conduit
import Data.Conduit.Types.Sink
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
            (state', res) <- state `seq` push state input
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

-- | Return value from a 'SequencedSink'.
data SequencedSinkResponse state input m output =
    Emit state [output] -- ^ Set a new state, and emit some new output.
  | Stop -- ^ End the conduit.
  | StartConduit (Conduit input m output) -- ^ Pass control to a new conduit.

-- | Helper type for constructing a @Conduit@ based on @Sink@s. This allows you
-- to write higher-level code that takes advantage of existing conduits and
-- sinks, and leverages a sink's monadic interface.
type SequencedSink state input m output =
    state -> Sink input m (SequencedSinkResponse state input m output)

data SCState state input m output =
    SCNewState state
  | SCConduit (PreparedConduit input m output)
  | SCSink (input -> ResourceT m (SinkResult input (SequencedSinkResponse state input m output)))
           (ResourceT m (SequencedSinkResponse state input m output))

-- | Convert a 'SequencedSink' into a 'Conduit'.
sequenceSink
    :: Resource m
    => state -- ^ initial state
    -> SequencedSink state input m output
    -> Conduit input m output
sequenceSink state0 fsink = conduitState
    (SCNewState state0)
    (scPush id fsink)
    scClose

goRes :: Resource m
      => SequencedSinkResponse state input m output
      -> Maybe input
      -> ([output] -> [output])
      -> SequencedSink state input m output
      -> ResourceT m (SCState state input m output, ConduitResult input output)
goRes (Emit state output) (Just input) front fsink =
    scPush (front . (output++)) fsink (SCNewState state) input
goRes (Emit state output) Nothing front _ =
    return (SCNewState state, Producing $ front output)
goRes Stop minput front _ =
    return (error "sequenceSink", Finished minput $ front [])
goRes (StartConduit c) Nothing front _ = do
    pc <- prepareConduit c
    return (SCConduit pc, Producing $ front [])
goRes (StartConduit c) (Just input) front fsink = do
    pc <- prepareConduit c
    scPush front fsink (SCConduit pc) input

scPush :: Resource m
     => ([output] -> [output])
     -> SequencedSink state input m output
     -> SCState state input m output
     -> input
     -> ResourceT m (SCState state input m output, ConduitResult input output)
scPush front fsink (SCNewState state) input = do
    sink <- prepareSink $ fsink state
    case sink of
        SinkData push' close' -> scPush front fsink (SCSink push' close') input
        SinkNoData res -> goRes res (Just input) front fsink
scPush front _ (SCConduit conduit) input = do
    res <- conduitPush conduit input
    let res' =
            case res of
                Producing x -> Producing $ front x
                Finished x y -> Finished x $ front y
    return (SCConduit conduit, res')
scPush front fsink (SCSink push close) input = do
    mres <- push input
    case mres of
        Done minput res -> goRes res minput front fsink
        Processing -> return (SCSink push close, Producing $ front [])

scClose :: Monad m => SCState state inptu m output -> ResourceT m [output]
scClose (SCNewState _) = return []
scClose (SCConduit conduit) = conduitClose conduit
scClose (SCSink _ close) = do
    res <- close
    case res of
        Emit _ os -> return os
        Stop -> return []
        StartConduit c -> do
            pc <- prepareConduit c
            conduitClose pc
