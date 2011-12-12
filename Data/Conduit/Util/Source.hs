{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Utilities for constructing and converting 'Source', 'SourceM' and
-- 'BSource' types. Please see "Data.Conduit.Types.Source" for more information
-- on the base types.
module Data.Conduit.Util.Source
    ( sourceM
    , bsource
    , bsourceM
    , transSourceM
    , sourceMState
    ) where

import Control.Monad.Trans.Resource (ResourceT, transResourceT)
import Data.Conduit.Types.Source
import Control.Monad.Base (MonadBase (liftBase))
import qualified Data.IORef as I
import Control.Monad (liftM)

-- | Construct a 'SourceM' with some stateful functions. This function address
-- all mutable state for you.
sourceMState
    :: MonadBase IO m
    => state -- ^ Initial state
    -> (state -> ResourceT m (state, Stream output)) -- ^ Pull function
    -> SourceM m output
sourceMState state0 pull = sourceM
    (liftBase $ I.newIORef state0)
    (const $ return ())
    (\istate -> do
        state <- liftBase $ I.readIORef istate
        (state', res) <- pull state
        liftBase $ I.writeIORef istate state'
        return res)

-- | Construct a 'SourceM'.
sourceM :: Monad m
        => ResourceT m state -- ^ resource and/or state allocation
        -> (state -> ResourceT m ()) -- ^ resource and/or state cleanup
        -> (state -> ResourceT m (Stream output)) -- ^ Pull function. Note that this need not explicitly perform any cleanup.
        -> SourceM m output
sourceM alloc cleanup pull = SourceM $ do
    state <- alloc
    return Source
        { sourcePull = pull state
        , sourceClose = cleanup state
        }

-- | State of a 'BSource'
data BState a = EmptyOpen -- ^ nothing in buffer, EOF not received yet
              | EmptyClosed -- ^ nothing in buffer, EOF has been received
              | Open [a] -- ^ something in buffer, EOF not received yet
              | Closed [a] -- ^ something in buffer, EOF has been received
    deriving Show

-- | Convert a 'SourceM' into a 'BSource'.
bsource :: MonadBase IO m
        => Source m output
        -> ResourceT m (BSource m output)
bsource src = do
    istate <- liftBase (I.newIORef EmptyOpen)
    return BSource
        { bsourcePull = do
            mresult <- liftBase $ I.atomicModifyIORef istate $ \state ->
                case state of
                    Open buffer -> (EmptyOpen, Just (Chunks buffer))
                    Closed buffer -> (EmptyClosed, Just (Chunks buffer))
                    EmptyOpen -> (EmptyOpen, Nothing)
                    EmptyClosed -> (EmptyClosed, Just EOF)
            case mresult of
                Nothing -> do
                    result <- sourcePull src
                    case result of
                        EOF -> do
                            liftBase $ I.writeIORef istate EmptyClosed
                            return EOF
                        _ -> return result
                Just result -> return result
        , bsourceUnpull =
            \x ->
                if null x
                    then return ()
                    else liftBase $ I.atomicModifyIORef istate $ \state ->
                        case state of
                            Open buffer -> (Open (x ++ buffer), ())
                            Closed buffer -> (Closed (x ++ buffer), ())
                            EmptyOpen -> (Open x, ())
                            EmptyClosed -> (Closed x, ())
        , bsourceClose = do
            action <- liftBase $ I.atomicModifyIORef istate $ \state ->
                case state of
                    Open x -> (Closed x, sourceClose src)
                    Closed _ -> (state, return ())
                    EmptyOpen -> (EmptyClosed, sourceClose src)
                    EmptyClosed -> (state, return ())
            action
        }

-- | Convert a 'SourceM' into a 'BSource'.
bsourceM :: MonadBase IO m
         => SourceM m output
         -> ResourceT m (BSource m output)
bsourceM (SourceM msrc) = msrc >>= bsource

-- | Transform the monad a 'SourceM' lives in.
transSourceM :: Monad m
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
