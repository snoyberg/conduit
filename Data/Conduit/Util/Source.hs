{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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

import Control.Monad.Trans.Resource (ResourceT, transResourceT, Resource (..))
import Data.Conduit.Types.Source
import Control.Monad (liftM)

-- | Construct a 'SourceM' with some stateful functions. This function address
-- all mutable state for you.
sourceMState
    :: Resource m
    => state -- ^ Initial state
    -> (state -> ResourceT m (state, SourceResult output)) -- ^ Pull function
    -> SourceM m output
sourceMState state0 pull = sourceM
    (newRef state0)
    (const $ return ())
    (\istate -> do
        state <- readRef istate
        (state', res) <- pull state
        writeRef istate state'
        return res)

-- | Construct a 'SourceM'.
sourceM :: Monad m
        => ResourceT m state -- ^ resource and/or state allocation
        -> (state -> ResourceT m ()) -- ^ resource and/or state cleanup
        -> (state -> ResourceT m (SourceResult output)) -- ^ Pull function. Note that this need not explicitly perform any cleanup.
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
bsource :: Resource m
        => Source m output
        -> ResourceT m (BSource m output)
bsource src = do
    istate <- newRef EmptyOpen
    return BSource
        { bsourcePull = do
            mresult <- modifyRef istate $ \state ->
                case state of
                    Open buffer -> (EmptyOpen, Just $ SourceResult StreamOpen buffer)
                    Closed buffer -> (EmptyClosed, Just $ SourceResult StreamClosed buffer)
                    EmptyOpen -> (EmptyOpen, Nothing)
                    EmptyClosed -> (EmptyClosed, Just $ SourceResult StreamClosed [])
            case mresult of
                Nothing -> do
                    result@(SourceResult state _) <- sourcePull src
                    case state of
                        StreamClosed -> writeRef istate EmptyClosed
                        StreamOpen -> return ()
                    return result
                Just result -> return result
        , bsourceUnpull =
            \x ->
                if null x
                    then return ()
                    else modifyRef istate $ \state ->
                        case state of
                            Open buffer -> (Open (x ++ buffer), ())
                            Closed buffer -> (Closed (x ++ buffer), ())
                            EmptyOpen -> (Open x, ())
                            EmptyClosed -> (Closed x, ())
        , bsourceClose = do
            action <- modifyRef istate $ \state ->
                case state of
                    Open x -> (Closed x, sourceClose src)
                    Closed _ -> (state, return ())
                    EmptyOpen -> (EmptyClosed, sourceClose src)
                    EmptyClosed -> (state, return ())
            action
        }

-- | Convert a 'SourceM' into a 'BSource'.
bsourceM :: Resource m
         => SourceM m output
         -> ResourceT m (BSource m output)
bsourceM (SourceM msrc) = msrc >>= bsource

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
