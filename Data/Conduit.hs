{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Note: although this module does expose a resumable source, this source is
-- /not/ threadsafe. It can only be used within a single thread.
--
-- Non-resumable sources are also not-thread-safe, but reusing them in multiple
-- threads is impossible at the type level.
module Data.Conduit
    ( -- * Connect pieces together
      ($$)
    , (<$$>)
    , ($=)
    , (<$=>)
    , (=$)
    , (<=$>)
    , (<=$=>)
      -- * Conduit Types
      -- ** Source
    , module Data.Conduit.Types.Source
    , module Data.Conduit.Util.Source
      -- ** Sink
    , module Data.Conduit.Types.Sink
    , module Data.Conduit.Util.Sink
      -- ** Conduit
    , module Data.Conduit.Types.Conduit
    , module Data.Conduit.Util.Conduit
      -- * Other utils
    , sequence
      -- * Convenience re-exports
    , ResourceT
    , runResourceT
    , MonadBase
    , MonadBaseControl
    , liftBase
    ) where

import Prelude hiding (sequence)
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit.Types.Source
import Data.Conduit.Util.Source
import Data.Conduit.Types.Sink
import Data.Conduit.Util.Sink
import Data.Conduit.Types.Conduit
import Data.Conduit.Util.Conduit
import qualified Data.IORef as I
import Control.Monad.Base (MonadBase, liftBase)

infixr 0 $$

($$) :: MonadBaseControl IO m => BSource m a -> SinkM a m b -> ResourceT m b
bs $$ SinkM msink = do
    sinkI <- msink
    case sinkI of
        SinkNoData output -> return output
        SinkData push close -> connect' push close
  where
    connect' push close = do
        SourceResult state a <- bsourcePull bs
        case state of
            StreamClosed -> do
                SinkResult leftover res <- close a
                bsourceUnpull bs leftover
                return res
            StreamOpen -> do
                SinkResult leftover mres <- push a
                bsourceUnpull bs leftover
                case mres of
                    Just res -> return res
                    Nothing -> connect' push close

infixr 0 <$$>

(<$$>) :: MonadBaseControl IO m => SourceM m a -> SinkM a m b -> ResourceT m b
msrc <$$> sink = bsourceM msrc >>= ($$ sink)

infixl 1 <$=>

(<$=>) :: MonadBaseControl IO m
     => SourceM m a
     -> ConduitM a m b
     -> SourceM m b
srcm <$=> ConduitM mc = SourceM $ do
    istate <- liftBase $ I.newIORef StreamOpen
    bsrc <- bsourceM srcm
    c <- mc
    return Source
        { sourcePull = do
            state' <- liftBase $ I.readIORef istate
            case state' of
                StreamClosed -> return $ SourceResult StreamClosed []
                StreamOpen -> do
                    SourceResult state input <- bsourcePull bsrc
                    case state of
                        StreamClosed -> do
                            liftBase $ I.writeIORef istate StreamClosed
                            ConduitCloseResult leftover o <- conduitClose c input
                            bsourceUnpull bsrc leftover
                            return $ SourceResult StreamClosed o
                        StreamOpen -> do
                            ConduitResult cstate leftover output <-
                                conduitPush c input
                            bsourceUnpull bsrc leftover
                            case cstate of
                                StreamClosed -> do
                                    bsourceClose bsrc
                                    liftBase $ I.writeIORef istate StreamClosed
                                    return $ SourceResult StreamClosed output
                                StreamOpen -> return $ SourceResult StreamOpen output
        , sourceClose = do
            -- Invariant: sourceClose cannot be called twice, so we will assume
            -- it is currently open. We could add a sanity check here.
            liftBase $ I.writeIORef istate StreamClosed
            _ignored <- conduitClose c []
            bsourceClose bsrc
        }

infixl 1 $=

($=) :: MonadBaseControl IO m
     => BSource m a
     -> BConduit a m b
     -> BSource m b
bsrc $= bcon = BSource
    { bsourcePull = do
        output <- bconduitPull bcon
        if null output
            then do
                SourceResult state input <- bsourcePull bsrc
                case state of
                    StreamOpen -> do
                        ConduitResult cstate leftover output' <- bconduitPush bcon input
                        bsourceUnpull bsrc leftover
                        return $ SourceResult cstate output'
                    StreamClosed -> do
                        ConduitCloseResult leftover s <- bconduitClose bcon input
                        bsourceUnpull bsrc leftover
                        return $ SourceResult StreamClosed s
            else return $ SourceResult StreamOpen output
    , bsourceUnpull = bconduitUnpull bcon
    , bsourceClose = do
        ConduitCloseResult leftover _ignored <- bconduitClose bcon []
        bsourceUnpull bsrc leftover
        bsourceClose bsrc
    }

infixr 0 <=$>

(<=$>) :: MonadBaseControl IO m => ConduitM a m b -> SinkM b m c -> SinkM a m c
mc <=$> s = SinkM $ bconduitM mc >>= genSink . (=$ s) -- FIXME close the conduit

infixr 0 =$

(=$) :: MonadBaseControl IO m => BConduit a m b -> SinkM b m c -> SinkM a m c
c =$ (SinkM ms) = SinkM $ do
    s <- ms
    case s of
        SinkData pushI closeI -> return $ SinkData
            { sinkPush = push pushI closeI
            , sinkClose = close closeI
            }
        SinkNoData mres -> return $ SinkNoData mres
  where
    push pushI closeI stream = do
        -- FIXME bconduitPull
        ConduitResult state leftover cs <- bconduitPush c stream
        case state of
            StreamOpen -> do
                SinkResult leftover' mres <- pushI cs
                bconduitUnpull c leftover'
                return $ SinkResult leftover mres
            StreamClosed -> do
                SinkResult leftover' res <- closeI cs
                bconduitUnpull c leftover'
                return $ SinkResult leftover (Just res)
    close closeI input = do
        ConduitCloseResult leftover input' <- bconduitClose c input
        SinkResult leftover' res <- closeI input'
        bconduitUnpull c leftover'
        return $ SinkResult leftover res

infixr 0 <=$=>

(<=$=>) :: MonadBaseControl IO m => ConduitM a m b -> ConduitM b m c -> ConduitM a m c
outerM <=$=> ConduitM innerM = ConduitM $ do
    outer <- bconduitM outerM
    inner <- innerM
    return Conduit
        { conduitPush = \a -> do
            ConduitResult ostate leftoverO b <- bconduitPush outer a
            case ostate of
                StreamClosed -> do
                    ConduitCloseResult _leftoverI c <- conduitClose inner b
                    return $ ConduitResult StreamClosed leftoverO c
                StreamOpen -> do
                    ConduitResult istate leftoverI c <- conduitPush inner b
                    case istate of
                        StreamClosed -> do
                            _ <- bconduitClose outer []
                            return $ ConduitResult StreamClosed leftoverO c
                        StreamOpen -> do
                            bconduitUnpull outer leftoverI
                            return $ ConduitResult StreamOpen leftoverO c
        , conduitClose = \a -> do
            ConduitCloseResult leftoverO b <- bconduitClose outer a
            ConduitCloseResult _leftoverI c <- conduitClose inner b
            return $ ConduitCloseResult leftoverO c
        }

sequence :: MonadBaseControl IO m
         => SinkM a m b
         -> ConduitM a m b
sequence (SinkM sm) = ConduitM $ do
    sink <- sm
    genConduit $ conduitMState sink push close
  where
    push sink input = push' sink input id

    push' (SinkNoData output) input front = do
        sink <- sm
        return (sink, ConduitResult StreamOpen input $ front [output])
    push' sink@(SinkData p _) input front = do
        SinkResult leftover mres <- p input
        case mres of
            Nothing -> return (sink, ConduitResult StreamOpen leftover $ front [])
            Just res -> do
                sink' <- sm
                push' sink' leftover $ front . (res:)
    close (SinkNoData output) input = return $ ConduitCloseResult input [output]
    close (SinkData _ c) input0 =
        go input0 id
      where
        go input front = do
            SinkResult leftover res <- c input
            if null leftover
                then return $ ConduitCloseResult leftover $ front [res]
                else go leftover $ front . (res:)
