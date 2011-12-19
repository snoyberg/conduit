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
    , ($=)
    , (=$)
    , (=$=)
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
    , Resource (..)
    , runResourceT
    , MonadBase
    , MonadBaseControl
    , liftBase
    , ResourceThrow (..)
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
import Control.Monad.Base (MonadBase, liftBase)

infixr 0 $$

($$) :: (BufferSource bsrc, Resource m) => bsrc m a -> SinkM a m b -> ResourceT m b
bs' $$ SinkM msink = do
    sinkI <- msink
    case sinkI of
        SinkNoData output -> return output
        SinkData push close -> do
            bs <- bufferSource bs'
            connect' bs push close
  where
    connect' bs push close =
        loop
      where
        loop = do
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
                        Nothing -> loop

infixl 1 $=

($=) :: (Resource m, BufferSource bsrc)
     => bsrc m a
     -> ConduitM a m b
     -> SourceM m b
bsrc' $= ConduitM mc = SourceM $ do
    istate <- newRef StreamOpen
    bsrc <- bufferSource bsrc'
    c <- mc
    return Source
        { sourcePull = do
            state' <- readRef istate
            case state' of
                StreamClosed -> return $ SourceResult StreamClosed []
                StreamOpen -> do
                    SourceResult state input <- bsourcePull bsrc
                    case state of
                        StreamClosed -> do
                            writeRef istate StreamClosed
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
                                    writeRef istate StreamClosed
                                    return $ SourceResult StreamClosed output
                                StreamOpen -> return $ SourceResult StreamOpen output
        , sourceClose = do
            -- Invariant: sourceClose cannot be called twice, so we will assume
            -- it is currently open. We could add a sanity check here.
            writeRef istate StreamClosed
            _ignored <- conduitClose c []
            bsourceClose bsrc
        }

infixr 0 =$

(=$) :: Resource m => ConduitM a m b -> SinkM b m c -> SinkM a m c
ConduitM mc =$ SinkM ms = SinkM $ do
    s <- ms
    case s of
        SinkData pushI closeI -> mc >>= go pushI closeI
        SinkNoData mres -> return $ SinkNoData mres
  where
    go pushI closeI c = do
        ibuffer <- newRef id
        return SinkData
            { sinkPush = \cinput -> do
                ConduitResult state cleftover sinput <- conduitPush c cinput
                buffer <- readRef ibuffer
                case state of
                    StreamOpen -> do
                        SinkResult sleftover mres <- pushI $ buffer sinput
                        writeRef ibuffer (sleftover ++)
                        case mres of
                            Nothing -> return $ SinkResult cleftover mres
                            Just _ -> do
                                ConduitCloseResult cleftover' _ <-
                                    conduitClose c cleftover
                                return $ SinkResult cleftover' mres
                    StreamClosed -> do
                        SinkResult _ res <- closeI $ buffer sinput
                        return $ SinkResult cleftover (Just res)
            , sinkClose = \cinput -> do
                ConduitCloseResult cleftover sinput <- conduitClose c cinput
                buffer <- readRef ibuffer
                SinkResult _ res <- closeI $ buffer sinput
                return $ SinkResult cleftover res
            }

infixr 0 =$=

(=$=) :: Resource m => ConduitM a m b -> ConduitM b m c -> ConduitM a m c
ConduitM outerM =$= ConduitM innerM = ConduitM $ do
    outer <- outerM
    inner <- innerM
    ibuffer <- newRef id
    return Conduit
        { conduitPush = \inputO -> do
            ConduitResult ostate leftoverO inputI' <- conduitPush outer inputO
            buffer <- readRef ibuffer
            let inputI = buffer inputI'
            case ostate of
                StreamClosed -> do
                    ConduitCloseResult _leftoverI c <- conduitClose inner inputI
                    return $ ConduitResult StreamClosed leftoverO c
                StreamOpen -> do
                    ConduitResult istate leftoverI c <- conduitPush inner inputI
                    case istate of
                        StreamClosed -> do
                            _ <- conduitClose outer []
                            return $ ConduitResult StreamClosed leftoverO c
                        StreamOpen -> do
                            writeRef ibuffer (leftoverI ++)
                            return $ ConduitResult StreamOpen leftoverO c
        , conduitClose = \a -> do
            ConduitCloseResult leftoverO b <- conduitClose outer a
            ConduitCloseResult _leftoverI c <- conduitClose inner b
            return $ ConduitCloseResult leftoverO c
        }

sequence :: Resource m
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
