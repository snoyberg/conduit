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
    , ResourceIO
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

($$) :: (BufferSource bsrc, Resource m) => bsrc m a -> Sink a m b -> ResourceT m b
bs' $$ Sink msink = do
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
                    mres <- push a
                    case mres of
                        Done (SinkResult leftover res) -> do
                            bsourceUnpull bs leftover
                            bsourceClose bs
                            return res
                        Processing -> loop

infixl 1 $=

($=) :: (Resource m, BufferSource bsrc)
     => bsrc m a
     -> Conduit a m b
     -> Source m b
bsrc' $= Conduit mc = Source $ do
    istate <- newRef StreamOpen
    bsrc <- bufferSource bsrc'
    c <- mc
    return PreparedSource
        { sourcePull = do
            state' <- readRef istate
            case state' of
                StreamClosed -> return $ SourceResult StreamClosed []
                StreamOpen -> do
                    SourceResult state input <- bsourcePull bsrc
                    case state of
                        StreamClosed -> do
                            writeRef istate StreamClosed
                            ConduitResult leftover o <- conduitClose c input
                            bsourceUnpull bsrc leftover
                            return $ SourceResult StreamClosed o
                        StreamOpen -> do
                            res <- conduitPush c input
                            case res of
                                ConduitResult Processing output ->
                                    return $ SourceResult StreamOpen output
                                ConduitResult (Done leftover) output -> do
                                    bsourceUnpull bsrc leftover
                                    bsourceClose bsrc
                                    writeRef istate StreamClosed
                                    return $ SourceResult StreamClosed output
        , sourceClose = do
            -- Invariant: sourceClose cannot be called twice, so we will assume
            -- it is currently open. We could add a sanity check here.
            writeRef istate StreamClosed
            _ignored <- conduitClose c []
            bsourceClose bsrc
        }

infixr 0 =$

(=$) :: Resource m => Conduit a m b -> Sink b m c -> Sink a m c
Conduit mc =$ Sink ms = Sink $ do
    s <- ms
    case s of
        SinkData pushI closeI -> mc >>= go pushI closeI
        SinkNoData mres -> return $ SinkNoData mres
  where
    go pushI closeI c = do
        return SinkData
            { sinkPush = \cinput -> do
                res <- conduitPush c cinput
                case res of
                    ConduitResult Processing sinput -> do
                        mres <- pushI sinput
                        case mres of
                            Processing -> return Processing
                            Done (SinkResult _sleftover res') -> do
                                ConduitResult cleftover _ <- conduitClose c []
                                return $ Done $ SinkResult cleftover res'
                    ConduitResult (Done cleftover) sinput -> do
                        SinkResult _ res' <- closeI sinput
                        return $ Done $ SinkResult cleftover res'
            , sinkClose = \cinput -> do
                ConduitResult cleftover sinput <- conduitClose c cinput
                SinkResult _ res <- closeI sinput
                return $ SinkResult cleftover res
            }

infixr 0 =$=

(=$=) :: Resource m => Conduit a m b -> Conduit b m c -> Conduit a m c
Conduit outerM =$= Conduit innerM = Conduit $ do
    outer <- outerM
    inner <- innerM
    return PreparedConduit
        { conduitPush = \inputO -> do
            res <- conduitPush outer inputO
            case res of
                ConduitResult Processing inputI -> do
                    resI <- conduitPush inner inputI
                    case resI of
                        ConduitResult Processing c ->
                            return $ ConduitResult Processing c
                        ConduitResult (Done _leftoverI) c -> do
                            ConduitResult leftoverO _ <- conduitClose outer []
                            return $ ConduitResult (Done leftoverO) c
                ConduitResult (Done leftoverO) inputI -> do
                    ConduitResult _leftoverI c <- conduitClose inner inputI
                    return $ ConduitResult (Done leftoverO) c
        , conduitClose = \a -> do
            ConduitResult leftoverO b <- conduitClose outer a
            ConduitResult _leftoverI c <- conduitClose inner b
            return $ ConduitResult leftoverO c
        }

sequence :: Resource m
         => Sink a m b
         -> Conduit a m b
sequence (Sink sm) = Conduit $ do
    sink <- sm
    prepareConduit $ conduitState (id, sink) push close
  where
    push sink input = push' sink input id

    push' (frontI, SinkNoData output) input frontO = do
        sink <- sm
        return ((frontI . (input++), sink), ConduitResult Processing $ frontO [output])
    push' (frontI, sink@(SinkData p _)) input frontO = do
        mres <- p $ frontI input
        case mres of
            Processing -> return ((id, sink), ConduitResult Processing $ frontO [])
            Done (SinkResult leftover res) -> do
                sink' <- sm
                push' (id, sink') leftover $ frontO . (res:)
    close (frontI, SinkNoData output) input = return $ ConduitResult (frontI input) [output]
    close (frontI, SinkData _ c) input0 =
        go (frontI input0) id
      where
        go input front = do
            SinkResult leftover res <- c input
            if null leftover
                then return $ ConduitResult leftover $ front [res]
                else go leftover $ front . (res:)
