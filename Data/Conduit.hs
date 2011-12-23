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
            res <- bsourcePull bs
            case res of
                Closed -> do
                    SinkResult leftover res <- close
                    bsourceUnpull bs leftover
                    return res
                Open a -> do
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
    istate <- newRef True
    bsrc <- bufferSource bsrc'
    c <- mc
    return PreparedSource
        { sourcePull = do
            state' <- readRef istate
            case state' of
                False -> return Closed
                True -> do
                    res <- bsourcePull bsrc
                    case res of
                        Closed -> do
                            writeRef istate False
                            ConduitResult leftover o <- conduitClose c
                            bsourceUnpull bsrc leftover
                            return $ Open o
                        Open input -> do
                            res <- conduitPush c input
                            case res of
                                ConduitResult Processing output ->
                                    return $ Open output
                                ConduitResult (Done leftover) output -> do
                                    bsourceUnpull bsrc leftover
                                    bsourceClose bsrc
                                    writeRef istate False
                                    return $ Open output
        , sourceClose = do
            -- Invariant: sourceClose cannot be called twice, so we will assume
            -- it is currently open. We could add a sanity check here.
            writeRef istate False
            _ignored <- conduitClose c
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
                                ConduitResult cleftover _ <- conduitClose c
                                return $ Done $ SinkResult cleftover res'
                    ConduitResult (Done cleftover) sinput -> do
                        mres <- pushI sinput
                        SinkResult _ res' <-
                            case mres of
                                Done x -> return x
                                Processing -> closeI
                        return $ Done $ SinkResult cleftover res'
            , sinkClose = do
                ConduitResult cleftover sinput <- conduitClose c
                mres <- pushI sinput
                SinkResult _ res <-
                    case mres of
                        Done x -> return x
                        Processing -> closeI
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
                            ConduitResult leftoverO _ <- conduitClose outer
                            return $ ConduitResult (Done leftoverO) c
                ConduitResult (Done leftoverO) inputI -> do
                    ConduitResult _leftoverI c <- conduitPushClose inner inputI
                    return $ ConduitResult (Done leftoverO) c
        , conduitClose = do
            ConduitResult leftoverO b <- conduitClose outer
            ConduitResult _leftoverI c <- conduitPushClose inner b
            return $ ConduitResult leftoverO c
        }

-- | Push some data to a conduit, then close it if necessary.
conduitPushClose :: Monad m => PreparedConduit a m b -> [a] -> ResourceT m (ConduitResult [a] b)
conduitPushClose c [] = conduitClose c
conduitPushClose c input = do
    res <- conduitPush c input
    case res of
        ConduitResult (Done a) b -> return $ ConduitResult a b
        ConduitResult Processing b -> do
            ConduitResult a b' <- conduitClose c
            return $ ConduitResult a $ b ++ b'

sequence :: Resource m
         => Sink a m b
         -> Conduit a m b
sequence (Sink sm) = Conduit $ do
    sink <- sm
    prepareConduit $ conduitState sink push close
  where
    push sink input = push' sink input id

    push' (SinkNoData output) input frontO = do
        sink <- sm
        push' sink input (frontO . (output:))
    push' sink@(SinkData p _) input frontO = do
        mres <- p input
        case mres of
            Processing -> return (sink, ConduitResult Processing $ frontO [])
            Done (SinkResult leftover res) -> do
                sink' <- sm
                push' sink' leftover $ frontO . (res:)
    close (SinkNoData output) = return $ ConduitResult [] [output]
    close (SinkData _ c) = do
        SinkResult leftover res <- c
        return $ ConduitResult leftover [res]
