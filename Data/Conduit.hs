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
                    res' <- close
                    return res'
                Open a -> do
                    mres <- push a
                    case mres of
                        Done leftover res' -> do
                            bsourceUnpull bs leftover
                            bsourceClose bs
                            return res'
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
                            res <- conduitClose c
                            return $ Open res
                        Open input -> do
                            res' <- conduitPush c input
                            case res' of
                                Producing output ->
                                    return $ Open output
                                Finished leftover output -> do
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
                    Producing sinput -> do
                        mres <- pushI sinput
                        case mres of
                            Processing -> return Processing
                            Done _sleftover res' -> do
                                conduitClose c
                                return $ Done [] res'
                    Finished cleftover sinput -> do
                        mres <- pushI sinput
                        res' <-
                            case mres of
                                Done _ x -> return x
                                Processing -> closeI
                        return $ Done cleftover res'
            , sinkClose = do
                sinput <- conduitClose c
                mres <- pushI sinput
                res <-
                    case mres of
                        Done _ x -> return x
                        Processing -> closeI
                return res
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
                Producing inputI -> do
                    resI <- conduitPush inner inputI
                    case resI of
                        Producing c ->
                            return $ Producing c
                        Finished _leftoverI c -> do
                            conduitClose outer
                            return $ Finished [] c
                Finished leftoverO inputI -> do
                    c <- conduitPushClose inner inputI
                    return $ Finished leftoverO c
        , conduitClose = do
            b <- conduitClose outer
            c <- conduitPushClose inner b
            return c
        }

-- | Push some data to a conduit, then close it if necessary.
conduitPushClose :: Monad m => PreparedConduit a m b -> [a] -> ResourceT m [b]
conduitPushClose c [] = conduitClose c
conduitPushClose c input = do
    res <- conduitPush c input
    case res of
        Finished a b -> return b
        Producing b -> do
            b' <- conduitClose c
            return $ b ++ b'

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
            Processing -> return (sink, Producing $ frontO [])
            Done leftover res -> do
                sink' <- sm
                push' sink' leftover $ frontO . (res:)
    close (SinkNoData output) = return [output]
    close (SinkData _ c) = do
        res <- c
        return [res]
