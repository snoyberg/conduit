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
                            maybe (return ()) (bsourceUnpull bs) leftover
                            bsourceClose bs
                            return res'
                        Processing -> loop

data FuseLeftState a = FLClosed [a] | FLOpen [a]

infixl 1 $=

($=) :: (Resource m, BufferSource bsrc)
     => bsrc m a
     -> Conduit a m b
     -> Source m b
bsrc' $= Conduit mc = Source $ do
    istate <- newRef $ FLOpen [] -- still open, no buffer
    bsrc <- bufferSource bsrc'
    c <- mc
    return $ PreparedSource
        (pull istate bsrc c)
        (close istate bsrc c)
  where
    pull istate bsrc c = do
        state' <- readRef istate
        case state' of
            FLClosed [] -> return Closed
            FLClosed (x:xs) -> do
                writeRef istate $ FLClosed xs
                return $ Open x
            FLOpen (x:xs) -> do
                writeRef istate $ FLOpen xs
                return $ Open x
            FLOpen [] -> do
                res <- bsourcePull bsrc
                case res of
                    Closed -> do
                        res <- conduitClose c
                        case res of
                            [] -> do
                                writeRef istate $ FLClosed []
                                return Closed
                            x:xs -> do
                                writeRef istate $ FLClosed xs
                                return $ Open x
                    Open input -> do
                        res' <- conduitPush c input
                        case res' of
                            Producing [] -> pull istate bsrc c
                            Producing (x:xs) -> do
                                writeRef istate $ FLOpen xs
                                return $ Open x
                            Finished leftover output -> do
                                maybe (return ()) (bsourceUnpull bsrc) leftover
                                bsourceClose bsrc
                                case output of
                                    [] -> do
                                        writeRef istate $ FLClosed []
                                        return Closed
                                    x:xs -> do
                                        writeRef istate $ FLClosed xs
                                        return $ Open x
    close istate bsrc c = do
        -- Invariant: sourceClose cannot be called twice, so we will assume
        -- it is currently open. We could add a sanity check here.
        writeRef istate $ FLClosed []
        _ignored <- conduitClose c
        bsourceClose bsrc

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
                        let push [] = return Processing
                            push (i:is) = do
                                mres <- pushI i
                                case mres of
                                    Processing -> push is
                                    Done _sleftover res' -> do
                                        conduitClose c
                                        return $ Done Nothing res'
                        push sinput
                    Finished cleftover sinput -> do
                        let push [] = closeI
                            push (i:is) = do
                                mres <- pushI i
                                case mres of
                                    Processing -> push is
                                    Done _sleftover res' -> return res'
                        res' <- push sinput
                        return $ Done cleftover res'
            , sinkClose = do
                sinput <- conduitClose c
                let push [] = closeI
                    push (i:is) = do
                        mres <- pushI i
                        case mres of
                            Processing -> push is
                            Done _sleftover res' -> return res'
                push sinput
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
                    let push [] front = return $ Producing $ front []
                        push (i:is) front = do
                            resI <- conduitPush inner i
                            case resI of
                                Producing c -> push is (front . (c ++))
                                Finished leftover c -> do
                                    conduitClose outer
                                    return $ Finished Nothing $ front c
                    push inputI id
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
conduitPushClose c (input:rest) = do
    res <- conduitPush c input
    case res of
        Finished a b -> return b
        Producing b -> do
            b' <- conduitPushClose c rest
            return $ b ++ b'
