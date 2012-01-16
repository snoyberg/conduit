{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | The main module, exporting types, utility functions, and fuse and connect
-- operators.
module Data.Conduit
    ( -- * Types
      -- ** Source
      module Data.Conduit.Types.Source
      -- ** Sink
    , module Data.Conduit.Types.Sink
      -- ** Conduit
    , module Data.Conduit.Types.Conduit
    , -- * Connect/fuse operators
      ($$)
    , ($=)
    , (=$)
    , (=$=)
      -- * Utility functions
      -- ** Source
    , module Data.Conduit.Util.Source
      -- ** Sink
    , module Data.Conduit.Util.Sink
      -- ** Conduit
    , module Data.Conduit.Util.Conduit
      -- * Convenience re-exports
    , ResourceT
    , Resource (..)
    , ResourceIO
    , ResourceUnsafeIO
    , runResourceT
    , ResourceThrow (..)
      -- * experimental
    , BufferedSource2 (..)
    , bufferSource2
    , connect2
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Types.Source hiding (BufferSource (..))
import Data.Conduit.Types.Source (BufferSource (bufferSource))
import qualified Data.Conduit.Types.Source
import Data.Conduit.Util.Source
import Data.Conduit.Types.Sink
import Data.Conduit.Util.Sink
import Data.Conduit.Types.Conduit
import Data.Conduit.Util.Conduit

infixr 0 $$

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- There are three ways this process can terminate:
--
-- 1. In the case of a @SinkNoData@ constructor, the source is not opened at
-- all, and the output value is returned immediately.
--
-- 2. The sink returns @Done@, in which case any leftover input is returned via
-- @bsourceUnpull@ the source is closed.
--
-- 3. The source return @Closed@, in which case the sink is closed.
--
-- Note that the input source is converted to a 'BufferedSource' via
-- 'bufferSource'. As such, if the input to this function is itself a
-- 'BufferedSource', the call to 'bsourceClose' will have no effect, as
-- described in the comments on that instance.
($$) :: (BufferSource bsrc, Resource m) => bsrc m a -> Sink a m b -> ResourceT m b
bs' $$ Sink msink = do
    sinkI <- msink
    case sinkI of
        SinkNoData output -> return output
        SinkData push close -> do
            bs <- Data.Conduit.Types.Source.unsafeBufferSource bs'
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
{-# SPECIALIZE ($$) :: Resource m => Source m a -> Sink a m b -> ResourceT m b #-}

data FuseLeftState a = FLClosed [a] | FLOpen [a]

infixl 1 $=

-- | Left fuse, combining a source and a conduit together into a new source.
($=) :: (Resource m, BufferSource bsrc)
     => bsrc m a
     -> Conduit a m b
     -> Source m b
bsrc' $= Conduit mc = Source $ do
    istate <- newRef $ FLOpen [] -- still open, no buffer
    bsrc <- Data.Conduit.Types.Source.unsafeBufferSource bsrc'
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
                mres <- bsourcePull bsrc
                case mres of
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

-- | Right fuse, combining a conduit and a sink together into a new sink.
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
                                        _ <- conduitClose c
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

-- | Middle fuse, combining two conduits together into a new conduit.
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
                                Finished _leftover c -> do
                                    _ <- conduitClose outer
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
        Finished _ b -> return b
        Producing b -> do
            b' <- conduitPushClose c rest
            return $ b ++ b'

data BufferedSource2 m a = BufferedSource2
    { bsSource :: PreparedSource m a
    , bsBuffer :: Ref (Base m) (BSState a)
    }

data BSState a = ClosedEmpty | OpenEmpty | ClosedFull a | OpenFull a

bufferSource2 :: Resource m => Source m a -> ResourceT m (BufferedSource2 m a)
bufferSource2 (Source msrc) = do
    src <- msrc
    buf <- newRef OpenEmpty
    return $ BufferedSource2 src buf

connect2 :: Resource m => BufferedSource2 m a -> Sink a m b -> ResourceT m b
connect2 bs (Sink msink) = do
    sinkI <- msink
    case sinkI of
        SinkNoData output -> return output
        SinkData push close -> do
            bsState <- readRef $ bsBuffer bs
            case bsState of
                ClosedEmpty -> close
                OpenEmpty -> connect' push close
                ClosedFull a -> do
                    res <- push a
                    case res of
                        Done mleftover res' -> do
                            writeRef (bsBuffer bs) $ maybe ClosedEmpty ClosedFull mleftover
                            return res'
                        Processing -> do
                            writeRef (bsBuffer bs) ClosedEmpty
                            close
                OpenFull a -> push a >>= onRes (connect' push close)
  where
    connect' push close =
        loop
      where
        loop = do
            res <- sourcePull $ bsSource bs
            case res of
                Closed -> do
                    res' <- close
                    return res'
                Open a -> push a >>= onRes loop
    onRes _ (Done mleftover res) = do
        writeRef (bsBuffer bs) (maybe OpenEmpty OpenFull mleftover)
        return res
    onRes loop Processing = loop
