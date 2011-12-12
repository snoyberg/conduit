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
        stream <- bsourcePull bs
        case stream of
            EOF -> do
                SinkResult leftover res <- close
                bsourceUnpull bs leftover
                return res
            Chunks a -> do
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
    istate <- liftBase $ I.newIORef False
    bsrc <- bsourceM srcm
    c <- mc
    return Source
        { sourcePull = do
            state <- liftBase $ I.readIORef istate
            if state
                -- already closed
                then return EOF
                else do
                    stream <- bsourcePull bsrc
                    case stream of
                        EOF -> do
                            liftBase $ I.writeIORef istate True
                            o <- conduitClose c
                            if null o
                                then return EOF
                                else return $ Chunks o
                        Chunks cs -> do
                            ConduitResult leftover output <-
                                conduitPush c cs
                            bsourceUnpull bsrc leftover
                            case output of
                                EOF -> do
                                    bsourceClose bsrc
                                    liftBase $ I.writeIORef istate True
                                    return EOF
                                Chunks _ -> return output
        , sourceClose = do
            -- Invariant: sourceClose cannot be called twice, so we will assume
            -- it is currently open. We could add a sanity check here.
            liftBase $ I.writeIORef istate True
            _ignored <- conduitClose c
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
                sinput <- bsourcePull bsrc
                case sinput of
                    Chunks input -> do
                        ConduitResult leftover output' <- bconduitPush bcon input
                        bsourceUnpull bsrc leftover
                        return output'
                    EOF -> do
                        s <- bconduitClose bcon
                        return $ if null s then EOF else Chunks s
            else return $ Chunks output
    , bsourceUnpull = bconduitUnpull bcon
    , bsourceClose = do
        _ignored <- bconduitClose bcon
        bsourceClose bsrc
    }
    {-
    source' <- unSource $ mkSource $ do
        ConduitResult leftover result <- sourcePull source >>= pipe
        sourcePush source leftover
        return result
    return source'
    -}

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
        ConduitResult leftover stream' <- bconduitPush c stream
        case stream' of
            Chunks cs -> do
                SinkResult leftover' mres <- pushI cs
                bconduitUnpull c leftover'
                return $ SinkResult leftover mres
            EOF -> do
                SinkResult leftover' res <- closeI
                bconduitUnpull c leftover'
                return $ SinkResult leftover (Just res)
    close closeI = do
        SinkResult leftover res <- closeI
        bconduitUnpull c leftover
        return $ SinkResult [] res

sequence :: MonadBaseControl IO m
         => SinkM a m b
         -> ConduitM a m b
sequence (SinkM sm) = ConduitM $ do
    sink <- sm
    genConduit $ conduitMState sink push close
  where
    push (SinkNoData output) input = do
        sink <- sm
        return (sink, ConduitResult input $ Chunks [output])
    push sink@(SinkData p _) input = do
        SinkResult leftover mres <- p input
        case mres of
            Nothing -> return (sink, ConduitResult leftover $ Chunks [])
            Just res -> do
                sink' <- sm
                return (sink', ConduitResult leftover $ Chunks [res])
    close (SinkNoData output) = return [output]
    close (SinkData _ c) = do
        SinkResult _ res <- c
        return [res]
