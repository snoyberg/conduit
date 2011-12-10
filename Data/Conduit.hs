{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit
    ( -- * Helper types
      ConduitResult (..)
      -- * Main types
    , Conduit
      -- * Connect pieces together
    , ($$)
    , (<$$>)
    , ($=)
    , (=$)
      -- * Other modules
      -- ** Source
    , module Data.Conduit.Types.Source
    , module Data.Conduit.Util.Source
      -- ** Sink
    , module Data.Conduit.Types.Sink
    , module Data.Conduit.Util.Sink
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit.Types.Source
import Data.Conduit.Util.Source
import Data.Conduit.Types.Sink
import Data.Conduit.Util.Sink

data ConduitResult input output = ConduitResult [input] (Stream output)

type Conduit a m b = Stream a -> ResourceT m (ConduitResult a b)

infixr 0 $$

($$) :: MonadBaseControl IO m => Source m a -> SinkM a m b -> ResourceT m b
source $$ SinkM msink = do
    sinkI <- msink
    case sinkI of
        SinkNoData output -> return output
        SinkData push close -> do
            bs <- bsource source
            connect' bs push close
  where
    connect' bs push close = do
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
                    Nothing -> connect' bs push close

infixr 0 <$$>

(<$$>) :: MonadBaseControl IO m => SourceM m a -> SinkM a m b -> ResourceT m b
SourceM msrc <$$> sink = msrc >>= ($$ sink)

infixl 1 $=

($=) :: MonadBaseControl IO m
     => Source m a
     -> Conduit a m b
     -> Source m b
_ $= _ =
    undefined
    {-
    source' <- unSource $ mkSource $ do
        ConduitResult leftover result <- sourcePull source >>= pipe
        sourcePush source leftover
        return result
    return source'
    -}

infixr 0 =$

(=$) :: MonadBaseControl IO m => Conduit a m b -> SinkM b m c -> SinkM a m c
_ =$ _ = SinkM $ do
    undefined {-
    case sinkI of
        SinkData sink -> return $ SinkData $ \stream -> do
            ConduitResult leftover stream' <- pipe stream
            SinkResult _thisislost mres <- sink stream'
            return $ SinkResult leftover mres
        SinkNoData mres -> return $ SinkNoData mres
        -}
