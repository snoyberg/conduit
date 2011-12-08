{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit
    ( -- * Helper types
      Stream (..)
    , SinkResult (..)
    , ConduitResult (..)
      -- * Main types
    , Source (..)
    , Sink (..)
    , Conduit
      -- * Connect pieces together
    , connect
    , connectLeft
    , connectRight
      -- * Helper functions
    , mkSource
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (liftBase)
import Control.Exception.Lifted (throwIO, Exception)
import qualified Data.IORef as I
import Data.Typeable (Typeable)
import Control.Monad (liftM)

data Stream a = EOF | Chunks [a]
    deriving Show
instance Functor Stream where
    fmap _ EOF = EOF
    fmap f (Chunks l) = Chunks (map f l)

data SinkResult input output = SinkResult [input] (Maybe output)
instance Functor (SinkResult input) where
    fmap f (SinkResult input moutput) = SinkResult input (fmap f moutput)

data ConduitResult input output = ConduitResult [input] (Stream output)

data SourceInside m a = SourceInside
    { sourcePull :: ResourceT m (Stream a)
    , sourcePush :: [a] -> ResourceT m ()
    }
newtype Source m a = Source (ResourceT m (SourceInside m a))

type SinkInside a m b = (Stream a -> ResourceT m (SinkResult a b))
newtype Sink input m output = Sink (ResourceT m (SinkInside input m output))
instance Monad m => Functor (Sink input m) where
    fmap f (Sink msink) = Sink $ do
        sink <- msink
        return $ \stream -> liftM (fmap f) (sink stream)

type Conduit a m b = Stream a -> ResourceT m (ConduitResult a b)

mkSource :: MonadBaseControl IO m
         => ResourceT m (Stream a)
         -> Source m a
mkSource pull = Source $ do
    buffer <- liftBase $ I.newIORef []
    return SourceInside
        { sourcePull = do
            x <- liftBase $ I.atomicModifyIORef buffer (\a -> ([], a))
            if null x
                then pull
                else return $ Chunks x
        , sourcePush = \s ->
            if null s
                then return ()
                else liftBase $
                    I.atomicModifyIORef buffer (\a -> (s ++ a, ()))
        }

connect :: MonadBaseControl IO m => Source m a -> Sink a m b -> ResourceT m b
connect (Source msource) (Sink msink) = do
    sink <- msink
    source <- msource
    connect' source sink
  where
    connect' source sink = do
        stream <- sourcePull source
        SinkResult leftover mres <- sink stream
        sourcePush source leftover
        case (stream, mres) of
            (_, Just res) -> return res
            (Chunks _, Nothing) -> connect' source sink
            (EOF, Nothing) -> liftBase (throwIO NothingAfterEOF)

data NothingAfterEOF = NothingAfterEOF
    deriving (Show, Typeable)
instance Exception NothingAfterEOF

connectLeft :: MonadBaseControl IO m
            => Source m a
            -> Conduit a m b
            -> Source m b
connectLeft (Source msource) pipe = Source $ do
    source <- msource
    source' <- unSource $ mkSource $ do
        ConduitResult leftover result <- sourcePull source >>= pipe
        sourcePush source leftover
        return result
    return source'
  where
    unSource (Source s) = s

connectRight :: MonadBaseControl IO m => Conduit a m b -> Sink b m c -> Sink a m c
connectRight pipe (Sink msink) = Sink $ do
    sink <- msink
    return $ \stream -> do
        ConduitResult leftover stream' <- pipe stream
        SinkResult _thisislost mres <- sink stream'
        return $ SinkResult leftover mres
