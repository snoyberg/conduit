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
    , ($$)
    , ($=)
    , (=$)
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
import Control.Applicative (Applicative (..))

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
instance MonadBaseControl IO m => Applicative (Sink input m) where
    pure x =
        Sink (return (\s -> return (SinkResult (toList s) (Just x))))
      where
        toList EOF = []
        toList (Chunks a) = a
    Sink mf <*> Sink ma = Sink $ do
        istate <- liftBase $ I.newIORef Nothing
        f <- mf
        a <- ma
        return $ appHelper istate f a

appHelper :: MonadBaseControl IO m
          => I.IORef (Maybe (Either (b -> c) ()))
          -> SinkInside a m (b -> c)
          -> SinkInside a m b
          -> SinkInside a m c
appHelper istate f a stream = do
    state <- liftBase $ I.readIORef istate
    go state stream
  where
    go Nothing stream = do
        SinkResult leftover mres <- f stream
        case mres of
            Nothing -> return $ SinkResult leftover Nothing
            Just res -> do
                let state' = Just $ Left res
                liftBase $ I.writeIORef istate state'
                if null leftover
                    then return $ SinkResult [] Nothing
                    else go state' $ Chunks leftover

instance MonadBaseControl IO m => Monad (Sink input m) where
    return = pure
    x >>= f = sinkJoin (fmap f x)

sinkJoin :: MonadBaseControl IO m => Sink a m (Sink a m b) -> Sink a m b
sinkJoin (Sink mmsink) = Sink $ do
    istate <- liftBase $ I.newIORef Nothing
    msink <- mmsink
    return $ go istate msink
  where
    go :: MonadBaseControl IO m => I.IORef (Maybe (SinkInside a m b)) -> SinkInside a m (Sink a m b) -> SinkInside a m b
    go istate outer stream = do
        state <- liftBase $ I.readIORef istate
        case state of
            Nothing -> do
                SinkResult leftover minner <- outer stream
                case minner of
                    Just (Sink minner) -> do
                        inner <- minner
                        liftBase $ I.writeIORef istate $ Just inner
                        if null leftover
                            then return $ SinkResult [] Nothing
                            else go istate outer $ Chunks leftover
                    Nothing -> return $ SinkResult leftover Nothing
            Just inner -> inner stream

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

infixr 0 $$

($$) :: MonadBaseControl IO m => Source m a -> Sink a m b -> ResourceT m b
Source msource $$ Sink msink = do
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

infixl 1 $=

($=) :: MonadBaseControl IO m
     => Source m a
     -> Conduit a m b
     -> Source m b
Source msource $= pipe = Source $ do
    source <- msource
    source' <- unSource $ mkSource $ do
        ConduitResult leftover result <- sourcePull source >>= pipe
        sourcePush source leftover
        return result
    return source'
  where
    unSource (Source s) = s

infixr 0 =$

(=$) :: MonadBaseControl IO m => Conduit a m b -> Sink b m c -> Sink a m c
pipe =$ Sink msink = Sink $ do
    sink <- msink
    return $ \stream -> do
        ConduitResult leftover stream' <- pipe stream
        SinkResult _thisislost mres <- sink stream'
        return $ SinkResult leftover mres
