{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit
    ( -- * Helper types
      Stream (..)
    , SinkResult (..)
    , ConduitResult (..)
      -- * Main types
    , Source (..)
    , Sink (..)
    , SinkInside (..)
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
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Exception.Lifted (throwIO, Exception)
import qualified Data.IORef as I
import Data.Typeable (Typeable)
import Control.Monad (liftM)
import Control.Applicative (Applicative (..))
import Data.Monoid (Monoid (mempty, mappend, mconcat))

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
newtype Source m a = Source { unSource :: ResourceT m (SourceInside m a) }
instance MonadBaseControl IO m => Monoid (Source m a) where
    mempty = mkSource $ return EOF
    mappend a b = mconcat [a, b]
    mconcat [] = mempty
    mconcat (Source mnext:rest0) = Source $ do
        next0 <- mnext
        istate <- liftBase $ I.newIORef $ Just (next0, rest0)
        unSource $ mkSource $ go istate
      where
        go istate = do
            state <- liftBase $ I.readIORef istate
            case state of
                Nothing -> return EOF
                Just (next, rest) -> do
                    stream <- sourcePull next
                    case stream of
                        EOF ->
                            case rest of
                                Source a:as -> do
                                    a' <- a
                                    liftBase $ I.writeIORef istate $ Just (a', as)
                                    go istate
                                [] -> do
                                    liftBase $ I.writeIORef istate Nothing
                                    return EOF
                        Chunks _ -> return stream

type SinkInsideData a m b = Stream a -> ResourceT m (SinkResult a b)
data SinkInside a m b =
    SinkData (SinkInsideData a m b)
  | SinkNoData b
newtype Sink input m output = Sink (ResourceT m (SinkInside input m output))
instance Monad m => Functor (Sink input m) where
    fmap f (Sink msink) = Sink $ do
        sinkI <- msink
        return $ case sinkI of
            SinkData sink -> SinkData $ liftM (fmap f) . sink
            SinkNoData x -> SinkNoData $ f x
instance MonadBaseControl IO m => Applicative (Sink input m) where
    pure x = Sink $ return $ SinkNoData x
    Sink mf <*> Sink ma = Sink $ do
        f <- mf
        a <- ma
        case (f, a) of
            (SinkNoData f', SinkNoData a') -> return $ SinkNoData (f' a')
            _ -> do
                let toEither (SinkData x) = Left x
                    toEither (SinkNoData x) = Right x
                istate <- liftBase $ I.newIORef (toEither f, toEither a)
                return $ appHelper istate

appHelper :: MonadBaseControl IO m
          => I.IORef
                ( Either (SinkInsideData input m (a -> b)) (a -> b)
                , Either (SinkInsideData input m a) a
                )
          -> SinkInside input m b
appHelper istate = SinkData $ \stream0 -> do
    state <- liftBase $ I.readIORef istate
    go state stream0
  where
    go (Left f, eb) stream = do
        SinkResult leftover mres <- f stream
        case mres of
            Nothing -> return $ SinkResult leftover Nothing
            Just res -> do
                let state' = (Right res, eb)
                liftBase $ I.writeIORef istate state'
                go state' $ Chunks leftover
    go (f@Right{}, Left b) stream = do
        SinkResult leftover mres <- b stream
        case mres of
            Nothing -> return $ SinkResult leftover Nothing
            Just res -> do
                let state' = (f, Right res)
                liftBase $ I.writeIORef istate state'
                go state' $ Chunks leftover
    go (Right f, Right b) EOF = return $ SinkResult [] (Just $ f b)
    go (Right f, Right b) (Chunks s) = return $ SinkResult s (Just $ f b)

instance MonadBaseControl IO m => Monad (Sink input m) where
    return = pure
    x >>= f = sinkJoin (fmap f x)

instance MonadBaseControl IO m => MonadBase IO (Sink input m) where
    liftBase = lift . liftBase

instance MonadTrans (Sink input) where
    lift f = Sink (lift (liftM SinkNoData f))

sinkJoin :: MonadBaseControl IO m => Sink a m (Sink a m b) -> Sink a m b
sinkJoin (Sink msinkI) = Sink $ do
    sinkI <- msinkI
    case sinkI of
        SinkNoData (Sink inner) -> inner
        SinkData msink -> do
            istate <- liftBase $ I.newIORef Nothing
            return $ SinkData $ go istate msink
  where
    go :: MonadBaseControl IO m
       => I.IORef (Maybe (SinkInsideData a m b))
       -> (Stream a -> ResourceT m (SinkResult a (Sink a m b)))
       -> SinkInsideData a m b
    go istate outer stream = do
        state <- liftBase $ I.readIORef istate
        case state of
            Nothing -> do
                SinkResult leftover minner' <- outer stream
                case minner' of
                    Just (Sink msink) -> do
                        sink <- msink
                        case sink of
                            SinkData inner -> do
                                liftBase $ I.writeIORef istate $ Just inner
                                if null leftover
                                    then return $ SinkResult [] Nothing
                                    else go istate outer $ Chunks leftover
                            SinkNoData x -> return $ SinkResult leftover $ Just x
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
    sinkI <- msink
    case sinkI of
        SinkNoData output -> return output
        SinkData sink -> do
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

infixr 0 =$

(=$) :: MonadBaseControl IO m => Conduit a m b -> Sink b m c -> Sink a m c
pipe =$ Sink msink = Sink $ do
    sinkI <- msink
    case sinkI of
        SinkData sink -> return $ SinkData $ \stream -> do
            ConduitResult leftover stream' <- pipe stream
            SinkResult _thisislost mres <- sink stream'
            return $ SinkResult leftover mres
        SinkNoData mres -> return $ SinkNoData mres
