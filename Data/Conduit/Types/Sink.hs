{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Defines the types for a sink, which is a consumer of data.
module Data.Conduit.Types.Sink
    ( SinkResult (..)
    , PreparedSink (..)
    , Sink (..)
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (liftM)
import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (liftBase))

-- | At the end of running, a 'Sink' returns some leftover input and an output.
data SinkResult input output = Processing | Done (Maybe input) output
instance Functor (SinkResult input) where
    fmap _ Processing = Processing
    fmap f (Done input output) = Done input (f output)

-- | In general, a sink will consume data and eventually produce an output when
-- it has consumed \"enough\" data. There are two caveats to that statement:
--
-- * Some sinks do not actually require any data to produce an output. This is
-- included with a sink in order to allow for a 'Monad' instance.
--
-- * Some sinks will consume all available data and only produce a result at
-- the \"end\" of a data stream (e.g., @sum@).
--
-- To allow for the first caveat, we have the 'SinkNoData' constructor. For the
-- second, the 'SinkData' constructor has two records: one for receiving more
-- input, and the other to indicate the end of a stream. Note that, at the end
-- of a stream, some output is required. If a specific 'Sink' implementation
-- cannot always produce output, this should be indicated in its return value,
-- using something like a 'Maybe' or 'Either'.
--
-- Invariants:
--
-- * After a 'Sink' produces a result (either via 'sinkPush' or 'sinkClose'),
-- neither of those two functions may be called on the @Sink@ again.
--
-- * If a @Sink@ needs to clean up any resources (e.g., close a file handle),
-- it must do so whenever it returns a result, either via @sinkPush@ or
-- @sinkClose@.
data PreparedSink input m output =
    SinkNoData output
  | SinkData
        { sinkPush :: input -> ResourceT m (SinkResult input output)
        , sinkClose :: ResourceT m output
        }

instance Monad m => Functor (PreparedSink input m) where
    fmap f (SinkNoData x) = SinkNoData (f x)
    fmap f (SinkData p c) = SinkData
        { sinkPush = liftM (fmap f) . p
        , sinkClose = liftM f c
        }

-- | Most 'Sink's require some type of state, similar to 'Source's. Like a
-- @SourceM@ for a @Source@, a @Sink@ is a simple monadic wrapper around a
-- @Sink@ which allows initialization of such state. See @SourceM@ for further
-- caveats.
--
-- Note that this type provides a 'Monad' instance, allowing you to easily
-- compose @Sink@s together.
newtype Sink input m output = Sink { prepareSink :: ResourceT m (PreparedSink input m output) }

instance Monad m => Functor (Sink input m) where
    fmap f (Sink msink) = Sink (liftM (fmap f) msink)

instance Resource m => Applicative (Sink input m) where
    pure x = Sink (return (SinkNoData x))
    Sink mf <*> Sink ma = Sink $ do
        f <- mf
        a <- ma
        case (f, a) of
            (SinkNoData f', SinkNoData a') -> return (SinkNoData (f' a'))
            _ -> do
                istate <- newRef (toEither f, toEither a)
                return $ appHelper istate

toEither :: PreparedSink input m output -> SinkEither input m output
toEither (SinkData x y) = SinkPair x y
toEither (SinkNoData x) = SinkOutput x

type SinkPush input m output = input -> ResourceT m (SinkResult input output)
type SinkClose input m output = ResourceT m output
data SinkEither input m output
    = SinkPair (SinkPush input m output) (SinkClose input m output)
    | SinkOutput output
type SinkState input m a b = Ref (Base m) (SinkEither input m (a -> b), SinkEither input m a)

appHelper :: Resource m => SinkState input m a b -> PreparedSink input m b
appHelper istate = SinkData (pushHelper istate) (closeHelper istate)

pushHelper :: Resource m
           => SinkState input m a b
           -> input
           -> ResourceT m (SinkResult input b)
pushHelper istate stream0 = do
    state <- readRef istate
    go state stream0
  where
    go (SinkPair f _, eb) stream = do
        mres <- f stream
        case mres of
            Processing -> return Processing
            Done leftover res -> do
                let state' = (SinkOutput res, eb)
                writeRef istate state'
                maybe (return Processing) (go state') leftover
    go (f@SinkOutput{}, SinkPair b _) stream = do
        mres <- b stream
        case mres of
            Processing -> return Processing
            Done leftover res -> do
                let state' = (f, SinkOutput res)
                writeRef istate state'
                maybe (return Processing) (go state') leftover
    go (SinkOutput f, SinkOutput b) leftover = return $ Done (Just leftover) $ f b

closeHelper :: Resource m
            => SinkState input m a b
            -> ResourceT m b
closeHelper istate = do
    (sf, sa) <- readRef istate
    case sf of
        SinkOutput f -> go' f sa
        SinkPair _ close -> do
            f <- close
            go' f sa
  where
    go' f (SinkPair _ close) = do
        a <- close
        return (f a)
    go' f (SinkOutput a) = return (f a)

instance Resource m => Monad (Sink input m) where
    return = pure
    mx >>= f = Sink $ do
        x <- prepareSink mx
        case x of
            SinkNoData x' -> prepareSink $ f x'
            SinkData push' close' -> do
                istate <- newRef $ Left (push', close')
                return $ SinkData (push istate) (close istate)
      where
        push istate input = do
            state <- readRef istate
            case state of
                Left (push', _) -> do
                    res <- push' input
                    case res of
                        Done leftover output -> do
                            f' <- prepareSink $ f output
                            case f' of
                                SinkNoData y ->
                                    return $ Done leftover y
                                SinkData pushF closeF -> do
                                    writeRef istate $ Right (pushF, closeF)
                                    maybe (return Processing) (push istate) leftover
                        Processing -> return Processing
                Right (push', _) -> push' input
        close istate = do
            state <- readRef istate
            case state of
                Left (_, close') -> do
                    output <- close'
                    f' <- prepareSink $ f output
                    case f' of
                        SinkNoData y -> return y
                        SinkData _ closeF -> closeF
                Right (_, close') -> close'

instance (Resource m, Base m ~ base, Applicative base) => MonadBase base (Sink input m) where
    liftBase = lift . resourceLiftBase

instance MonadTrans (Sink input) where
    lift f = Sink (lift (liftM SinkNoData f))

instance (Resource m, MonadIO m) => MonadIO (Sink input m) where
    liftIO = lift . liftIO
