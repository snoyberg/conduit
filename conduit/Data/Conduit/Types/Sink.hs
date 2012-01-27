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
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (liftBase))

type SinkPush input m output = input -> ResourceT m (SinkResult input m output)
type SinkClose m output = ResourceT m output

-- | A @Sink@ ultimately returns a single output value. Each time data is
-- pushed to it, a @Sink@ may indicate that it is still processing data, or
-- that it is done, in which case it returns some optional leftover input and
-- an output value.
--
-- Since 0.0.0
data SinkResult input m output =
    Processing (SinkPush input m output) (SinkClose m output)
  | Done (Maybe input) output
instance Monad m => Functor (SinkResult input m) where
    fmap f (Processing push close) = Processing ((fmap . fmap . fmap) f push) (fmap f close)
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
-- * After a 'PreparedSink' produces a result (either via 'sinkPush' or
-- 'sinkClose'), neither of those two functions may be called on the @Sink@
-- again.
--
-- * If a @Sink@ needs to clean up any resources (e.g., close a file handle),
-- it must do so whenever it returns a result, either via @sinkPush@ or
-- @sinkClose@. Note that, due to usage of @ResourceT@, this is merely an
-- optimization.
--
-- Since 0.0.0
data PreparedSink input m output =
    SinkNoData output
  | SinkData
        { sinkPush :: SinkPush input m output
        , sinkClose :: SinkClose m output
        }

instance Monad m => Functor (PreparedSink input m) where
    fmap f (SinkNoData x) = SinkNoData (f x)
    fmap f (SinkData p c) = SinkData
        { sinkPush = liftM (fmap f) . p
        , sinkClose = liftM f c
        }

-- | Most 'PreparedSink's require some type of state, similar to
-- 'PreparedSource's. Like a @Source@ for a @PreparedSource@, a @Sink@ is a
-- simple monadic wrapper around a @PreparedSink@ which allows initialization
-- of such state. See @Source@ for further caveats.
--
-- Note that this type provides a 'Monad' instance, allowing you to easily
-- compose @Sink@s together.
--
-- Since 0.0.0
newtype Sink input m output = Sink { prepareSink :: ResourceT m (PreparedSink input m output) }

instance Monad m => Functor (Sink input m) where
    fmap f (Sink msink) = Sink (liftM (fmap f) msink)

instance Resource m => Applicative (Sink input m) where
    pure = return
    (<*>) = ap

instance Resource m => Monad (Sink input m) where
    return x = Sink (return (SinkNoData x))
    mx >>= f = Sink $ do
        x <- prepareSink mx
        case x of
            SinkNoData x' -> prepareSink $ f x'
            SinkData push' close' ->
                return $ SinkData (push push') (close close')
      where
        push push' input = do
            res <- push' input
            case res of
                Done leftover output -> do
                    f' <- prepareSink $ f output
                    case f' of
                        SinkNoData y ->
                            return $ Done leftover y
                        SinkData pushF closeF -> do
                            case leftover of
                                Nothing -> return $ Processing pushF closeF
                                Just l -> pushF l
                Processing push'' close'' ->
                    return $ Processing (push push'') (close close'')
        close close' = do
            output <- close'
            f' <- prepareSink $ f output
            case f' of
                SinkNoData y -> return y
                SinkData _ closeF -> closeF

instance (Resource m, Base m ~ base, Applicative base) => MonadBase base (Sink input m) where
    liftBase = lift . resourceLiftBase

instance MonadTrans (Sink input) where
    lift f = Sink (lift (liftM SinkNoData f))

instance (Resource m, MonadIO m) => MonadIO (Sink input m) where
    liftIO = lift . liftIO
