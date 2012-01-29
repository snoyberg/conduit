{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Defines the types for a sink, which is a consumer of data.
module Data.Conduit.Types.Sink
    ( SinkResult (..)
    , Sink (..)
    , SinkPush
    , SinkClose
    ) where

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (liftBase))

-- | The value of the @sinkPush@ record.
type SinkPush input m output = input -> ResourceT m (SinkResult input m output)

-- | The value of the @sinkClose@ record.
type SinkClose m output = ResourceT m output

-- | A @Sink@ ultimately returns a single output value. Each time data is
-- pushed to it, a @Sink@ may indicate that it is still processing data, or
-- that it is done, in which case it returns some optional leftover input and
-- an output value.
--
-- The @Processing@ constructors provides updated push and close functions to
-- be used in place of the original @Sink@.
--
-- Since 0.2.0
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
-- A @Sink@ should clean up any resources it has allocated when it returns a
-- value, whether that be via @sinkPush@ or @sinkClose@.
--
-- Since 0.2.0
data Sink input m output =
    SinkNoData output
  | SinkData
        { sinkPush :: SinkPush input m output
        , sinkClose :: SinkClose m output
        }
  -- | This constructor is provided to allow us to create an efficient
  -- @MonadTrans@ instance.
  | SinkMonad (m (Sink input m output))

instance Monad m => Functor (Sink input m) where
    fmap f (SinkNoData x) = SinkNoData (f x)
    fmap f (SinkData p c) = SinkData
        { sinkPush = liftM (fmap f) . p
        , sinkClose = liftM f c
        }
    fmap f (SinkMonad msink) = SinkMonad (liftM (fmap f) msink)

instance Resource m => Applicative (Sink input m) where
    pure = return
    (<*>) = ap

instance Resource m => Monad (Sink input m) where
    return = SinkNoData
    SinkNoData x >>= f = f x
    SinkMonad mx >>= f = SinkMonad $ do
        x <- mx
        return $ x >>= f
    SinkData push0 close0 >>= f =
        SinkData (push push0) (close close0)
      where
        push push' input = do
            res <- push' input
            case res of
                Done lo output -> pushHelper lo (f output)
                Processing push'' close'' ->
                    return $ Processing (push push'') (close close'')

        pushHelper lo (SinkNoData y) = return $ Done lo y
        pushHelper (Just l) (SinkData pushF _) = pushF l
        pushHelper Nothing (SinkData pushF closeF) =
            return (Processing pushF closeF)
        pushHelper lo (SinkMonad msink) = lift msink >>= pushHelper lo

        close close' = do
            output <- close'
            closeHelper (f output)

        closeHelper (SinkNoData y) = return y
        closeHelper (SinkData _ closeF) = closeF
        closeHelper (SinkMonad msink) = lift msink >>= closeHelper

instance (Resource m, Base m ~ base, Applicative base) => MonadBase base (Sink input m) where
    liftBase = lift . resourceLiftBase

instance MonadTrans (Sink input) where
    lift = SinkMonad . liftM SinkNoData

instance (Resource m, MonadIO m) => MonadIO (Sink input m) where
    liftIO = lift . liftIO
