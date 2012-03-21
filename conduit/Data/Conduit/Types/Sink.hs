{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Defines the types for a sink, which is a consumer of data.
module Data.Conduit.Types.Sink
    ( Sink (..)
    , SinkPush
    , SinkClose
    ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (liftBase))

-- | Push a value into a @Sink@ and get a new @Sink@ as a result.
type SinkPush input m output = input -> Sink input m output

-- | Closing a @Sink@ returns the final output.
type SinkClose m output = m output

-- | In general, a sink will consume data and eventually produce an output when
-- it has consumed \"enough\" data. There are two caveats to that statement:
--
-- * Some sinks do not actually require any data to produce an output. This is
-- included with a sink in order to allow for a 'Monad' instance.
--
-- * Some sinks will consume all available data and only produce a result at
-- the \"end\" of a data stream (e.g., @sum@).
--
-- Note that you can indicate any leftover data from processing via the @Maybe
-- input@ field of the @Done@ constructor. However, it is a violation of the
-- @Sink@ invariants to return leftover data when no input has been consumed.
-- Concrete, that means that a function like yield is invalid:
--
-- > yield :: input -> Sink input m ()
-- > yield input = Done (Just input) ()
--
-- A @Sink@ should clean up any resources it has allocated when it returns a
-- value.
--
-- Since 0.3.0
data Sink input m output =
    Processing (SinkPush input m output) (SinkClose m output) -- ^ Awaiting more input.
  | Done (Maybe input) output -- ^ Processing complete.
  | SinkM (m (Sink input m output)) -- ^ Perform some monadic action to continue.

instance Monad m => Functor (Sink input m) where
    fmap f (Processing push close) = Processing (fmap f . push) (liftM f close)
    fmap f (Done minput output) = Done minput (f output)
    fmap f (SinkM msink) = SinkM (liftM (fmap f) msink)

instance Monad m => Applicative (Sink input m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Sink input m) where
    return = Done Nothing
    Done Nothing x >>= f = f x
    Done (Just leftover) x >>= f =
        sinkPush (f x)
      where
        sinkPush (Processing push _) = push leftover
        sinkPush (Done Nothing output) = Done (Just leftover) output
        sinkPush (Done Just{} _) = error $ "Sink invariant violated: leftover input returned without any push"
        sinkPush (SinkM msink) = SinkM (liftM sinkPush msink)
    SinkM msink >>= f = SinkM (liftM (>>= f) msink)
    Processing push close >>= f = Processing
        (\input -> push input >>= f)
        (close >>= sinkClose . f)

sinkClose :: Monad m => Sink input m output -> m output
sinkClose (Done _ output) = return output
sinkClose (Processing _ close) = close
sinkClose (SinkM msink) = msink >>= sinkClose

instance MonadBase base m => MonadBase base (Sink input m) where
    liftBase = lift . liftBase

instance MonadTrans (Sink input) where
    lift = SinkM . liftM return

instance MonadIO m => MonadIO (Sink input m) where
    liftIO = lift . liftIO
