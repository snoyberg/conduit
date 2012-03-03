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

-- | The value of the @sinkPush@ record.
type SinkPush input m output = input -> Sink input m output

-- | The value of the @sinkClose@ record.
type SinkClose m output = m output

{-
Note to my future self, and anyone else who reads my code: It's tempting to
change `Sink` to look like:

    newtype Sink input m output = Sink { runSink :: ResourceT m (SinkResult input m output) }

If you start implementing this, eventually you'll realize that you will have to
enforce an invariant to make it all work: a `SinkResult` can't return leftovers
unless data was pushed to it.

The idea is that, with the actual definition of `Sink`, it's impossible to get
a `SinkResult` without first pushing in some input. Therefore, it's always
valid at the type level to return leftovers. In this simplified `Sink`, it
would be possible to have code that looks like:

    sink1 = Sink $ return $ Done (Just "foo") ()
    fsink2 () = Sink $ return $ Done (Just "bar") ()
    sink1 >>= fsink2

Now we'd have to coalesce "foo" and "bar" together (e.g., require `Monoid`),
throw away data, or throw an exception.

So the current three-constructor approach to `Sink` may not be as pretty, but
it enforce the invariants much better.
-}

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
    Processing (SinkPush input m output) (SinkClose m output)
  | Done (Maybe input) output
  | SinkM (m (Sink input m output))

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
