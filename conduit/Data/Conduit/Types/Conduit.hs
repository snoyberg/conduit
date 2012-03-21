-- | Defines the types for a conduit, which is a transformer of data. A conduit
-- is almost always connected either left (to a source) or right (to a sink).
module Data.Conduit.Types.Conduit
    ( Conduit (..)
    , ConduitPush
    , ConduitClose
    ) where

import Control.Monad (liftM)
import Data.Conduit.Types.Source

-- | Pushing new data to a @Conduit@ produces a new @Conduit@.
--
-- Since 0.3.0
type ConduitPush input m output = input -> Conduit input m output

-- | When closing a @Conduit@, it can produce a final stream of values.
--
-- Since 0.3.0
type ConduitClose m output = Source m output

-- | A @Conduit@ allows data to be pushed to it, and for each new input, can
-- produce a stream of output values (possibly an empty stream). It can be
-- considered a hybrid of a @Sink@ and a @Source@.
--
-- A @Conduit@ has four constructors, corresponding to four distinct states of
-- operation.
--
-- Since 0.3.0
data Conduit input m output =
    NeedInput (ConduitPush input m output) (ConduitClose m output)
    -- ^ Indicates that the @Conduit@ needs more input in order to produce
    -- output. It also provides an action to close the @Conduit@ early, for
    -- cases when there is no more input available, or when no more output is
    -- requested. Closing at this point returns a @Source@ to allow for either
    -- consuming or ignoring the new stream.

  | HaveOutput (Conduit input m output) (m ()) output
    -- ^ Indicates that the @Conduit@ has more output available. It has three
    -- records: the next @Conduit@ to continue the stream, a close action for
    -- early termination, and the output currently available. Note that, unlike
    -- @NeedInput@, the close action here returns @()@ instead of @Source@. The
    -- reasoning is that @HaveOutput@ will only be closed early if no more
    -- output is requested, since no input is required.

  | Finished (Maybe input)
    -- ^ Indicates that no more output is available, and no more input may be
    -- sent. It provides an optional leftover input record. Note: It is a
    -- violation of @Conduit@'s invariants to return leftover output that was
    -- never consumed, similar to the invariants of a @Sink@.

  | ConduitM (m (Conduit input m output)) (m ())
    -- ^ Indicates that a monadic action must be taken to determine the next
    -- @Conduit@. It also provides an early close action. Like @HaveOutput@,
    -- this action returns @()@, since it should only be used when no more
    -- output is requested.

instance Monad m => Functor (Conduit input m) where
    fmap f (NeedInput p c) = NeedInput (fmap f . p) (fmap f c)
    fmap _ (Finished i) = Finished i
    fmap f (HaveOutput pull close output) = HaveOutput
        (fmap f pull) close (f output)
    fmap f (ConduitM mcon c) = ConduitM (liftM (fmap f) mcon) c
