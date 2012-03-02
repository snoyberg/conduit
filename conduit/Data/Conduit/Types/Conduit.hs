-- | Defines the types for a conduit, which is a transformer of data. A conduit
-- is almost always connected either left (to a source) or right (to a sink).
module Data.Conduit.Types.Conduit
    ( ConduitResult (..)
    , Conduit (..)
    , ConduitPush
    , ConduitPull
    , ConduitClose
    ) where

import Control.Monad (liftM)
import Data.Conduit.Types.Source

-- | The value of the @conduitPush@ record.
type ConduitPush input m output = input -> m (ConduitResult input m output)

type ConduitPull input m output = m (ConduitResult input m output)

-- | The value of the @conduitClose@ record.
type ConduitClose m output = Source m output

-- | When data is pushed to a @Conduit@, it may either indicate that it is
-- still producing output and provide some, or indicate that it is finished
-- producing output, in which case it returns optional leftover input and some
-- final output.
--
-- The @Producing@ constructor provides a new @Conduit@ to be used in place of
-- the previous one.
--
-- Since 0.2.0
data ConduitResult input m output =
    Running (ConduitPush input m output) (ConduitClose m output)
  | Finished (Maybe input)
  | HaveMore (ConduitPull input m output) (m ()) output

instance Monad m => Functor (ConduitResult input m) where
    fmap f (Running p c) = Running (liftM (fmap f) . p) (fmap f c)
    fmap _ (Finished i) = Finished i
    fmap f (HaveMore pull close output) = HaveMore
        (liftM (fmap f) pull) close (f output)

-- | A conduit has two operations: it can receive new input (a push), and can
-- be closed.
--
-- Since 0.2.0
data Conduit input m output = Conduit
    { conduitPush :: ConduitPush input m output
    , conduitClose :: ConduitClose m output
    }

instance Monad m => Functor (Conduit input m) where
    fmap f c = c
        { conduitPush = liftM (fmap f) . conduitPush c
        , conduitClose = fmap f (conduitClose c)
        }
