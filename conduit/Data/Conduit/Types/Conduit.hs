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
type ConduitPush input m output = input -> Conduit input m output

-- | The value of the @conduitClose@ record.
type ConduitClose m output = Source m output

-- | A conduit has two operations: it can receive new input (a push), and can
-- be closed.
--
-- Since 0.3.0
data Conduit input m output =
    Running (ConduitPush input m output) (ConduitClose m output)
  | Finished (Maybe input)
  | HaveMore (Conduit input m output) (m ()) output
  | ConduitM (m (Conduit input m output)) (m ())

instance Monad m => Functor (Conduit input m) where
    fmap f (Running p c) = Running (fmap f . p) (fmap f c)
    fmap _ (Finished i) = Finished i
    fmap f (HaveMore pull close output) = HaveMore
        (fmap f pull) close (f output)
    fmap f (ConduitM mcon c) = ConduitM (liftM (fmap f) mcon) c
