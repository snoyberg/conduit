-- | Defines the types for a conduit, which is a transformer of data. A conduit
-- is almost always connected either left (to a source) or right (to a sink).
module Data.Conduit.Types.Conduit
    ( ConduitResult (..)
    , PreparedConduit (..)
    , Conduit (..)
    , ConduitPush
    , ConduitClose
    ) where

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad (liftM)

type ConduitPush input m output = input -> ResourceT m (ConduitResult input m output)
type ConduitClose m output = ResourceT m [output]

-- | When data is pushed to a @Conduit@, it may either indicate that it is
-- still producing output and provide some, or indicate that it is finished
-- producing output, in which case it returns optional leftover input and some
-- final output.
--
-- Since 0.0.0
data ConduitResult input m output =
    Producing (ConduitPush input m output) (ConduitClose m output) [output]
  | Finished (Maybe input) [output]

instance Monad m => Functor (ConduitResult input m) where
    fmap f (Producing p c o) = Producing ((fmap . fmap . fmap) f p) ((fmap . fmap) f c) (fmap f o)
    fmap f (Finished i o) = Finished i (fmap f o)

-- | A conduit has two operations: it can receive new input (a push), and can
-- be closed.
--
-- Invariants:
--
-- * Neither a push nor close may be performed after a conduit returns a
-- 'Finished' from a push, or after a close is performed.
--
-- Since 0.0.0
data PreparedConduit input m output = PreparedConduit
    { conduitPush :: ConduitPush input m output
    , conduitClose :: ConduitClose m output
    }

instance Monad m => Functor (PreparedConduit input m) where
    fmap f c = c
        { conduitPush = liftM (fmap f) . conduitPush c
        , conduitClose = liftM (fmap f) (conduitClose c)
        }

-- | A monadic action generating a 'PreparedConduit'. See @Source@ and @Sink@
-- for more motivation.
--
-- Since 0.0.0
newtype Conduit input m output =
    Conduit { prepareConduit :: ResourceT m (PreparedConduit input m output) }

instance Monad m => Functor (Conduit input m) where
    fmap f (Conduit mc) = Conduit (liftM (fmap f) mc)
