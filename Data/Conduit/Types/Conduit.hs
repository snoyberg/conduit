-- | Defines the types for a conduit, which is a transformer of data. A conduit
-- is almost always connected either left (to a source) or right (to a sink).
module Data.Conduit.Types.Conduit
    ( ConduitResult (..)
    , ConduitCloseResult (..)
    , Conduit (..)
    , ConduitM (..)
    ) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit.Types.Source (StreamState (..))
import Control.Monad (liftM)

-- | A conduit can return both leftover input data and a new stream of output
-- data, as well as signal that it will provide no more data (via
-- 'StreamClosed').
data ConduitResult input output = ConduitResult StreamState [input] [output]

instance Functor (ConduitResult input) where
    fmap f (ConduitResult s i o) = ConduitResult s i (fmap f o)

-- | When a conduit it closed, it can also return leftover input data and
-- output data. However, the output data always signifies an 'EOF', and
-- therefore we use a simple list.
data ConduitCloseResult input output = ConduitCloseResult [input] [output]

instance Functor (ConduitCloseResult input) where
    fmap f (ConduitCloseResult i o) = ConduitCloseResult i (fmap f o)

-- | A conduit has two operations: it can receive new input (a push), which
-- will generate some possible leftover values and a new 'Stream' of output,
-- and can be closed.
--
-- Invariants:
--
-- * Neither a push nor close may be performed after a conduit returns an EOF
-- from a push, or after a close is performed.
data Conduit input m output = Conduit
    { conduitPush :: [input] -> ResourceT m (ConduitResult input output)
    , conduitClose :: [input] -> ResourceT m (ConduitCloseResult input output)
    }

instance Monad m => Functor (Conduit input m) where
    fmap f c = c
        { conduitPush = liftM (fmap f) . conduitPush c
        , conduitClose = liftM (fmap f) . conduitClose c
        }

-- | A monadic action generating a 'Conduit'. See @SourceM@ and @SinkM@ for
-- more motivation.
newtype ConduitM input m output =
    ConduitM { genConduit :: ResourceT m (Conduit input m output) }

instance Monad m => Functor (ConduitM input m) where
    fmap f (ConduitM mc) = ConduitM (liftM (fmap f) mc)
