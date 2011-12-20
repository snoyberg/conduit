-- | Defines the types for a conduit, which is a transformer of data. A conduit
-- is almost always connected either left (to a source) or right (to a sink).
module Data.Conduit.Types.Conduit
    ( ConduitResult (..)
    , Conduit (..)
    , ConduitM (..)
    ) where

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad (liftM)

data ConduitResult leftover output = ConduitResult leftover [output]

instance Functor (ConduitResult leftover) where
    fmap f (ConduitResult l o) = ConduitResult l (fmap f o)

-- | A conduit has two operations: it can receive new input (a push), which
-- will generate some possible leftover values and a new 'Stream' of output,
-- and can be closed.
--
-- Invariants:
--
-- * Neither a push nor close may be performed after a conduit returns a
-- 'ConduitCloseResult' from a push, or after a close is performed.
data Conduit input m output = Conduit
    { conduitPush :: [input] -> ResourceT m (ConduitResult (Maybe [input]) output)
    , conduitClose :: [input] -> ResourceT m (ConduitResult [input] output)
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
