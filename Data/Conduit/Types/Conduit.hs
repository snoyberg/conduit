-- | Defines the types for a conduit, which is a transformer of data. A conduit
-- is almost always connected either left (to a source) or right (to a sink).
module Data.Conduit.Types.Conduit
    ( ConduitResult (..)
    , Conduit (..)
    , ConduitM (..)
    , BConduit (..)
    ) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit.Types.Source (Stream (..))
import Control.Monad (liftM)

-- | A conduit can return both leftover input data and a new stream of output
-- data.
data ConduitResult input output = ConduitResult [input] (Stream output)

instance Functor (ConduitResult input) where
    fmap f (ConduitResult i o) = ConduitResult i (fmap f o)

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
    , conduitClose :: ResourceT m [output]
    }

instance Monad m => Functor (Conduit input m) where
    fmap f c = c
        { conduitPush = liftM (fmap f) . conduitPush c
        , conduitClose = liftM (map f) (conduitClose c)
        }

-- | A monadic action generating a 'Conduit'. See @SourceM@ and @SinkM@ for
-- more motivation.
newtype ConduitM input m output =
    ConduitM { genConduit :: ResourceT m (Conduit input m output) }

instance Monad m => Functor (ConduitM input m) where
    fmap f (ConduitM mc) = ConduitM (liftM (fmap f) mc)

-- | A corrollary to @BSource@ for a conduit.
--
-- Note that if bconduitPush is called when there is buffered content, all
-- input will be returned as leftover. Therefore, bconduitPull should always be
-- called until it produces no more output.
--
-- Also, like @BSource@, it is the @BConduit@'s responsbility to check for null
-- input for 'bconduitUnpull'.
data BConduit input m output = BConduit
    { bconduitPush :: [input] -> ResourceT m (ConduitResult input output)
    , bconduitUnpull :: [output] -> ResourceT m ()
    , bconduitClose :: ResourceT m [output]
    , bconduitPull :: ResourceT m [output] -- ^ pull data from buffer, if available
    }
