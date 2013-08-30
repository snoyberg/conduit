module Data.Conduit.Extra.ZipSink where

import Control.Applicative
import Control.Monad
import Data.Conduit as C
import Data.Conduit.Util
import Data.Traversable (Traversable(..), sequenceA)

-- | A wrapper for defining an 'Applicative' instance for 'Sink's which allows
-- to combine sinks together, generalizing 'zipSinks'. A combined sink
-- distributes the input to all its participants and when all finish, produces
-- the result. This allows to define functions like
--
-- @
-- broadcast :: (Monad m)
--           => [Sink i m r] -> Sink i m [r]
-- broadcast = getZipSink . sequenceA . fmap ZipSink
-- @
-- 
-- Note that the standard 'Applicative' instance for conduits works
-- differently. It feeds one sink with input until it finishes, then switches
-- to another, etc., and at the end combines their results.
newtype ZipSink i m r = ZipSink { getZipSink :: Sink i m r }

instance Monad m => Functor (ZipSink i m) where
    fmap f (ZipSink x) = ZipSink (liftM f x)
instance Monad m => Applicative (ZipSink i m) where
    pure  = ZipSink . return
    (ZipSink f) <*> (ZipSink x) =
         ZipSink $ liftM (uncurry ($)) $ zipSinks f x

broadcast :: (Traversable f, Monad m) => f (Sink i m r) -> Sink i m (f r)
broadcast = getZipSink . sequenceA . fmap ZipSink
