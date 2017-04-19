{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module Data.Conduit.Internal.Deprecated
  ( Source
  , Sink
  , Conduit
  , Producer
  , Consumer
  , ($$)
  , ($=)
  , (=$)
  , (=$=)
  , connect
  ) where

import Data.Conduit.Internal.Conduit
import Data.Void (Void)

-- | Provides a stream of output values, without consuming any input or
-- producing a final result.
--
-- Since 0.5.0
type Source m o = ConduitM () o m ()
{-# DEPRECATED Source "Use ConduitM directly instead" #-}

-- | A component which produces a stream of output values, regardless of the
-- input stream. A @Producer@ is a generalization of a @Source@, and can be
-- used as either a @Source@ or a @Conduit@.
--
-- Since 1.0.0
type Producer m o = forall i. ConduitM i o m ()
{-# DEPRECATED Producer "Use ConduitM directly instead" #-}

-- | Consumes a stream of input values and produces a final result, without
-- producing any output.
--
-- > type Sink i m r = ConduitM i Void m r
--
-- Since 0.5.0
type Sink i = ConduitM i Void
{-# DEPRECATED Sink "Use ConduitM directly instead" #-}

-- | A component which consumes a stream of input values and produces a final
-- result, regardless of the output stream. A @Consumer@ is a generalization of
-- a @Sink@, and can be used as either a @Sink@ or a @Conduit@.
--
-- Since 1.0.0
type Consumer i m r = forall o. ConduitM i o m r
{-# DEPRECATED Consumer "Use ConduitM directly instead" #-}

-- | Consumes a stream of input values and produces a stream of output values,
-- without producing a final result.
--
-- Since 0.5.0
type Conduit i m o = ConduitM i o m ()
{-# DEPRECATED Conduit "Use ConduitM directly instead" #-}

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- If you would like to keep the @Source@ open to be used for other
-- operations, use the connect-and-resume operator '$$+'.
--
-- Since 0.4.0
($$) :: Monad m => Source m a -> Sink a m b -> m b
src $$ sink = do
    (rsrc, res) <- src $$+ sink
    rsrc $$+- return ()
    return res
{-# INLINE [1] ($$) #-}
{-# DEPRECATED ($$) "Use .| and runConduit instead" #-}

-- | Named function synonym for '$$'.
--
-- Since 1.2.3
connect :: Monad m
        => ConduitM () a m ()
        -> ConduitM a Void m b
        -> m b
connect = ($$)
{-# DEPRECATED connect "Use runConduit and .| instead" #-}

-- | A synonym for '=$=' for backwards compatibility.
--
-- Since 0.4.0
($=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
($=) = (=$=)
{-# INLINE [0] ($=) #-}
{-# RULES "conduit: $= is =$=" ($=) = (=$=) #-}
{-# DEPRECATED ($=) "Use .| instead" #-}

-- | A synonym for '=$=' for backwards compatibility.
--
-- Since 0.4.0
(=$) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
(=$) = (=$=)
{-# INLINE [0] (=$) #-}
{-# RULES "conduit: =$ is =$=" (=$) = (=$=) #-}
{-# DEPRECATED (=$) "Use .| instead" #-}

-- | Fusion operator, combining two @Conduit@s together into a new @Conduit@.
--
-- Both @Conduit@s will be closed when the newly-created @Conduit@ is closed.
--
-- Leftover data returned from the right @Conduit@ will be discarded.
--
-- @since 0.4.0
(=$=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
(=$=) = fuse
{-# INLINE [0] (=$=) #-}
{-# DEPRECATED (=$=) "Use .| instead" #-}

infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
