{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Data.Conduit.Internal.Fusion
    ( -- ** Types
      Step (..)
    , Stream (..)
      -- ** Producer
    , streamProducerM
    , streamProducerId
    ) where

import Data.Conduit.Internal.Conduit
import Data.Conduit.Internal.Pipe (Pipe (..))
import Data.Functor.Identity (Identity (runIdentity))

-- | This is the same as stream fusion\'s Step. Constructors are renamed to
-- avoid confusion with conduit names.
data Step s o
    = Emit s o
    | Skip s
    | Stop

data Stream m o = forall s. Stream
    (s -> m (Step s o))
    (m s)

streamProducerM :: Monad m => Stream m o -> Producer m o
streamProducerM (Stream step ms0) =
    ConduitM $ PipeM $ ms0 >>= loop
  where
    loop s = do
        res <- step s
        case res of
            Emit s' o -> return $ HaveOutput (PipeM $ loop s') (return ()) o
            Skip s' -> loop s'
            Stop -> return $ Done ()
{-# INLINE [0] streamProducerM #-}

streamProducerId :: Monad m => Stream Identity o -> Producer m o
streamProducerId (Stream step ms0) =
    ConduitM $ loop $ runIdentity ms0
  where
    loop s =
        case runIdentity $ step s of
            Emit s' o -> HaveOutput (loop s') (return ()) o
            Skip s' -> loop s'
            Stop -> Done ()
{-# INLINE [0] streamProducerId #-}
