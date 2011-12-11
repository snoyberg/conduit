{-# LANGUAGE RankNTypes #-}
-- | Utilities for constructing 'SinkM's. Please see "Data.Conduit.Types.Sink"
-- for more information on the base types.
module Data.Conduit.Util.Sink
    ( sinkM
    , transSinkM
    ) where

import Control.Monad.Trans.Resource (ResourceT, transResourceT)
import Data.Conduit.Types.Sink
import Control.Monad (liftM)

-- | Construct a 'SinkM'. Note that your push and close functions need not
-- explicitly perform any cleanup.
sinkM :: Monad m
      => ResourceT m state -- ^ resource and/or state allocation
      -> (state -> ResourceT m ()) -- ^ resource and/or state cleanup
      -> (state -> [input] -> ResourceT m (SinkResult input (Maybe output))) -- ^ push
      -> (state -> ResourceT m (SinkResult input output)) -- ^ close
      -> SinkM input m output
sinkM alloc cleanup push close = SinkM $ do
    state <- alloc
    return SinkData
        { sinkPush = \input -> do
            res@(SinkResult _ mout) <- push state input
            maybe (return ()) (const $ cleanup state) mout
            return res
        , sinkClose = do
            res <- close state
            cleanup state
            return res
        }

-- | Transform the monad a 'SinkM' lives in.
transSinkM :: Monad m
           => (forall a. m a -> n a)
           -> SinkM input m output
           -> SinkM input n output
transSinkM f (SinkM mc) =
    SinkM (transResourceT f (liftM go mc))
  where
    go c = c
        { sinkPush = transResourceT f . sinkPush c
        , sinkClose = transResourceT f (sinkClose c)
        }
