{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Data.Conduit.Internal.Fusion
    ( -- ** Types
      Step (..)
    , Stream (..)
      -- ** Producer
    , streamProducerM
    , streamProducerId
      -- ** Consumer
    , streamConsumerM
    , foldStream
    --, foldMStream
    ) where

import Data.Conduit.Internal.Conduit
import Data.Conduit.Internal.Pipe (Pipe (..))
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad (liftM)

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
    ConduitM $ \rest ->
        let loop s = do
                res <- step s
                case res of
                    Emit s' o -> return $ HaveOutput (PipeM $ loop s') (return ()) o
                    Skip s' -> loop s'
                    Stop -> return $ rest ()
        in PipeM $ ms0 >>= loop
{-# INLINE [0] streamProducerM #-}

streamProducerId :: Monad m => Stream Identity o -> Producer m o
streamProducerId (Stream step ms0) = ConduitM $ \rest -> let
    loop s =
        case runIdentity $ step s of
            Emit s' o -> HaveOutput (loop s') (return ()) o
            Skip s' -> loop s'
            Stop -> rest ()
    in loop $ runIdentity ms0
{-# INLINE [0] streamProducerId #-}

data Unstream i m r = forall s. Unstream
    (s -> i -> m (Either s r))
    (s -> m r)
    (m s)

streamConsumerM :: Monad m
                => Unstream i m r
                -> Consumer i m r
streamConsumerM (Unstream step final ms0) = ConduitM $ \rest -> let
    loop s =
        NeedInput more done
      where
        more i = PipeM $ do
            res <- step s i
            case res of
                Left s' -> return $ loop s'
                Right r -> return $ rest r
        done () = PipeM $ liftM rest $ final s
    in PipeM $ ms0 >>= return . loop
{-# INLINE [0] streamConsumerM #-}

streamConsumerId :: Monad m
                 => Unstream i Identity r
                 -> Consumer i m r
streamConsumerId (Unstream step final ms0) =
    ConduitM $ \rest ->
        let loop s =
                NeedInput more done
              where
                more i =
                    case runIdentity $ step s i of
                        Left s' -> loop s'
                        Right r -> rest r
                done () = rest $ runIdentity $ final s
         in loop (runIdentity ms0)
{-# INLINE [0] streamConsumerId #-}

{-
streamConsumer :: Monad m
                => (forall t. (MonadTrans t, Monad (t m)) => t m (Maybe i) -> t m r)
                -> Stream m i -> m r
streamConsumer = error "streamConsumer"
{-# INLINE streamConsumer #-}
{-# RULES "streamConsumer" forall s f. streamProducerM s $$ streamConsumerM f = streamConsumer f s #-}
-}

foldStream f b0 =
    streamConsumerId $ Unstream step return (return b0)
  where
    step !b a = return $ Left $ f b a
{-# INLINE foldStream #-}

{-
foldMStream f b0 = streamConsumerM $ \step s0 ->
    let loop !b s = do
            res <- step s
            case res of
                Emit s' a -> lift (f b a) >>= flip loop s'
                Skip s' -> loop b s'
                Stop -> return b
     in loop b0 s0
     -}
