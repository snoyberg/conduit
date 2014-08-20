{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Data.Conduit.Internal.Fusion
    ( -- ** Types
      Step (..)
    , Stream (..)
    , StreamConduit (..)
      -- ** Functions
    , unstream
    , fuseStream
    , connectStream
    , streamToStreamConduit
    ) where

import Data.Conduit.Internal.Conduit
import Data.Conduit.Internal.Pipe (Pipe (..))
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad (liftM)
import Data.Void (Void)

-- | This is the same as stream fusion\'s Step. Constructors are renamed to
-- avoid confusion with conduit names.
data Step s o r
    = Emit s o
    | Skip s
    | Stop r

data Stream m o r = forall s. Stream
    (s -> m (Step s o r))
    (m s)

-- FIXME investigate if slimming down the number of constructors helps or hurts performance
data StreamConduit i o m r
    = SCSource  (ConduitM i o m r) (Stream m o r)
    | SCConduit (ConduitM i o m r) (Stream m i () -> Stream m o r)
    | SCSink    (ConduitM i o m r) (Stream m i () -> m r)

unstream :: StreamConduit i o m r -> ConduitM i o m r
unstream (SCSource  c _) = c
unstream (SCConduit c _) = c
unstream (SCSink    c _) = c
{-# INLINE [0] unstream #-}

fuseStream :: Monad m
           => StreamConduit a b m ()
           -> StreamConduit b c m r
           -> StreamConduit a c m r
fuseStream left (SCSource c s) = SCSource (unstream left =$= c) s
fuseStream (SCSource a x) (SCConduit b y) = SCSource (a =$= b) (y x)
fuseStream (SCConduit a x) (SCConduit b y) = SCConduit (a =$= b) (y . x)
fuseStream (SCConduit a x) (SCSink b y) = SCSink (a =$= b) (y . x)
{-# INLINE fuseStream #-}

{-# RULES "fuseStream" forall left right.
        unstream left =$= unstream right = unstream (fuseStream left right)
  #-}

connectStream :: Monad m
              => StreamConduit () i    m ()
              -> StreamConduit i  Void m r
              -> m r
connectStream (SCSource _ stream) (SCSink _ f) = f stream
{-# INLINE connectStream #-}

{-# RULES "connectStream" forall left right.
        unstream left $$ unstream right = connectStream left right
  #-}

streamToStreamConduit
    :: Monad m
    => Stream m o ()
    -> StreamConduit i o m ()
streamToStreamConduit str@(Stream step ms0) =
    SCSource con str
  where
    con = ConduitM $ \rest -> PipeM $ do
        s0 <- ms0
        let loop s = do
                res <- step s
                case res of
                    Stop () -> return $ rest ()
                    Emit s' o -> return $ HaveOutput (PipeM $ loop s') (return ()) o
                    Skip s' -> loop s'
        loop s0
