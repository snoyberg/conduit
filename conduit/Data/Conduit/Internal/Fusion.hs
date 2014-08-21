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
import Data.Void (Void, absurd)

-- | This is the same as stream fusion\'s Step. Constructors are renamed to
-- avoid confusion with conduit names.
data Step s o r
    = Emit s o
    | Skip s
    | Stop r

data Stream m o r = forall s. Stream
    (s -> m (Step s o r))
    (m s)

data StreamConduit i o m r = StreamConduit
    (ConduitM i o m r)
    (Stream m i () -> Stream m o r)

unstream :: StreamConduit i o m r -> ConduitM i o m r
unstream (StreamConduit c _) = c
{-# INLINE [0] unstream #-}

fuseStream :: Monad m
           => StreamConduit a b m ()
           -> StreamConduit b c m r
           -> StreamConduit a c m r
fuseStream (StreamConduit a x) (StreamConduit b y) = StreamConduit (a =$= b) (y . x)
{-# INLINE fuseStream #-}

{-# RULES "fuseStream" forall left right.
        unstream left =$= unstream right = unstream (fuseStream left right)
  #-}

connectStream :: Monad m
              => StreamConduit () i    m ()
              -> StreamConduit i  Void m r
              -> m r
connectStream (StreamConduit _ stream) (StreamConduit _ f) =
    run $ f $ stream $ Stream emptyStep (return ())
  where
    emptyStep _ = return $ Stop ()
    run (Stream step ms0) =
        ms0 >>= loop
      where
        loop s = do
            res <- step s
            case res of
                Stop r -> return r
                Skip s' -> loop s'
                Emit _ o -> absurd o
{-# INLINE connectStream #-}

{-# RULES "connectStream" forall left right.
        unstream left $$ unstream right = connectStream left right
  #-}

connectStream1 :: Monad m
               => StreamConduit () i    m ()
               -> ConduitM      i  Void m r
               -> m r
connectStream1 (StreamConduit _ fstream) (ConduitM sink0) =
    case fstream $ Stream (const $ return $ Stop ()) (return ()) of
        Stream step ms0 ->
            let loop _ (Done r) _ = return r
                loop ls (PipeM mp) s = mp >>= flip (loop ls) s
                loop ls (Leftover p l) s = loop (l:ls) p s
                loop _ (HaveOutput _ _ o) _ = absurd o
                loop (l:ls) (NeedInput p _) s = loop ls (p l) s
                loop [] (NeedInput p c) s = do
                    res <- step s
                    case res of
                        Stop () -> loop [] (c ()) s
                        Skip s' -> loop [] (NeedInput p c) s'
                        Emit s' i -> loop [] (p i) s'
             in ms0 >>= loop [] (sink0 Done)
{-# INLINE connectStream1 #-}

{-# RULES "connectStream1" forall left right.
        unstream left $$ right = connectStream1 left right
  #-}

streamToStreamConduit
    :: Monad m
    => Stream m o ()
    -> StreamConduit i o m ()
streamToStreamConduit str@(Stream step ms0) =
    StreamConduit con (const str)
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
