{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Trustworthy #-}
module Data.Conduit.Internal.Fusion
    ( -- ** Types
      Step (..)
    , Stream (..)
    , ConduitWithStream
    , StreamConduitM
    , StreamConduit
    , StreamSource
    , StreamProducer
    , StreamSink
    , StreamConsumer
      -- ** Functions
    , streamConduit
    , streamSource
    , streamSourcePure
    , unstream
    ) where

import Data.Conduit.Internal.Conduit
import Data.Conduit.Internal.Pipe (Pipe (..))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Void (Void, absurd)

-- | This is the same as stream fusion\'s Step. Constructors are renamed to
-- avoid confusion with conduit names.
data Step s o r
    = Emit s o
    | Skip s
    | Stop r
    deriving Functor

data Stream m o r = forall s. Stream
    (s -> m (Step s o r))
    (m s)

data ConduitWithStream i o m r = ConduitWithStream
    (ConduitM i o m r)
    (StreamConduitM i o m r)

type StreamConduitM i o m r = Stream m i () -> Stream m o r

type StreamConduit i m o = StreamConduitM i o m ()

type StreamSource m o = StreamConduitM () o m ()

type StreamProducer m o = forall i. StreamConduitM i o m ()

type StreamSink i m r = StreamConduitM i Void m r

type StreamConsumer i m r = forall o. StreamConduitM i o m r

unstream :: ConduitWithStream i o m r -> ConduitM i o m r
unstream (ConduitWithStream c _) = c
{-# INLINE [0] unstream #-}

fuseStream :: Monad m
           => ConduitWithStream a b m ()
           -> ConduitWithStream b c m r
           -> ConduitWithStream a c m r
fuseStream (ConduitWithStream a x) (ConduitWithStream b y) = ConduitWithStream (a =$= b) (y . x)
{-# INLINE fuseStream #-}

{-# RULES "conduit: fuseStream" forall left right.
        unstream left =$= unstream right = unstream (fuseStream left right)
  #-}

runStream :: Monad m
          => ConduitWithStream () Void m r
          -> m r
runStream (ConduitWithStream _ f) =
    run $ f $ Stream emptyStep (return ())
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
{-# INLINE runStream #-}

{-# RULES "conduit: runStream" forall stream.
        runConduit (unstream stream) = runStream stream
  #-}

connectStream :: Monad m
              => ConduitWithStream () i    m ()
              -> ConduitWithStream i  Void m r
              -> m r
connectStream (ConduitWithStream _ stream) (ConduitWithStream _ f) =
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

{-# RULES "conduit: connectStream" forall left right.
        unstream left $$ unstream right = connectStream left right
  #-}

connectStream1 :: Monad m
               => ConduitWithStream () i    m ()
               -> ConduitM          i  Void m r
               -> m r
connectStream1 (ConduitWithStream _ fstream) (ConduitM sink0) =
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

{-# RULES "conduit: connectStream1" forall left right.
        unstream left $$ right = connectStream1 left right
  #-}

{-

Not only will this rule not fire reliably, but due to finalizers, it can change
behavior unless implemented very carefully. Odds are that the careful
implementation won't be any faster, so leaving this commented out for now.

connectStream2 :: Monad m
               => ConduitM      () i    m ()
               -> ConduitWithStream i  Void m r
               -> m r
connectStream2 (ConduitM src0) (ConduitWithStream _ fstream) =
    run $ fstream $ Stream step' $ return (return (), src0 Done)
  where
    step' (_, Done ()) = return $ Stop ()
    {-# INLINE step' #-}

    run (Stream step ms0) =
        ms0 >>= loop
      where
        loop s = do
            res <- step s
            case res of
                Stop r -> return r
                Emit _ o -> absurd o
                Skip s' -> loop s'
{-# INLINE connectStream2 #-}

{-# RULES "conduit: connectStream2" forall left right.
        left $$ unstream right = connectStream2 left right
  #-}
-}

streamConduit :: ConduitM i o m r
              -> (Stream m i () -> Stream m o r)
              -> ConduitWithStream i o m r
streamConduit = ConduitWithStream
{-# INLINE CONLIKE streamConduit #-}

streamSource
    :: Monad m
    => Stream m o ()
    -> ConduitWithStream i o m ()
streamSource str@(Stream step ms0) =
    ConduitWithStream con (const str)
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
{-# INLINE streamSource #-}

streamSourcePure
    :: Monad m
    => Stream Identity o ()
    -> ConduitWithStream i o m ()
streamSourcePure (Stream step ms0) =
    ConduitWithStream con (const $ Stream (return . runIdentity . step) (return s0))
  where
    s0 = runIdentity ms0
    con = ConduitM $ \rest ->
        let loop s =
                case runIdentity $ step s of
                    Stop () -> rest ()
                    Emit s' o -> HaveOutput (loop s') (return ()) o
                    Skip s' -> loop s'
         in loop s0
{-# INLINE streamSourcePure #-}
