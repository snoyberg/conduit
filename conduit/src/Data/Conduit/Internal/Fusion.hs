{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Conduit.Internal.Fusion
    ( -- ** Types
      Step (..)
    , Stream (..)
    , ConduitWithStream
    , StreamConduitT
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
import Control.Monad.Trans.Resource (runResourceT)

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
    (ConduitT i o m r)
    (StreamConduitT i o m r)

type StreamConduitT i o m r = Stream m i () -> Stream m o r

type StreamConduit i m o = StreamConduitT i o m ()

type StreamSource m o = StreamConduitT () o m ()

type StreamProducer m o = forall i. StreamConduitT i o m ()

type StreamSink i m r = StreamConduitT i Void m r

type StreamConsumer i m r = forall o. StreamConduitT i o m r

unstream :: ConduitWithStream i o m r -> ConduitT i o m r
unstream (ConduitWithStream c _) = c
{-# INLINE [0] unstream #-}

fuseStream :: Monad m
           => ConduitWithStream a b m ()
           -> ConduitWithStream b c m r
           -> ConduitWithStream a c m r
fuseStream (ConduitWithStream a x) (ConduitWithStream b y) =
  ConduitWithStream (a .| b) (y . x)
{-# INLINE fuseStream #-}

{-# RULES "conduit: fuseStream (.|)" forall left right.
        unstream left .| unstream right = unstream (fuseStream left right)
  #-}
{-# RULES "conduit: fuseStream (fuse)" forall left right.
        fuse (unstream left) (unstream right) = unstream (fuseStream left right)
  #-}
{-# RULES "conduit: fuseStream (=$=)" forall left right.
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
{-# RULES "conduit: runStream (pure)" forall stream.
        runConduitPure (unstream stream) = runIdentity (runStream stream)
  #-}
{-# RULES "conduit: runStream (ResourceT)" forall stream.
        runConduitRes (unstream stream) = runResourceT (runStream stream)
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

{-# RULES "conduit: connectStream ($$)" forall left right.
        unstream left $$ unstream right = connectStream left right
  #-}

connectStream1 :: Monad m
               => ConduitWithStream () i    m ()
               -> ConduitT          i  Void m r
               -> m r
connectStream1 (ConduitWithStream _ fstream) (ConduitT sink0) =
    case fstream $ Stream (const $ return $ Stop ()) (return ()) of
        Stream step ms0 ->
            let loop _ (Done r) _ = return r
                loop ls (PipeM mp) s = mp >>= flip (loop ls) s
                loop ls (Leftover p l) s = loop (l:ls) p s
                loop _ (HaveOutput _ o) _ = absurd o
                loop (l:ls) (NeedInput p _) s = loop ls (p l) s
                loop [] (NeedInput p c) s = do
                    res <- step s
                    case res of
                        Stop () -> loop [] (c ()) s
                        Skip s' -> loop [] (NeedInput p c) s'
                        Emit s' i -> loop [] (p i) s'
             in ms0 >>= loop [] (sink0 Done)
{-# INLINE connectStream1 #-}

{-# RULES "conduit: connectStream1 ($$)" forall left right.
        unstream left $$ right = connectStream1 left right
  #-}

{-# RULES "conduit: connectStream1 (runConduit/.|)" forall left right.
        runConduit (unstream left .| right) = connectStream1 left right
  #-}
{-# RULES "conduit: connectStream1 (runConduit/=$=)" forall left right.
        runConduit (unstream left =$= right) = connectStream1 left right
  #-}
{-# RULES "conduit: connectStream1 (runConduit/fuse)" forall left right.
        runConduit (fuse (unstream left) right) = connectStream1 left right
  #-}

{-# RULES "conduit: connectStream1 (runConduitPure/.|)" forall left right.
        runConduitPure (unstream left .| right) = runIdentity (connectStream1 left right)
  #-}
{-# RULES "conduit: connectStream1 (runConduitPure/=$=)" forall left right.
        runConduitPure (unstream left =$= right) = runIdentity (connectStream1 left right)
  #-}
{-# RULES "conduit: connectStream1 (runConduitPure/fuse)" forall left right.
        runConduitPure (fuse (unstream left) right) = runIdentity (connectStream1 left right)
  #-}

{-# RULES "conduit: connectStream1 (runConduitRes/.|)" forall left right.
        runConduitRes (unstream left .| right) = runResourceT (connectStream1 left right)
  #-}
{-# RULES "conduit: connectStream1 (runConduitRes/=$=)" forall left right.
        runConduitRes (unstream left =$= right) = runResourceT (connectStream1 left right)
  #-}
{-# RULES "conduit: connectStream1 (runConduitRes/fuse)" forall left right.
        runConduitRes (fuse (unstream left) right) = runResourceT (connectStream1 left right)
  #-}

connectStream2 :: forall i m r. Monad m
               => ConduitT          () i    m ()
               -> ConduitWithStream i  Void m r
               -> m r
connectStream2 (ConduitT src0) (ConduitWithStream _ fstream) =
    run $ fstream $ Stream step' $ return (src0 Done)
  where
    step' :: Pipe () () i () m () -> m (Step (Pipe () () i () m ()) i ())
    step' (Done ()) = return $ Stop ()
    step' (HaveOutput pipe o) = return $ Emit pipe o
    step' (NeedInput _ c) = return $ Skip $ c ()
    step' (PipeM mp) = Skip <$> mp
    step' (Leftover p ()) = return $ Skip p
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

{-# RULES "conduit: connectStream2 ($$)" forall left right.
        left $$ unstream right = connectStream2 left right
  #-}

{-# RULES "conduit: connectStream2 (runConduit/.|)" forall left right.
        runConduit (left .| unstream right) = connectStream2 left right
  #-}
{-# RULES "conduit: connectStream2 (runConduit/fuse)" forall left right.
        runConduit (fuse left (unstream right)) = connectStream2 left right
  #-}
{-# RULES "conduit: connectStream2 (runConduit/=$=)" forall left right.
        runConduit (left =$= unstream right) = connectStream2 left right
  #-}

{-# RULES "conduit: connectStream2 (runConduitPure/.|)" forall left right.
        runConduitPure (left .| unstream right) = runIdentity (connectStream2 left right)
  #-}
{-# RULES "conduit: connectStream2 (runConduitPure/fuse)" forall left right.
        runConduitPure (fuse left (unstream right)) = runIdentity (connectStream2 left right)
  #-}
{-# RULES "conduit: connectStream2 (runConduitPure/=$=)" forall left right.
        runConduitPure (left =$= unstream right) = runIdentity (connectStream2 left right)
  #-}

{-# RULES "conduit: connectStream2 (runConduitRes/.|)" forall left right.
        runConduitRes (left .| unstream right) = runResourceT (connectStream2 left right)
  #-}
{-# RULES "conduit: connectStream2 (runConduitRes/fuse)" forall left right.
        runConduitRes (fuse left (unstream right)) = runResourceT (connectStream2 left right)
  #-}
{-# RULES "conduit: connectStream2 (runConduitRes/=$=)" forall left right.
        runConduitRes (left =$= unstream right) = runResourceT (connectStream2 left right)
  #-}

streamConduit :: ConduitT i o m r
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
    con = ConduitT $ \rest -> PipeM $ do
        s0 <- ms0
        let loop s = do
                res <- step s
                case res of
                    Stop () -> return $ rest ()
                    Emit s' o -> return $ HaveOutput (PipeM $ loop s') o
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
    con = ConduitT $ \rest ->
        let loop s =
                case runIdentity $ step s of
                    Stop () -> rest ()
                    Emit s' o -> HaveOutput (loop s') o
                    Skip s' -> loop s'
         in loop s0
{-# INLINE streamSourcePure #-}
