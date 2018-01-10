{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
-- | These are stream fusion versions of some of the functions in
-- "Data.Conduit.Combinators".  Many functions don't have stream
-- versions here because instead they have @RULES@ which inline a
-- definition that fuses.
module Data.Conduit.Combinators.Stream
  ( yieldManyS
  , repeatMS
  , repeatWhileMS
  , foldl1S
  , allS
  , anyS
  , sinkLazyS
  , sinkVectorS
  , sinkVectorNS
  , sinkLazyBuilderS
  , lastS
  , lastES
  , findS
  , concatMapS
  , concatMapMS
  , concatS
  , scanlS
  , scanlMS
  , mapAccumWhileS
  , mapAccumWhileMS
  , intersperseS
  , slidingWindowS
  , filterMS
  , splitOnUnboundedES
  , initReplicateS
  , initRepeatS
  )
  where

-- BEGIN IMPORTS

import           Control.Monad (liftM)
import           Control.Monad.Primitive (PrimMonad)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder (Builder, toLazyByteString)
import           Data.Conduit.Internal.Fusion
import           Data.Conduit.Internal.List.Stream (foldS)
import           Data.Maybe (isNothing, isJust)
import           Data.MonoTraversable
#if ! MIN_VERSION_base(4,8,0)
import           Data.Monoid (Monoid (..))
#endif
import qualified Data.NonNull as NonNull
import qualified Data.Sequences as Seq
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import           Prelude

#if MIN_VERSION_mono_traversable(1,0,0)
import           Data.Sequences (LazySequence (..))
#else
import           Data.Sequences.Lazy
#endif

-- END IMPORTS

yieldManyS :: (Monad m, MonoFoldable mono)
            => mono
            -> StreamProducer m (Element mono)
yieldManyS mono _ =
    Stream (return . step) (return (otoList mono))
  where
    step [] = Stop ()
    step (x:xs) = Emit xs x
{-# INLINE yieldManyS #-}

repeatMS :: Monad m
         => m a
         -> StreamProducer m a
repeatMS m _ =
    Stream step (return ())
  where
    step _ = liftM (Emit ()) m
{-# INLINE repeatMS #-}

repeatWhileMS :: Monad m
              => m a
              -> (a -> Bool)
              -> StreamProducer m a
repeatWhileMS m f _ =
    Stream step (return ())
  where
    step _ = do
        x <- m
        return $ if f x
            then Emit () x
            else Stop ()
{-# INLINE repeatWhileMS #-}

foldl1S :: Monad m
        => (a -> a -> a)
        -> StreamConsumer a m (Maybe a)
foldl1S f (Stream step ms0) =
    Stream step' (liftM (Nothing, ) ms0)
  where
    step' (mprev, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop mprev
            Skip s' -> Skip (mprev, s')
            Emit s' a -> Skip (Just $ maybe a (`f` a) mprev, s')
{-# INLINE foldl1S #-}

allS :: Monad m
     => (a -> Bool)
     -> StreamConsumer a m Bool
allS f = fmapS isNothing (findS (Prelude.not . f))
{-# INLINE allS #-}

anyS :: Monad m
     => (a -> Bool)
     -> StreamConsumer a m Bool
anyS f = fmapS isJust (findS f)
{-# INLINE anyS #-}

--TODO: use a definition like
-- fmapS (fromChunks . ($ [])) <$> CL.fold (\front next -> front . (next:)) id

sinkLazyS :: (Monad m, LazySequence lazy strict)
          => StreamConsumer strict m lazy
sinkLazyS = fmapS (fromChunks . ($ [])) $ foldS (\front next -> front . (next:)) id
{-# INLINE sinkLazyS #-}

sinkVectorS :: (V.Vector v a, PrimMonad m)
            => StreamConsumer a m (v a)
sinkVectorS (Stream step ms0) = do
    Stream step' $ do
        s0 <- ms0
        mv0 <- VM.new initSize
        return (initSize, 0, mv0, s0)
  where
    initSize = 10
    step' (maxSize, i, mv, s) = do
        res <- step s
        case res of
            Stop () -> liftM (Stop . V.slice 0 i) $ V.unsafeFreeze mv
            Skip s' -> return $ Skip (maxSize, i, mv, s')
            Emit s' x -> do
                VM.write mv i x
                let i' = i + 1
                if i' >= maxSize
                    then do
                        let newMax = maxSize * 2
                        mv' <- VM.grow mv maxSize
                        return $ Skip (newMax, i', mv', s')
                    else return $ Skip (maxSize, i', mv, s')
{-# INLINE sinkVectorS #-}

sinkVectorNS :: (V.Vector v a, PrimMonad m)
             => Int -- ^ maximum allowed size
             -> StreamConsumer a m (v a)
sinkVectorNS maxSize (Stream step ms0) = do
    Stream step' $ do
        s0 <- ms0
        mv0 <- VM.new maxSize
        return (0, mv0, s0)
  where
    step' (i, mv, _) | i >= maxSize = liftM Stop $ V.unsafeFreeze mv
    step' (i, mv, s) = do
        res <- step s
        case res of
            Stop () -> liftM (Stop . V.slice 0 i) $ V.unsafeFreeze mv
            Skip s' -> return $ Skip (i, mv, s')
            Emit s' x -> do
                VM.write mv i x
                let i' = i + 1
                return $ Skip (i', mv, s')
{-# INLINE sinkVectorNS #-}

sinkLazyBuilderS :: Monad m => StreamConsumer Builder m BL.ByteString
sinkLazyBuilderS = fmapS toLazyByteString (foldS mappend mempty)
{-# INLINE sinkLazyBuilderS #-}

lastS :: Monad m
      => StreamConsumer a m (Maybe a)
lastS (Stream step ms0) =
    Stream step' (liftM (Nothing,) ms0)
  where
    step' (mlast, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop mlast
            Skip s' -> Skip (mlast, s')
            Emit s' x -> Skip (Just x, s')
{-# INLINE lastS #-}

lastES :: (Monad m, Seq.IsSequence seq)
       => StreamConsumer seq m (Maybe (Element seq))
lastES (Stream step ms0) =
    Stream step' (liftM (Nothing, ) ms0)
  where
    step' (mlast, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop (fmap NonNull.last mlast)
            Skip s' -> Skip (mlast, s')
            Emit s' (NonNull.fromNullable -> mlast'@(Just _)) -> Skip (mlast', s')
            Emit s' _ -> Skip (mlast, s')
{-# INLINE lastES #-}

findS :: Monad m
      => (a -> Bool) -> StreamConsumer a m (Maybe a)
findS f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
      res <- step s
      return $ case res of
          Stop () -> Stop Nothing
          Skip s' -> Skip s'
          Emit s' x ->
              if f x
                  then Stop (Just x)
                  else Skip s'
{-# INLINE findS #-}

concatMapS :: (Monad m, MonoFoldable mono)
           => (a -> mono)
           -> StreamConduit a m (Element mono)
concatMapS f (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip ([], s')
            Emit s' x -> Skip (otoList (f x), s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE concatMapS #-}

concatMapMS :: (Monad m, MonoFoldable mono)
             => (a -> m mono)
             -> StreamConduit a m (Element mono)
concatMapMS f (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip ([], s')
            Emit s' x -> do
                o <- f x
                return $ Skip (otoList o, s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE concatMapMS #-}

concatS :: (Monad m, MonoFoldable mono)
         => StreamConduit mono m (Element mono)
concatS = concatMapS id
{-# INLINE concatS #-}

data ScanState a s
    = ScanEnded
    | ScanContinues a s

scanlS :: Monad m => (a -> b -> a) -> a -> StreamConduit b m a
scanlS f seed0 (Stream step ms0) =
    Stream step' (liftM (ScanContinues seed0) ms0)
  where
    step' ScanEnded = return $ Stop ()
    step' (ScanContinues seed s) = do
        res <- step s
        return $ case res of
            Stop () -> Emit ScanEnded seed
            Skip s' -> Skip (ScanContinues seed s')
            Emit s' x -> Emit (ScanContinues seed' s') seed
              where
                !seed' = f seed x
{-# INLINE scanlS #-}

scanlMS :: Monad m => (a -> b -> m a) -> a -> StreamConduit b m a
scanlMS f seed0 (Stream step ms0) =
    Stream step' (liftM (ScanContinues seed0) ms0)
  where
    step' ScanEnded = return $ Stop ()
    step' (ScanContinues seed s) = do
        res <- step s
        case res of
            Stop () -> return $ Emit ScanEnded seed
            Skip s' -> return $ Skip (ScanContinues seed s')
            Emit s' x -> do
                !seed' <- f seed x
                return $ Emit (ScanContinues seed' s') seed
{-# INLINE scanlMS #-}

mapAccumWhileS :: Monad m =>
    (a -> s -> Either s (s, b)) -> s -> StreamConduitT a b m s
mapAccumWhileS f initial (Stream step ms0) =
    Stream step' (liftM (initial, ) ms0)
  where
    step' (!accum, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop accum
            Skip s' -> Skip (accum, s')
            Emit s' x -> case f x accum of
                Right (!accum', r) -> Emit (accum', s') r
                Left   !accum'     -> Stop accum'
{-# INLINE mapAccumWhileS #-}

mapAccumWhileMS :: Monad m =>
    (a -> s -> m (Either s (s, b))) -> s -> StreamConduitT a b m s
mapAccumWhileMS f initial (Stream step ms0) =
    Stream step' (liftM (initial, ) ms0)
  where
    step' (!accum, s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop accum
            Skip s' -> return $ Skip (accum, s')
            Emit s' x -> do
                lr <- f x accum
                return $ case lr of
                    Right (!accum', r) -> Emit (accum', s') r
                    Left   !accum'     -> Stop accum'
{-# INLINE mapAccumWhileMS #-}

data IntersperseState a s
    = IFirstValue s
    | IGotValue s a
    | IEmitValue s a

intersperseS :: Monad m => a -> StreamConduit a m a
intersperseS sep (Stream step ms0) =
    Stream step' (liftM IFirstValue ms0)
  where
    step' (IFirstValue s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (IFirstValue s')
            Emit s' x -> Emit (IGotValue s' x) x
    -- Emit the separator once we know it's not the end of the list.
    step' (IGotValue s x) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (IGotValue s' x)
            Emit s' x' -> Emit (IEmitValue s' x') sep
    -- We emitted a separator, now emit the value that comes after.
    step' (IEmitValue s x) = return $ Emit (IGotValue s x) x
{-# INLINE intersperseS #-}

data SlidingWindowState seq s
    = SWInitial Int seq s
    | SWSliding seq s
    | SWEarlyExit

slidingWindowS :: (Monad m, Seq.IsSequence seq, Element seq ~ a) => Int -> StreamConduit a m seq
slidingWindowS sz (Stream step ms0) =
    Stream step' (liftM (SWInitial (max 1 sz) mempty) ms0)
  where
    step' (SWInitial n st s) = do
        res <- step s
        return $ case res of
            Stop () -> Emit SWEarlyExit st
            Skip s' -> Skip (SWInitial n st s')
            Emit s' x ->
                if n == 1
                    then Emit (SWSliding (Seq.unsafeTail st') s') st'
                    else Skip (SWInitial (n - 1) st' s')
              where
                st' = Seq.snoc st x
    -- After collecting the initial window, each upstream element
    -- causes an additional window to be yielded.
    step' (SWSliding st s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (SWSliding st s')
            Emit s' x -> Emit (SWSliding (Seq.unsafeTail st') s') st'
              where
                st' = Seq.snoc st x
    step' SWEarlyExit = return $ Stop ()

{-# INLINE slidingWindowS #-}

filterMS :: Monad m
         => (a -> m Bool)
         -> StreamConduit a m a
filterMS f (Stream step ms0) = do
    Stream step' ms0
  where
    step' s = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip s'
            Emit s' x -> do
                r <- f x
                return $
                    if r
                        then Emit s' x
                        else Skip s'
{-# INLINE filterMS #-}

data SplitState seq s
    = SplitDone
    -- When no element of seq passes the predicate.  This allows
    -- 'splitOnUnboundedES' to not run 'Seq.break' multiple times due
    -- to 'Skip's being sent by the upstream.
    | SplitNoSep seq s
    | SplitState seq s

splitOnUnboundedES :: (Monad m, Seq.IsSequence seq)
                   => (Element seq -> Bool) -> StreamConduit seq m seq
splitOnUnboundedES f (Stream step ms0) =
    Stream step' (liftM (SplitState mempty) ms0)
  where
    step' SplitDone = return $ Stop ()
    step' (SplitNoSep t s) = do
        res <- step s
        return $ case res of
            Stop () | not (onull t) -> Emit SplitDone t
                    | otherwise -> Stop ()
            Skip s' -> Skip (SplitNoSep t s')
            Emit s' t' -> Skip (SplitState (t `mappend` t') s')
    step' (SplitState t s) = do
        if onull y
            then do
                res <- step s
                return $ case res of
                    Stop () | not (onull t) -> Emit SplitDone t
                            | otherwise -> Stop ()
                    Skip s' -> Skip (SplitNoSep t s')
                    Emit s' t' -> Skip (SplitState (t `mappend` t') s')
            else return $ Emit (SplitState (Seq.drop 1 y) s) x
      where
        (x, y) = Seq.break f t
{-# INLINE splitOnUnboundedES #-}

-- | Streaming versions of @Data.Conduit.Combinators.Internal.initReplicate@
initReplicateS :: Monad m => m seed -> (seed -> m a) -> Int -> StreamProducer m a
initReplicateS mseed f cnt _ =
    Stream step (liftM (cnt, ) mseed)
  where
    step (ix, _) | ix <= 0 = return $ Stop ()
    step (ix, seed) = do
        x <- f seed
        return $ Emit (ix - 1, seed) x
{-# INLINE initReplicateS #-}

-- | Streaming versions of @Data.Conduit.Combinators.Internal.initRepeat@
initRepeatS :: Monad m => m seed -> (seed -> m a) -> StreamProducer m a
initRepeatS mseed f _ =
    Stream step mseed
  where
    step seed = do
        x <- f seed
        return $ Emit seed x
{-# INLINE initRepeatS #-}

-- | Utility function
fmapS :: Monad m
      => (a -> b)
      -> StreamConduitT i o m a
      -> StreamConduitT i o m b
fmapS f s inp =
    case s inp of
        Stream step ms0 -> Stream (fmap (liftM (fmap f)) step) ms0
{-# INLINE fmapS #-}
