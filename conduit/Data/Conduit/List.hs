{-# LANGUAGE RankNTypes #-}
-- | Higher-level functions to interact with the elements of a stream. Most of
-- these are based on list functions.
--
-- Note that these functions all deal with individual elements of a stream as a
-- sort of \"black box\", where there is no introspection of the contained
-- elements. Values such as @ByteString@ and @Text@ will likely need to be
-- treated specially to deal with their contents properly (@Word8@ and @Char@,
-- respectively). See the "Data.Conduit.Binary" and "Data.Conduit.Text"
-- modules.
module Data.Conduit.List
    ( -- * Sources
      sourceList
    , sourceNull
    , unfold
    , unfoldM
    , enumFromTo
    , iterate
      -- * Sinks
      -- ** Pure
    , fold
    , foldMap
    , take
    , drop
    , head
    , peek
    , consume
    , sinkNull
      -- ** Monadic
    , foldMapM
    , foldM
    , mapM_
      -- * Conduits
      -- ** Pure
    , map
    , mapMaybe
    , mapFoldable
    , catMaybes
    , concat
    , concatMap
    , concatMapAccum
    , scanl
    , scan
    , mapAccum
    , groupBy
    , isolate
    , filter
      -- ** Monadic
    , mapM
    , iterM
    , scanlM
    , scanM
    , mapAccumM
    , mapMaybeM
    , mapFoldableM
    , concatMapM
    , concatMapAccumM
      -- * Misc
    , sequence
    ) where

import qualified Prelude
import Prelude
    ( ($), return, (==), (-), Int
    , (.), id, Maybe (..), Monad
    , Bool (..)
    , (>>)
    , (>>=)
    , seq
    , otherwise
    , Enum (succ), Eq
    , maybe
    , either
    , (<=)
    )
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Foldable as F
import Data.Conduit
import qualified Data.Conduit.Internal as CI
import Control.Monad (when, (<=<), liftM, void)
import Control.Monad.Trans.Class (lift)

-- | Generate a source from a seed value.
--
-- Since 0.4.2
unfold :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> Producer m a
unfold f =
    go
  where
    go seed =
        case f seed of
            Just (a, seed') -> yield a >> go seed'
            Nothing -> return ()

-- | A monadic unfold.
--
-- Since 1.1.2
unfoldM :: Monad m
        => (b -> m (Maybe (a, b)))
        -> b
        -> Producer m a
unfoldM f =
    go
  where
    go seed = do
        mres <- lift $ f seed
        case mres of
            Just (a, seed') -> yield a >> go seed'
            Nothing -> return ()

sourceList :: Monad m => [a] -> Producer m a
sourceList = Prelude.mapM_ yield

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Since 0.4.2
enumFromTo :: (Enum a, Eq a, Monad m)
           => a
           -> a
           -> Producer m a
enumFromTo x = CI.ConduitM . CI.enumFromTo x
{-# INLINE enumFromTo #-}

-- | Produces an infinite stream of repeated applications of f to x.
iterate :: Monad m => (a -> a) -> a -> Producer m a
iterate f =
    go
  where
    go a = yield a >> go (f a)

-- | A strict left fold.
--
-- Since 0.3.0
fold :: Monad m
     => (b -> a -> b)
     -> b
     -> Consumer a m b
fold f =
    loop
  where
    loop accum =
        await >>= maybe (return accum) go
      where
        go a =
            let accum' = f accum a
             in accum' `seq` loop accum'

-- | A monadic strict left fold.
--
-- Since 0.3.0
foldM :: Monad m
      => (b -> a -> m b)
      -> b
      -> Consumer a m b
foldM f =
    loop
  where
    loop accum = do
        await >>= maybe (return accum) go
      where
        go a = do
            accum' <- lift $ f accum a
            accum' `seq` loop accum'

-- | A monoidal strict left fold.
--
-- Since 0.5.3
foldMap :: (Monad m, Monoid b)
        => (a -> b)
        -> Consumer a m b
foldMap f =
    fold combiner mempty
  where
    combiner accum = mappend accum . f

-- | A monoidal strict left fold in a Monad.
--
-- Since 1.0.8
foldMapM :: (Monad m, Monoid b)
        => (a -> m b)
        -> Consumer a m b
foldMapM f =
    foldM combiner mempty
  where
    combiner accum = liftM (mappend accum) . f

-- | Apply the action to all values in the stream.
--
-- Since 0.3.0
mapM_ :: Monad m
      => (a -> m ())
      -> Consumer a m ()
mapM_ f = awaitForever $ lift . f
{-# INLINE [1] mapM_ #-}

srcMapM_ :: Monad m => Source m a -> (a -> m ()) -> m ()
srcMapM_ (CI.ConduitM src) f =
    go src
  where
    go (CI.Done ()) = return ()
    go (CI.PipeM mp) = mp >>= go
    go (CI.Leftover p ()) = go p
    go (CI.HaveOutput p _ o) = f o >> go p
    go (CI.NeedInput _ c) = go (c ())
{-# INLINE srcMapM_ #-}
{-# RULES "connect to mapM_" forall f src. src $$ mapM_ f = srcMapM_ src f #-}

-- | Ignore a certain number of values in the stream. This function is
-- semantically equivalent to:
--
-- > drop i = take i >> return ()
--
-- However, @drop@ is more efficient as it does not need to hold values in
-- memory.
--
-- Since 0.3.0
drop :: Monad m
     => Int
     -> Consumer a m ()
drop =
    loop
  where
    loop i | i <= 0 = return ()
    loop count = await >>= maybe (return ()) (\_ -> loop (count - 1))

-- | Take some values from the stream and return as a list. If you want to
-- instead create a conduit that pipes data to another sink, see 'isolate'.
-- This function is semantically equivalent to:
--
-- > take i = isolate i =$ consume
--
-- Since 0.3.0
take :: Monad m
     => Int
     -> Consumer a m [a]
take =
    loop id
  where
    loop front 0 = return $ front []
    loop front count = await >>= maybe
        (return $ front [])
        (\x -> loop (front .(x:)) (count - 1))

-- | Take a single value from the stream, if available.
--
-- Since 0.3.0
head :: Monad m => Consumer a m (Maybe a)
head = await

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.3.0
peek :: Monad m => Consumer a m (Maybe a)
peek = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))

-- | Apply a transformation to all values in a stream.
--
-- Since 0.3.0
map :: Monad m => (a -> b) -> Conduit a m b
map f = awaitForever $ yield . f
{-# INLINE [1] map #-}

-- Since a Source never has any leftovers, fusion rules on it are safe.
{-# RULES "source/map fusion $=" forall f src. src $= map f = mapFuseRight src f #-}
{-# RULES "source/map fusion =$=" forall f src. src =$= map f = mapFuseRight src f #-}

mapFuseRight :: Monad m => Source m a -> (a -> b) -> Source m b
mapFuseRight (CI.ConduitM src) f = CI.ConduitM (CI.mapOutput f src)
{-# INLINE mapFuseRight #-}

{-

It might be nice to include these rewrite rules, but they may have subtle
differences based on leftovers.

{-# RULES "map-to-mapOutput pipeL" forall f src. pipeL src (map f) = mapOutput f src #-}
{-# RULES "map-to-mapOutput $=" forall f src. src $= (map f) = mapOutput f src #-}
{-# RULES "map-to-mapOutput pipe" forall f src. pipe src (map f) = mapOutput f src #-}
{-# RULES "map-to-mapOutput >+>" forall f src. src >+> (map f) = mapOutput f src #-}

{-# RULES "map-to-mapInput pipeL" forall f sink. pipeL (map f) sink = mapInput f (Prelude.const Prelude.Nothing) sink #-}
{-# RULES "map-to-mapInput =$" forall f sink. map f =$ sink = mapInput f (Prelude.const Prelude.Nothing) sink #-}
{-# RULES "map-to-mapInput pipe" forall f sink. pipe (map f) sink = mapInput f (Prelude.const Prelude.Nothing) sink #-}
{-# RULES "map-to-mapInput >+>" forall f sink. map f >+> sink = mapInput f (Prelude.const Prelude.Nothing) sink #-}

{-# RULES "map-to-mapOutput =$=" forall f con. con =$= map f = mapOutput f con #-}
{-# RULES "map-to-mapInput =$=" forall f con. map f =$= con = mapInput f (Prelude.const Prelude.Nothing) con #-}

{-# INLINE [1] map #-}

-}

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Since 0.3.0
mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM f = awaitForever $ yield <=< lift . f

-- | Apply a monadic action on all values in a stream.
--
-- This @Conduit@ can be used to perform a monadic side-effect for every
-- value, whilst passing the value through the @Conduit@ as-is.
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
--
-- Since 0.5.6
iterM :: Monad m => (a -> m ()) -> Conduit a m a
iterM f = awaitForever $ \a -> lift (f a) >> yield a

-- | Apply a transformation that may fail to all values in a stream, discarding
-- the failures.
--
-- Since 0.5.1
mapMaybe :: Monad m => (a -> Maybe b) -> Conduit a m b
mapMaybe f = awaitForever $ maybe (return ()) yield . f

-- | Apply a monadic transformation that may fail to all values in a stream,
-- discarding the failures.
--
-- Since 0.5.1
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Conduit a m b
mapMaybeM f = awaitForever $ maybe (return ()) yield <=< lift . f

-- | Filter the @Just@ values from a stream, discarding the @Nothing@  values.
--
-- Since 0.5.1
catMaybes :: Monad m => Conduit (Maybe a) m a
catMaybes = awaitForever $ maybe (return ()) yield

-- | Generalization of 'catMaybes'. It puts all values from
--   'F.Foldable' into stream.
--
-- Since 1.0.6
concat :: (Monad m, F.Foldable f) => Conduit (f a) m a
concat = awaitForever $ F.mapM_ yield

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Since 0.3.0
concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap f = awaitForever $ sourceList . f

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Since 0.3.0
concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f = awaitForever $ sourceList <=< lift . f

-- | 'concatMap' with an accumulator.
--
-- Since 0.3.0
concatMapAccum :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccum f x0 = void (mapAccum f x0) =$= concat

-- | Deprecated synonym for @mapAccum@
--
-- Since 1.0.6
scanl :: Monad m => (a -> s -> (s, b)) -> s -> Conduit a m b
scanl f s = void $ mapAccum f s
{-# DEPRECATED scanl "Use mapAccum instead" #-}

-- | Deprecated synonym for @mapAccumM@
--
-- Since 1.0.6
scanlM :: Monad m => (a -> s -> m (s, b)) -> s -> Conduit a m b
scanlM f s = void $ mapAccumM f s
{-# DEPRECATED scanlM "Use mapAccumM instead" #-}

-- | Analog of @mapAccumL@ for lists.
--
-- Since 1.1.1
mapAccum :: Monad m => (a -> s -> (s, b)) -> s -> ConduitM a b m s
mapAccum f =
    loop
  where
    loop s = await >>= maybe (return s) go
      where
        go a = case f a s of
                 (s', b) -> yield b >> loop s'

-- | Monadic `mapAccum`.
--
-- Since 1.1.1
mapAccumM :: Monad m => (a -> s -> m (s, b)) -> s -> ConduitM a b m s
mapAccumM f =
    loop
  where
    loop s = await >>= maybe (return s) go
      where
        go a = do (s', b) <- lift $ f a s
                  yield b
                  loop s'

-- | Analog of 'Prelude.scanl' for lists.
--
-- Since 1.1.1
scan :: Monad m => (a -> b -> b) -> b -> ConduitM a b m b
scan f =
    mapAccum $ \a b -> let b' = f a b in (b', b')

-- | Monadic @scanl@.
--
-- Since 1.1.1
scanM :: Monad m => (a -> b -> m b) -> b -> ConduitM a b m b
scanM f =
    mapAccumM $ \a b -> do b' <- f a b
                           return (b', b')

-- | 'concatMapM' with an accumulator.
--
-- Since 0.3.0
concatMapAccumM :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
concatMapAccumM f x0 = void (mapAccumM f x0) =$= concat


-- | Generalization of 'mapMaybe' and 'concatMap'. It applies function
-- to all values in a stream and send values inside resulting
-- 'Foldable' downstream.
--
-- Since 1.0.6
mapFoldable :: (Monad m, F.Foldable f) => (a -> f b) -> Conduit a m b
mapFoldable f = awaitForever $ F.mapM_ yield . f

-- | Monadic variant of 'mapFoldable'.
--
-- Since 1.0.6
mapFoldableM :: (Monad m, F.Foldable f) => (a -> m (f b)) -> Conduit a m b
mapFoldableM f = awaitForever $ F.mapM_ yield <=< lift . f


-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
--
-- Since 0.3.0
consume :: Monad m => Consumer a m [a]
consume =
    loop id
  where
    loop front = await >>= maybe (return $ front []) (\x -> loop $ front . (x:))

-- | Grouping input according to an equality function.
--
-- Since 0.3.0
groupBy :: Monad m => (a -> a -> Bool) -> Conduit a m [a]
groupBy f =
    start
  where
    start = await >>= maybe (return ()) (loop id)

    loop rest x =
        await >>= maybe (yield (x : rest [])) go
      where
        go y
            | f x y     = loop (rest . (y:)) x
            | otherwise = yield (x : rest []) >> loop id y

-- | Ensure that the inner sink consumes no more than the given number of
-- values. Note this this does /not/ ensure that the sink consumes all of those
-- values. To get the latter behavior, combine with 'sinkNull', e.g.:
--
-- > src $$ do
-- >     x <- isolate count =$ do
-- >         x <- someSink
-- >         sinkNull
-- >         return x
-- >     someOtherSink
-- >     ...
--
-- Since 0.3.0
isolate :: Monad m => Int -> Conduit a m a
isolate =
    loop
  where
    loop 0 = return ()
    loop count = await >>= maybe (return ()) (\x -> yield x >> loop (count - 1))

-- | Keep only values in the stream passing a given predicate.
--
-- Since 0.3.0
filter :: Monad m => (a -> Bool) -> Conduit a m a
filter f = awaitForever $ \i -> when (f i) (yield i)

filterFuseRight :: Monad m => Source m a -> (a -> Bool) -> Source m a
filterFuseRight (CI.ConduitM src) f =
    CI.ConduitM (go src)
  where
    go (CI.Done ()) = CI.Done ()
    go (CI.PipeM mp) = CI.PipeM (liftM go mp)
    go (CI.Leftover p i) = CI.Leftover (go p) i
    go (CI.HaveOutput p c o)
        | f o = CI.HaveOutput (go p) c o
        | otherwise = go p
    go (CI.NeedInput p c) = CI.NeedInput (go . p) (go . c)
-- Intermediate finalizers are dropped, but this is acceptable: the next
-- yielded value would be demanded by downstream in any event, and that new
-- finalizer will always override the existing finalizer.
{-# RULES "source/filter fusion $=" forall f src. src $= filter f = filterFuseRight src f #-}
{-# RULES "source/filter fusion =$=" forall f src. src =$= filter f = filterFuseRight src f #-}
{-# INLINE filterFuseRight #-}

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
--
-- Since 0.3.0
sinkNull :: Monad m => Consumer a m ()
sinkNull = awaitForever $ \_ -> return ()
{-# RULES "connect to sinkNull" forall src. src $$ sinkNull = srcSinkNull src #-}

srcSinkNull :: Monad m => Source m a -> m ()
srcSinkNull (CI.ConduitM src) =
    go src
  where
    go (CI.Done ()) = return ()
    go (CI.PipeM mp) = mp >>= go
    go (CI.Leftover p ()) = go p
    go (CI.HaveOutput p _ _) = go p
    go (CI.NeedInput _ c) = go (c ())
{-# INLINE srcSinkNull #-}

-- | A source that outputs no values. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Since 0.3.0
sourceNull :: Monad m => Producer m a
sourceNull = return ()

-- | Run a @Pipe@ repeatedly, and output its result value downstream. Stops
-- when no more input is available from upstream.
--
-- Since 0.5.0
sequence :: Monad m
         => Consumer i m o -- ^ @Pipe@ to run repeatedly
         -> Conduit i m o
sequence sink =
    self
  where
    self = awaitForever $ \i -> leftover i >> sink >>= yield
