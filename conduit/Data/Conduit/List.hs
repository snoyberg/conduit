{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , foldM
    , mapM_
      -- * Conduits
      -- ** Pure
    , map
    , mapMaybe
    , catMaybes
    , concatMap
    , concatMapAccum
    , groupBy
    , isolate
    , filter
      -- ** Monadic
    , mapM
    , iterM
    , mapMaybeM
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
    )
import Data.Monoid (Monoid, mempty, mappend)
import Data.Conduit.Class (MonadStream (..), awaitForever, sourceList)
import Control.Monad (when, (<=<))

-- | Generate a source from a seed value.
--
-- Since 0.4.2
unfold :: MonadStream m
       => (b -> Maybe (Downstream m, b))
       -> b
       -> m ()
unfold f =
    go
  where
    go seed =
        case f seed of
            Just (a, seed') -> yield a >> go seed'
            Nothing -> return ()

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Since 0.4.2
enumFromTo :: (Enum (Downstream m), Eq (Downstream m), MonadStream m)
           => Downstream m
           -> Downstream m
           -> m ()
enumFromTo start stop =
    go start
  where
    go i
        | i == stop = yield i
        | otherwise = yield i >> go (succ i)

-- | Produces an infinite stream of repeated applications of f to x.
iterate :: (MonadStream m, a ~ Downstream m) => (a -> a) -> a -> m b
iterate f =
    go
  where
    go a = yield a >> go (f a)

-- | A strict left fold.
--
-- Since 0.3.0
fold :: MonadStream m
     => (b -> Upstream m -> b)
     -> b
     -> m b
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
foldM :: MonadStream m
      => (b -> Upstream m -> StreamMonad m b)
      -> b
      -> m b
foldM f =
    loop
  where
    loop accum = do
        await >>= maybe (return accum) go
      where
        go a = do
            accum' <- liftStreamMonad $ f accum a
            accum' `seq` loop accum'

-- | A monoidal strict left fold.
--
-- Since 0.5.3
foldMap :: (MonadStream m, Monoid b)
        => (Upstream m -> b)
        -> m b
foldMap f =
    fold combiner mempty
  where
    combiner accum = mappend accum . f

-- | Apply the action to all values in the stream.
--
-- Since 0.3.0
mapM_ :: MonadStream m
      => (Upstream m -> StreamMonad m ())
      -> m (StreamTerm m)
mapM_ f = awaitForever $ liftStreamMonad . f

-- | Ignore a certain number of values in the stream. This function is
-- semantically equivalent to:
--
-- > drop i = take i >> return ()
--
-- However, @drop@ is more efficient as it does not need to hold values in
-- memory.
--
-- Since 0.3.0
drop :: MonadStream m
     => Int
     -> m ()
drop =
    loop
  where
    loop 0 = return ()
    loop count = await >>= maybe (return ()) (\_ -> loop (count - 1))

-- | Take some values from the stream and return as a list. If you want to
-- instead create a conduit that pipes data to another sink, see 'isolate'.
-- This function is semantically equivalent to:
--
-- > take i = isolate i =$ consume
--
-- Since 0.3.0
take :: MonadStream m
     => Int
     -> m [Upstream m]
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
head :: MonadStream m => m (Maybe (Upstream m))
head = await

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.3.0
peek :: (MonadStream m, Leftover m ~ Upstream m) => m (Maybe (Upstream m))
peek = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))

-- | Apply a transformation to all values in a stream.
--
-- Since 0.3.0
map :: MonadStream m => (Upstream m -> Downstream m) -> m (StreamTerm m)
map f = awaitForever $ yield . f

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
mapM :: MonadStream m => (Upstream m -> StreamMonad m (Downstream m)) -> m (StreamTerm m)
mapM f = awaitForever $ yield <=< liftStreamMonad . f

-- | Apply a monadic action on all values in a stream.
--
-- This @Conduit@ can be used to perform a monadic side-effect for every
-- value, whilst passing the value through the @Conduit@ as-is.
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
--
-- Since 0.5.6
iterM :: (MonadStream m, Upstream m ~ Downstream m) => (Upstream m -> StreamMonad m ()) -> m (StreamTerm m)
iterM f = awaitForever $ \a -> liftStreamMonad (f a) >> yield a

-- | Apply a transformation that may fail to all values in a stream, discarding
-- the failures.
--
-- Since 0.5.1
mapMaybe :: MonadStream m
         => (Upstream m -> Maybe (Downstream m))
         -> m (StreamTerm m)
mapMaybe f = awaitForever $ maybe (return ()) yield . f

-- | Apply a monadic transformation that may fail to all values in a stream,
-- discarding the failures.
--
-- Since 0.5.1
mapMaybeM :: MonadStream m
          => (Upstream m -> StreamMonad m (Maybe (Downstream m)))
          -> m (StreamTerm m)
mapMaybeM f = awaitForever $ maybe (return ()) yield <=< liftStreamMonad . f

-- | Filter the @Just@ values from a stream, discarding the @Nothing@  values.
--
-- Since 0.5.1
catMaybes :: (MonadStream m, Upstream m ~ Maybe (Downstream m))
          => m (StreamTerm m)
catMaybes = awaitForever $ maybe (return ()) yield

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Since 0.3.0
concatMap :: MonadStream m
          => (Upstream m -> [Downstream m])
          -> m (StreamTerm m)
concatMap f = awaitForever $ sourceList . f

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Since 0.3.0
concatMapM :: MonadStream m
           => (Upstream m -> StreamMonad m [Downstream m])
           -> m (StreamTerm m)
concatMapM f = awaitForever $ sourceList <=< liftStreamMonad . f

-- | 'concatMap' with an accumulator.
--
-- Since 0.3.0
concatMapAccum :: MonadStream m
               => (Upstream m -> accum -> (accum, [Downstream m]))
               -> accum
               -> m (StreamTerm m)
concatMapAccum f =
    loop
  where
    loop accum =
        awaitE >>= either return go
      where
        go a = do
            let (accum', bs) = f a accum
            Prelude.mapM_ yield bs
            loop accum'

-- | 'concatMapM' with an accumulator.
--
-- Since 0.3.0
concatMapAccumM :: MonadStream m
                => (Upstream m -> accum -> StreamMonad m (accum, [Downstream m]))
                -> accum
                -> m (StreamTerm m)
concatMapAccumM f =
    loop
  where
    loop accum = do
        awaitE >>= either return go
      where
        go a = do
            (accum', bs) <- liftStreamMonad $ f a accum
            Prelude.mapM_ yield bs
            loop accum'

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
--
-- Since 0.3.0
consume :: MonadStream m => m [Upstream m]
consume =
    loop id
  where
    loop front = await >>= maybe (return $ front []) (\x -> loop $ front . (x:))

-- | Grouping input according to an equality function.
--
-- Since 0.3.0
groupBy :: (a ~ Upstream m, [a] ~ Downstream m, MonadStream m)
        => (a -> a -> Bool)
        -> m (StreamTerm m)
groupBy f =
    start
  where
    start = awaitE >>= either return (loop id)

    loop rest x =
        awaitE >>= either (\r -> yield (x : rest []) >> return r) go
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
isolate :: (MonadStream m, Upstream m ~ Downstream m) => Int -> m ()
isolate =
    loop
  where
    loop 0 = return ()
    loop count = await >>= maybe (return ()) (\x -> yield x >> loop (count - 1))

-- | Keep only values in the stream passing a given predicate.
--
-- Since 0.3.0
filter :: (MonadStream m, Upstream m ~ Downstream m)
       => (Downstream m -> Bool)
       -> m (StreamTerm m)
filter f = awaitForever $ \i -> when (f i) (yield i)

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
--
-- Since 0.3.0
sinkNull :: MonadStream m => m (StreamTerm m)
sinkNull = awaitForever $ \_ -> return ()

-- | A source that outputs no values. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Since 0.3.0
sourceNull :: Monad m => m ()
sourceNull = return ()

-- | Run a @Pipe@ repeatedly, and output its result value downstream. Stops
-- when no more input is available from upstream.
--
-- Since 0.5.0
sequence :: (MonadStream m, Leftover m ~ Upstream m)
         => m (Downstream m) -- ^ @Pipe@ to run repeatedly
         -> m (StreamTerm m)
sequence sink =
    self
  where
    self = awaitForever $ \i -> leftover i >> sink >>= yield
