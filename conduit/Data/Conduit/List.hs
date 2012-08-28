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
import Data.Conduit hiding (Source, Sink, Conduit, Pipe)
import Data.Conduit.Internal (sourceList)
import Control.Monad (when, (<=<))
import Control.Monad.Trans.Class (lift)

-- | Generate a source from a seed value.
--
-- Since 0.4.2
unfold :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> GSource m a
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
enumFromTo :: (Enum a, Eq a, Monad m)
           => a
           -> a
           -> GSource m a
enumFromTo start stop =
    go start
  where
    go i
        | i == stop = yield i
        | otherwise = yield i >> go (succ i)

-- | Produces an infinite stream of repeated applications of f to x.
iterate :: Monad m => (a -> a) -> a -> GSource m a
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
     -> GSink a m b
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
      -> GSink a m b
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
        -> GSink a m b
foldMap f =
    fold combiner mempty
  where
    combiner accum = mappend accum . f

-- | Apply the action to all values in the stream.
--
-- Since 0.3.0
mapM_ :: Monad m
      => (a -> m ())
      -> GInfSink a m
mapM_ f = awaitForever $ lift . f

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
     -> GSink a m ()
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
take :: Monad m
     => Int
     -> GSink a m [a]
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
head :: Monad m => GSink a m (Maybe a)
head = await

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.3.0
peek :: Monad m => GLSink a m (Maybe a)
peek = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))

-- | Apply a transformation to all values in a stream.
--
-- Since 0.3.0
map :: Monad m => (a -> b) -> GInfConduit a m b
map f = awaitForever $ yield . f

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Since 0.3.0
mapM :: Monad m => (a -> m b) -> GInfConduit a m b
mapM f = awaitForever $ yield <=< lift . f

-- | Apply a monadic action on all values in a stream.
--
-- This @Conduit@ can be used to perform a monadic side-effect for every
-- value, whilst passing the value through the @Conduit@ as-is.
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
iterM :: Monad m => (a -> m ()) -> GInfConduit a m a
iterM f = awaitForever $ \a -> lift (f a) >> yield a

-- | Apply a transformation that may fail to all values in a stream, discarding
-- the failures.
--
-- Since 0.5.1
mapMaybe :: Monad m => (a -> Maybe b) -> GInfConduit a m b
mapMaybe f = awaitForever $ maybe (return ()) yield . f

-- | Apply a monadic transformation that may fail to all values in a stream,
-- discarding the failures.
--
-- Since 0.5.1
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> GInfConduit a m b
mapMaybeM f = awaitForever $ maybe (return ()) yield <=< lift . f

-- | Filter the @Just@ values from a stream, discarding the @Nothing@  values.
--
-- Since 0.5.1
catMaybes :: Monad m => GInfConduit (Maybe a) m a
catMaybes = awaitForever $ maybe (return ()) yield

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Since 0.3.0
concatMap :: Monad m => (a -> [b]) -> GInfConduit a m b
concatMap f = awaitForever $ sourceList . f

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Since 0.3.0
concatMapM :: Monad m => (a -> m [b]) -> GInfConduit a m b
concatMapM f = awaitForever $ sourceList <=< lift . f

-- | 'concatMap' with an accumulator.
--
-- Since 0.3.0
concatMapAccum :: Monad m => (a -> accum -> (accum, [b])) -> accum -> GInfConduit a m b
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
concatMapAccumM :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> GInfConduit a m b
concatMapAccumM f =
    loop
  where
    loop accum = do
        awaitE >>= either return go
      where
        go a = do
            (accum', bs) <- lift $ f a accum
            Prelude.mapM_ yield bs
            loop accum'

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
--
-- Since 0.3.0
consume :: Monad m => GSink a m [a]
consume =
    loop id
  where
    loop front = await >>= maybe (return $ front []) (\x -> loop $ front . (x:))

-- | Grouping input according to an equality function.
--
-- Since 0.3.0
groupBy :: Monad m => (a -> a -> Bool) -> GInfConduit a m [a]
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
isolate :: Monad m => Int -> GConduit a m a
isolate =
    loop
  where
    loop 0 = return ()
    loop count = await >>= maybe (return ()) (\x -> yield x >> loop (count - 1))

-- | Keep only values in the stream passing a given predicate.
--
-- Since 0.3.0
filter :: Monad m => (a -> Bool) -> GInfConduit a m a
filter f = awaitForever $ \i -> when (f i) (yield i)

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
--
-- Since 0.3.0
sinkNull :: Monad m => GInfSink a m
sinkNull = awaitForever $ \_ -> return ()

-- | A source that outputs no values. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Since 0.3.0
sourceNull :: Monad m => GSource m a
sourceNull = return ()

-- | Run a @Pipe@ repeatedly, and output its result value downstream. Stops
-- when no more input is available from upstream.
--
-- Since 0.5.0
sequence :: Monad m
         => GLSink i m o -- ^ @Pipe@ to run repeatedly
         -> GLInfConduit i m o
sequence sink =
    self
  where
    self = awaitForever $ \i -> leftover i >> sink >>= yield
