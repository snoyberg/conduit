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
      -- * Sinks

      -- | Note that the @Sink@ functions do not have type @Sink@, but @Pipe@.
      -- This more general formulation allows them to be easily used to build
      -- up larger @Conduit@s. However, they all behave as @Sink@s.

      -- ** Pure
    , fold
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
    , concatMap
    , concatMapAccum
    , groupBy
    , isolate
    , filter
      -- ** Monadic
    , mapM
    , concatMapM
    , concatMapAccumM
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
    )
import Data.Conduit
import Data.Conduit.Internal (sourceList)
import Control.Monad (forever, when)
import Control.Monad.Trans.Class (lift)

-- | Generate a source from a seed value.
--
-- Since 0.4.2
unfold :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> Pipe i a m ()
unfold f =
    go
  where
    go seed =
        case f seed of
            Just (a, seed') -> tryYield a $ go seed'
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
           -> Pipe i a m ()
enumFromTo start stop =
    go start
  where
    go i
        | i == stop = tryYield i $ return ()
        | otherwise = tryYield i $ go $ succ i

-- | A strict left fold.
--
-- Since 0.3.0
fold :: Monad m
     => (b -> a -> b)
     -> b
     -> Pipe a o m b
fold f =
    loop
  where
    loop accum =
        tryAwait >>= go
      where
        go Nothing = return accum
        go (Just a) =
            let accum' = f accum a
             in accum' `seq` loop accum'

-- | A monadic strict left fold.
--
-- Since 0.3.0
foldM :: Monad m
      => (b -> a -> m b)
      -> b
      -> Pipe a o m b
foldM f =
    loop
  where
    loop accum = do
        ma <- tryAwait
        case ma of
            Nothing -> return accum
            Just a -> do
                accum' <- lift $ f accum a
                accum' `seq` loop accum'

-- | Apply the action to all values in the stream.
--
-- Since 0.3.0
mapM_ :: Monad m
      => (a -> m ())
      -> Pipe a o m ()
mapM_ f = toPipe $ forever $ await >>= lift . f

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
     -> Pipe a o m ()
drop =
    toPipe . loop
  where
    loop 0 = return ()
    loop count = await >> loop (count - 1)

-- | Take some values from the stream and return as a list. If you want to
-- instead create a conduit that pipes data to another sink, see 'isolate'.
-- This function is semantically equivalent to:
--
-- > take i = isolate i =$ consume
--
-- Since 0.3.0
take :: Monad m
     => Int
     -> Pipe a o m [a]
take =
    loop id
  where
    loop front 0 = return $ front []
    loop front count = tryAwait >>= maybe
        (return $ front [])
        (\x -> loop (front .(x:)) (count - 1))

-- | Take a single value from the stream, if available.
--
-- Since 0.3.0
head :: Monad m => Pipe a o m (Maybe a)
head = tryAwait

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.3.0
peek :: Monad m => Pipe a o m (Maybe a)
peek = tryAwait >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))

-- | Apply a transformation to all values in a stream.
--
-- Since 0.3.0
map :: Monad m => (a -> b) -> Conduit a m b
map f = toPipe $ forever $ await >>= yield . f

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Since 0.3.0
mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM f = toPipe $ forever $ await >>= lift . f >>= yield

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Since 0.3.0
concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap f = toPipe $ forever $ await >>= Prelude.mapM_ yield . f

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Since 0.3.0
concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f = toPipe $ forever $ await >>= lift . f >>= Prelude.mapM_ yield

-- | 'concatMap' with an accumulator.
--
-- Since 0.3.0
concatMapAccum :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccum f =
    toPipe . loop
  where
    loop accum = do
        a <- await
        let (accum', bs) = f a accum
        Prelude.mapM_ yield bs
        loop accum'

-- | 'concatMapM' with an accumulator.
--
-- Since 0.3.0
concatMapAccumM :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
concatMapAccumM f =
    toPipe . loop
  where
    loop accum = do
        a <- await
        (accum', bs) <- lift $ f a accum
        Prelude.mapM_ yield bs
        loop accum'

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
--
-- Since 0.3.0
consume :: Monad m => Pipe a o m [a]
consume =
    loop id
  where
    loop front = tryAwait >>= maybe (return $ front []) (\x -> loop $ front . (x:))

-- | Grouping input according to an equality function.
--
-- Since 0.3.0
groupBy :: Monad m => (a -> a -> Bool) -> Conduit a m [a]
groupBy f =
    start
  where
    start = tryAwait >>= maybe (return ()) (loop id)

    loop rest x = do
        my <- tryAwait
        case my of
            Nothing -> tryYield (x : rest []) (return ())
            Just y
                | f x y -> loop (rest . (y:)) x
                | otherwise -> tryYield (x : rest []) (loop id y)

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
    toPipe . loop
  where
    loop 0 = return ()
    loop count = await >>= \x -> yield x >> loop (count - 1)

-- | Keep only values in the stream passing a given predicate.
--
-- Since 0.3.0
filter :: Monad m => (a -> Bool) -> Conduit a m a
filter f = toPipe $ forever $ await >>= \x -> when (f x) $ yield x

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
--
-- Since 0.3.0
sinkNull :: Monad m => Pipe a o m ()
sinkNull = toPipe $ forever $ await >> return ()

-- | A source that outputs no values. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Since 0.3.0
sourceNull :: Monad m => Pipe i a m ()
sourceNull = return ()
