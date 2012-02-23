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
      -- * Sinks
      -- ** Pure
    , fold
    , take
    , drop
    , head
    , zip
    , peek
    , consume
    , sinkNull
      -- ** Monadic
    , foldM
    , mapM_
      -- Conduits
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

import Prelude
    ( ($), return, (==), (-), Int
    , (.), id, Maybe (..), Monad
    , Bool (..)
    , (>>)
    )
import qualified Prelude
import Data.Conduit
import Data.Monoid (mempty)
import Control.Monad (liftM)

-- | A strict left fold.
--
-- Since 0.2.0
fold :: Monad m
     => (b -> a -> b)
     -> b
     -> Sink a m b
fold f accum0 = sinkState
    accum0
    (\accum input -> return (StateProcessing $ f accum input))
    return

-- | A monadic strict left fold.
--
-- Since 0.2.0
foldM :: Monad m
      => (b -> a -> m b)
      -> b
      -> Sink a m b
foldM f accum0 = sinkState
    accum0
    (\accum input -> do
        accum' <- f accum input
        return $ StateProcessing accum'
    )
    return

-- | Apply the action to all values in the stream.
--
-- Since 0.2.0
mapM_ :: Monad m
      => (a -> m ())
      -> Sink a m ()
mapM_ f =
    SinkData push close
  where
    push input = f input >> return (Processing push close)
    close = return ()

-- | Convert a list into a source.
--
-- Since 0.2.0
sourceList :: Monad m => [a] -> Source m a
sourceList l0 =
    sourceState l0 go
  where
    go [] = return StateClosed
    go (x:xs) = return $ StateOpen xs x

-- | Ignore a certain number of values in the stream. This function is
-- semantically equivalent to:
--
-- > drop i = take i >> return ()
--
-- However, @drop@ is more efficient as it does not need to hold values in
-- memory.
--
-- Since 0.2.0
drop :: Monad m
     => Int
     -> Sink a m ()
drop count0 = sinkState
    count0
    push
    close
  where
    push 0 x = return $ StateDone (Just x) ()
    push count _ = do
        let count' = count - 1
        return $ if count' == 0
            then StateDone Nothing ()
            else StateProcessing count'
    close _ = return ()

-- | Take some values from the stream and return as a list. If you want to
-- instead create a conduit that pipes data to another sink, see 'isolate'.
-- This function is semantically equivalent to:
--
-- > take i = isolate i =$ consume
--
-- Since 0.2.0
take :: Monad m
     => Int
     -> Sink a m [a]
take count0 = sinkState
    (count0, id)
    push
    close
  where
    push (0, front) x = return (StateDone (Just x) (front []))
    push (count, front) x = do
        let count' = count - 1
            front' = front . (x:)
        return $ if count' == 0
                    then StateDone Nothing (front' [])
                    else StateProcessing (count', front')
    close (_, front) = return $ front []

-- | Take a single value from the stream, if available.
--
-- Since 0.2.0
head :: Monad m => Sink a m (Maybe a)
head =
    SinkData push close
  where
    push x = return $ Done Nothing (Just x)
    close = return Nothing

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.2.0
peek :: Monad m => Sink a m (Maybe a)
peek =
    SinkData push close
  where
    push x = return $ Done (Just x) (Just x)
    close = return Nothing

-- | Apply a transformation to all values in a stream.
--
-- Since 0.2.0
map :: Monad m => (a -> b) -> Conduit a m b
map f =
    conduit
  where
    conduit = Conduit push close
    push = return . Producing conduit . return . f
    close = return []

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Since 0.2.0
mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM f =
    conduit
  where
    conduit = Conduit push close
    push = liftM (Producing conduit . return) . f
    close = return []

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Since 0.2.0
concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap f =
    conduit
  where
    conduit = Conduit push close
    push = return . Producing conduit . f
    close = return []

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Since 0.2.0
concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f =
    conduit
  where
    conduit = Conduit push close
    push = liftM (Producing conduit) . f
    close = return []

-- | 'concatMap' with an accumulator.
--
-- Since 0.2.0
concatMapAccum :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccum f accum = conduitState accum push close
  where
    push state input = let (state', result) = f input state
                       in return $ StateProducing state' result
    close _ = return []

-- | 'concatMapM' with an accumulator.
--
-- Since 0.2.0
concatMapAccumM :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
concatMapAccumM f accum = conduitState accum push close
  where
    push state input = do (state', result) <- f input state
                          return $ StateProducing state' result
    close _ = return []

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
--
-- Since 0.2.0
consume :: Monad m => Sink a m [a]
consume = sinkState
    id
    (\front input -> return (StateProcessing $ front . (input :)))
    (\front -> return $ front [])

-- | Grouping input according to an equality function.
--
-- Since 0.2.0
groupBy :: Monad m => (a -> a -> Bool) -> Conduit a m [a]
groupBy f = conduitState
    []
    push
    close
  where
    push []      v = return $ StateProducing [v] []
    push s@(x:_) v =
      if f x v then
        return $ StateProducing (v:s) []
      else
        return $ StateProducing [v] [s]
    close s = return [s]

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
-- Since 0.2.0
isolate :: Monad m => Int -> Conduit a m a
isolate count0 = conduitState
    count0
    push
    close
  where
    close _ = return []
    push count x = do
        if count == 0
            then return $ StateFinished (Just x) []
            else do
                let count' = count - 1
                return $ if count' == 0
                    then StateFinished Nothing [x]
                    else StateProducing count' [x]

-- | Keep only values in the stream passing a given predicate.
--
-- Since 0.2.0
filter :: Monad m => (a -> Bool) -> Conduit a m a
filter f =
    conduit
  where
    conduit = Conduit push close
    push = return . Producing conduit . Prelude.filter f . return
    close = return []

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
--
-- Since 0.2.0
sinkNull :: Monad m => Sink a m ()
sinkNull =
    SinkData push close
  where
    push _ = return $ Processing push close
    close = return ()

-- | A source that returns nothing. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Since 0.2.0
sourceNull :: Monad m => Source m a
sourceNull = mempty

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 0.2.2
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip sa sb = Source pull close
    where
        pull = do ra <- sourcePull sa
                  case ra of
                    Closed -> return Closed
                    Open ra' a -> do rb <- sourcePull sb
                                     case rb of
                                        Closed -> return Closed
                                        Open rb' b -> return $ Open (zip ra' rb') (a, b)
        close = sourceClose sa >> sourceClose sb

