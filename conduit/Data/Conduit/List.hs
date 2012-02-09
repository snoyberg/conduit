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
    , peek
    , consume
    , sinkNull
      -- ** Monadic
    , foldM
    , mapM_
      -- Conduits
      -- ** Pure
    , map
    , zip
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
    , (.), id, Maybe (..), fmap, Monad
    , Bool (..)
    , (>>)
    )
import qualified Prelude
import Data.Conduit
import Control.Monad.Trans.Class (lift)
import Data.Monoid (mempty)

-- | A strict left fold.
--
-- Since 0.2.0
fold :: Resource m
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
foldM :: Resource m
      => (b -> a -> m b)
      -> b
      -> Sink a m b
foldM f accum0 = sinkState
    accum0
    (\accum input -> do
        accum' <- lift $ f accum input
        return $ StateProcessing accum'
    )
    return

-- | Apply the action to all values in the stream.
--
-- Since 0.2.0
mapM_ :: Resource m
      => (a -> m ())
      -> Sink a m ()
mapM_ f =
    SinkData push close
  where
    push input = lift (f input) >> return (Processing push close)
    close = return ()

-- | Convert a list into a source.
--
-- Since 0.2.0
sourceList :: Resource m => [a] -> Source m a
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
drop :: Resource m
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
take :: Resource m
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
head :: Resource m => Sink a m (Maybe a)
head =
    SinkData push close
  where
    push x = return $ Done Nothing (Just x)
    close = return Nothing

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.2.0
peek :: Resource m => Sink a m (Maybe a)
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

-- | Injects a list into a stream, zipping it with whatever values come by.
--   The conduit will stop producing after either the list is exhausted, or the
--   stream is closed.
zip :: Monad m => [b] -> Conduit a m (a, b)
zip [] = Conduit emptyPush close
    where
        emptyPush x = return $ Finished (Just x) []
        close = return []
zip (y:ys) = Conduit push close
    where
        push x = return $ Producing (zip ys) [(x, y)]
        close  = return []

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
    push = fmap (Producing conduit . return) . lift . f
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
    push = fmap (Producing conduit) . lift . f
    close = return []

-- | 'concatMap' with an accumulator.
--
-- Since 0.2.0
concatMapAccum :: Resource m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccum f accum = conduitState accum push close
  where
    push state input = let (state', result) = f input state
                       in return $ StateProducing state' result
    close _ = return []

-- | 'concatMapM' with an accumulator.
--
-- Since 0.2.0
concatMapAccumM :: Resource m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
concatMapAccumM f accum = conduitState accum push close
  where
    push state input = do (state', result) <- lift (f input state)
                          return $ StateProducing state' result
    close _ = return []

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
--
-- Since 0.2.0
consume :: Resource m => Sink a m [a]
consume = sinkState
    id
    (\front input -> return (StateProcessing $ front . (input :)))
    (\front -> return $ front [])

-- | Grouping input according to an equality function.
--
-- Since 0.2.0
groupBy :: Resource m => (a -> a -> Bool) -> Conduit a m [a]
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
isolate :: Resource m => Int -> Conduit a m a
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
filter :: Resource m => (a -> Bool) -> Conduit a m a
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
sinkNull :: Resource m => Sink a m ()
sinkNull =
    SinkData push close
  where
    push _ = return $ Processing push close
    close = return ()

-- | A source that returns nothing. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Since 0.2.0
sourceNull :: Resource m => Source m a
sourceNull = mempty
