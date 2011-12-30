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
    , concatMap
    , isolate
    , filter
      -- ** Monadic
    , mapM
    , concatMapM
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

-- | A strict left fold.
fold :: Resource m
     => (b -> a -> b)
     -> b
     -> Sink a m b
fold f accum0 = sinkState
    accum0
    (\accum input -> return (f accum input, Processing))
    return

-- | A monadic strict left fold.
foldM :: Resource m
      => (b -> a -> m b)
      -> b
      -> Sink a m b
foldM f accum0 = sinkState
    accum0
    (\accum input -> do
        accum' <- lift $ f accum input
        return (accum', Processing)
    )
    return

-- | Apply the action to all values in the stream.
mapM_ :: Resource m
      => (a -> m ())
      -> Sink a m ()
mapM_ f = Sink $ return $ SinkData
    (\input -> lift (f input) >> return Processing)
    (return ())

-- | Convert a list into a source.
sourceList :: Resource m => [a] -> Source m a
sourceList l0 =
    sourceState l0 go
  where
    go [] = return ([], Closed)
    go (x:xs) = return (xs, Open x)

-- | Ignore a certain number of values in the stream. This function is
-- semantically equivalent to:
--
-- > drop i = take i >> return ()
--
-- However, @drop@ is more efficient as it does not need to hold values in
-- memory.
drop :: Resource m
     => Int
     -> Sink a m ()
drop count0 = sinkState
    count0
    push
    close
  where
    push 0 x = return (0, Done (Just x) ())
    push count _ = do
        let count' = count - 1
        return (count', if count' == 0
                            then Done Nothing ()
                            else Processing)
    close _ = return ()

-- | Take some values from the stream and return as a list. If you want to
-- instead create a conduit that pipes data to another sink, see 'isolate'.
-- This function is semantically equivalent to:
--
-- > take i = isolate i =$ consume
take :: Resource m
     => Int
     -> Sink a m [a]
take count0 = sinkState
    (count0, id)
    push
    close
  where
    push (0, front) x = return ((0, front), Done (Just x) (front []))
    push (count, front) x = do
        let count' = count - 1
            front' = front . (x:)
            res = if count' == 0
                    then Done Nothing (front' [])
                    else Processing
        return ((count', front'), res)
    close (_, front) = return $ front []

-- | Take a single value from the stream, if available.
head :: Resource m => Sink a m (Maybe a)
head =
    Sink $ return $ SinkData push close
  where
    push x = return $ Done Nothing (Just x)
    close = return Nothing

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
peek :: Resource m => Sink a m (Maybe a)
peek =
    Sink $ return $ SinkData push close
  where
    push x = return $ Done (Just x) (Just x)
    close = return Nothing

-- | Apply a transformation to all values in a stream.
map :: Monad m => (a -> b) -> Conduit a m b
map f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . return . f
    , conduitClose = return []
    }

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM f = Conduit $ return $ PreparedConduit
    { conduitPush = fmap (Producing . return) . lift . f
    , conduitClose = return []
    }

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . f
    , conduitClose = return []
    }

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f = Conduit $ return $ PreparedConduit
    { conduitPush = fmap Producing . lift . f
    , conduitClose = return []
    }

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory. For a lazy variant, see
-- "Data.Conduit.Lazy".
consume :: Resource m => Sink a m [a]
consume = sinkState
    id
    (\front input -> return (front . (input :), Processing))
    (\front -> return $ front [])

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
isolate :: Resource m => Int -> Conduit a m a
isolate count0 = conduitState
    count0
    push
    close
  where
    close _ = return []
    push count x = do
        if count == 0
            then return (count, Finished (Just x) [])
            else do
                let count' = count - 1
                return (count',
                    if count' == 0
                        then Finished Nothing [x]
                        else Producing [x])

-- | Keep only values in the stream passing a given predicate.
filter :: Resource m => (a -> Bool) -> Conduit a m a
filter f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . Prelude.filter f . return
    , conduitClose = return []
    }

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
sinkNull :: Resource m => Sink a m ()
sinkNull = Sink $ return $ SinkData
    (\_ -> return Processing)
    (return ())
