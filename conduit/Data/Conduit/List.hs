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
      -- * Sinks
      -- ** Pure
    , fold
    , take
    , drop
    , head
    , zip
    , zipSinks
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
    , Ordering (..)
    , (>>)
    , flip
    , seq
    , otherwise
    , Enum (succ), Eq
    )
import Data.Conduit
import Data.Conduit.Internal (Pipe (..)) -- FIXME
import Data.Conduit.Internal
    ( pipeClose, runFinalize, pipePushStrip, noInput, Finalize (FinalizeM)
    , sourceList
    )
import Data.Monoid (mempty)
import Data.Void (absurd)
import Control.Monad (liftM, liftM2)
import Data.Conduit.Util -- FIXME remove

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
            Just (a, seed') -> HaveOutput (go seed') (return ()) a
            Nothing -> Done ()

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
        | i == stop = HaveOutput (Done ()) (return ()) i
        | otherwise = HaveOutput (go (succ i)) (return ()) i

-- | A strict left fold.
--
-- Since 0.3.0
fold :: Monad m
     => (b -> a -> b)
     -> b
     -> Pipe a o m b
fold f =
    go
  where
    go accum = NeedInput (push accum) (return accum)

    push accum input =
        let accum' = f accum input
         in accum' `seq` go accum'

-- | A monadic strict left fold.
--
-- Since 0.3.0
foldM :: Monad m
      => (b -> a -> m b)
      -> b
      -> Pipe a o m b
foldM f accum0 =
    sink accum0
  where
    sink accum = NeedInput (\a -> PipeM (push accum a) (final accum a)) (return accum)

    push accum a = do
        accum' <- f accum a
        accum' `seq` return (sink accum')

    final accum a = FinalizeM $ f accum a

-- | Apply the action to all values in the stream.
--
-- Since 0.3.0
mapM_ :: Monad m
      => (a -> m ())
      -> Pipe a o m ()
mapM_ f =
    NeedInput push close
  where
    push input = PipeM (f input >> return (NeedInput push close)) (return ())
    close = return ()

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
drop 0 = NeedInput (Leftover (Done ())) (return ())
drop count =
    NeedInput push (return ())
  where
    count' = count - 1
    push _
        | count' == 0 = Done ()
        | otherwise   = drop count'

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
take count0 =
    go count0 id
  where
    go count front = NeedInput (push count front) (return $ front [])

    push 0 front x = Leftover (Done $ front []) x
    push count front x
        | count' == 0 = Done (front [x])
        | otherwise   = NeedInput (push count' front') (return $ front' [])
      where
        count' = count - 1
        front' = front . (x:)

-- | Take a single value from the stream, if available.
--
-- Since 0.3.0
head :: Monad m => Pipe a o m (Maybe a)
head =
    NeedInput push close
  where
    push x = Done (Just x)
    close = return Nothing

-- | Look at the next value in the stream, if available. This function will not
-- change the state of the stream.
--
-- Since 0.3.0
peek :: Monad m => Pipe a o m (Maybe a)
peek =
    NeedInput push close
  where
    push x = Leftover (Done $ Just x) x
    close = Done Nothing

-- | Apply a transformation to all values in a stream.
--
-- Since 0.3.0
map :: Monad m => (a -> b) -> Conduit a m b
map f =
    NeedInput push close
  where
    push i = HaveOutput (NeedInput push close) (return ()) (f i)
    close = mempty

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Since 0.3.0
mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM f =
    NeedInput push close
  where
    push = flip PipeM (return ()) . liftM (HaveOutput (NeedInput push close) (return ())) . f
    close = mempty

-- | Apply a transformation to all values in a stream, concatenating the output
-- values.
--
-- Since 0.3.0
concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap f =
    NeedInput push close
  where
    push = haveMore (NeedInput push close) (return ()) . f
    close = mempty

-- | Apply a monadic transformation to all values in a stream, concatenating
-- the output values.
--
-- Since 0.3.0
concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f =
    NeedInput push close
  where
    push = flip PipeM (return ()) . liftM (haveMore (NeedInput push close) (return ())) . f
    close = mempty

-- | 'concatMap' with an accumulator.
--
-- Since 0.3.0
concatMapAccum :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
concatMapAccum f accum = conduitState accum push close
  where
    push state input = let (state', result) = f input state
                       in return $ StateProducing state' result
    close _ = return []

-- | 'concatMapM' with an accumulator.
--
-- Since 0.3.0
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
-- Since 0.3.0
consume :: Monad m => Pipe a o m [a]
consume =
    go id
  where
    go front = NeedInput (push front) (return $ front [])
    push front x = go (front . (x:))

-- | Grouping input according to an equality function.
--
-- Since 0.3.0
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
-- Since 0.3.0
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
-- Since 0.3.0
filter :: Monad m => (a -> Bool) -> Conduit a m a
filter f =
    NeedInput push close
  where
    push i | f i = HaveOutput (NeedInput push close) (return ()) i
    push _       = NeedInput push close
    close = mempty

-- | Ignore the remainder of values in the source. Particularly useful when
-- combined with 'isolate'.
--
-- Since 0.3.0
sinkNull :: Monad m => Pipe a o m ()
sinkNull =
    NeedInput push close
  where
    push _ = sinkNull
    close = return ()

-- | A source that returns nothing. Note that this is just a type-restricted
-- synonym for 'mempty'.
--
-- Since 0.3.0
sourceNull :: Monad m => Pipe i a m ()
sourceNull = mempty

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 0.3.0
zip :: Monad m => Source m a -> Source m b -> Source m (a, b)
zip (Leftover p i) right = zip (pipePushStrip i p) right
zip left (Leftover p i)  = zip left (pipePushStrip i p)
zip (Done ()) (Done ()) = Done ()
zip (Done ()) (HaveOutput _ close _) = PipeM (runFinalize close >> return (Done ())) close
zip (HaveOutput _ close _) (Done ()) = PipeM (runFinalize close >> return (Done ())) close
zip (Done ()) (PipeM _ close) = PipeM (runFinalize close >> return (Done ())) close
zip (PipeM _ close) (Done ()) = PipeM (runFinalize close >> return (Done ())) close
zip (PipeM mx closex) (PipeM my closey) = PipeM (liftM2 zip mx my) (closex >> closey)
zip (PipeM mx closex) y@(HaveOutput _ closey _) = PipeM (liftM (\x -> zip x y) mx) (closex >> closey)
zip x@(HaveOutput _ closex _) (PipeM my closey) = PipeM (liftM (\y -> zip x y) my) (closex >> closey)
zip (HaveOutput srcx closex x) (HaveOutput srcy closey y) = HaveOutput (zip srcx srcy) (closex >> closey) (x, y)
zip (NeedInput _ c) right = zip c right
zip left (NeedInput _ c) = zip left c


-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- If both sinks finish on the same chunk, and both report leftover input,
-- arbitrarily yield the left sink's leftover input.
--
-- Since 0.4.1
zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
zipSinks = (><)
  where
    (><) :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
    Leftover px i    >< py               = pipePushStrip i px >< py
    px               >< Leftover py i    = px >< pipePushStrip i py
    PipeM mpx mx     >< py               = PipeM (liftM (>< py) mpx) (liftM2 (,) mx (pipeClose py))
    px               >< PipeM mpy my     = PipeM (liftM (px ><) mpy) (liftM2 (,) (pipeClose px) my)

    Done x           >< Done y           = Done (x, y)

    NeedInput fpx px >< NeedInput fpy py = NeedInput (\i -> zipSinks (fpx i) (fpy i)) (px >< py)
    NeedInput fpx px >< py               = NeedInput (\i -> zipSinks (fpx i) py)      (px >< noInput py)
    px               >< NeedInput fpy py = NeedInput (\i -> zipSinks px (fpy i))      (noInput px >< py)

    HaveOutput _ _ o >< _                = absurd o
    _                >< HaveOutput _ _ o = absurd o
