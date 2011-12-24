{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.List
    ( fold
    , foldM
    , mapM_
    , sourceList
    , take
    , drop
    , map
    , mapM
    , concatMap
    , concatMapM
    , head
    , peek
    , consume
    , isolate
    , filter
    ) where

import Prelude
    ( ($), return, length, splitAt, (==), (-), Int
    , (.), id, Maybe (..), (>>=), fmap, (++), Monad
    , null, Bool (..), error
    , (>>)
    )
import qualified Prelude
import qualified Control.Monad as Monad
import Data.Conduit
import Data.List (foldl')
import Control.Monad.Trans.Class (lift)
import Control.Exception (assert)

fold :: Resource m
     => (b -> a -> b)
     -> b
     -> Sink a m b
fold f accum0 = sinkState
    accum0
    (\accum input -> return (f accum input, Processing))
    return

foldM :: Resource m
      => (b -> a -> m b)
      -> b
      -> Sink a m b
foldM f accum0 = sinkState
    accum0
    (\accum input -> do
        accum' <- lift $ f accum input
        return (accum', Processing))
    return

mapM_ :: Resource m
      => (a -> m ())
      -> Sink a m ()
mapM_ f = Sink $ return $ SinkData
    (\input -> lift (f input) >> return Processing)
    (return ())

sourceList :: Resource m => [a] -> Source m a
sourceList l0 =
    sourceState l0 go
  where
    go [] = return ([], Closed)
    go (x:xs) = return (xs, Open x)

drop :: Resource m
     => Int
     -> Sink a m ()
drop count0 = sinkState
    count0
    push
    close
  where
    push 0 x = return (0, Done (Just x) ())
    push count x = do
        let count' = count - 1
        return (count', if count' == 0
                            then Done Nothing ()
                            else Processing)
    close _ = return ()

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

head :: Resource m => Sink a m (Maybe a)
head =
    Sink $ return $ SinkData push close
  where
    push x = return $ Done Nothing (Just x)
    close = return Nothing

peek :: Resource m => Sink a m (Maybe a)
peek =
    Sink $ return $ SinkData push close
  where
    push x = return $ Done (Just x) (Just x)
    close = return Nothing

map :: Monad m => (a -> b) -> Conduit a m b
map f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . return . f
    , conduitClose = return []
    }

mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM f = Conduit $ return $ PreparedConduit
    { conduitPush = fmap (Producing . return) . lift . f
    , conduitClose = return []
    }

concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . f
    , conduitClose = return []
    }

concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f = Conduit $ return $ PreparedConduit
    { conduitPush = fmap Producing . lift . f
    , conduitClose = return []
    }

consume :: Resource m => Sink a m [a]
consume = sinkState
    id
    (\front input -> return (front . (input :), Processing))
    (\front -> return $ front [])

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

filter :: Resource m => (a -> Bool) -> Conduit a m a
filter f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . Prelude.filter f . return
    , conduitClose = return []
    }
