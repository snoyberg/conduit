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
    (\accum input -> return (foldl' f accum input, Processing))
    return

foldM :: Resource m
      => (b -> a -> m b)
      -> b
      -> Sink a m b
foldM f accum0 = sinkState
    accum0
    (\accum input -> do
        accum' <- lift $ Monad.foldM f accum input
        return (accum', Processing))
    return

mapM_ :: Resource m
      => (a -> m ())
      -> Sink a m ()
mapM_ f = Sink $ return $ SinkData
    (\input -> lift (Monad.mapM_ f input) >> return Processing)
    (return ())

sourceList :: Resource m => [a] -> Source m a
sourceList l0 = sourceState
    l0
    (\l -> return $ if null l
            then ([], Closed)
            else ([], Open l))

drop :: Resource m
     => Int
     -> Sink a m ()
drop count0 = sinkState
    count0
    push
    close
  where
    push count cs = do
        let (a, b) = splitAt count cs
            count' = count - length a
            res = if count' == 0 then Done b () else assert (null b) Processing
        return (count', res)
    close _ = return ()

take :: Resource m
     => Int
     -> Sink a m [a]
take count0 = sinkState
    (count0, id)
    push
    close
  where
    push (count, front) cs = do
        let (a, b) = splitAt count cs
            count' = count - length a
            front' = front . (a ++)
            res = if count' == 0 then Done b (front' []) else assert (null b) Processing
        return ((count', front'), res)
    close (_, front) = return $ front []

head :: Resource m => Sink a m (Maybe a)
head = sinkState
    False
    push
    close
  where
    push True _ = error "head: called after result given"
    push False [] = return (False, Processing)
    push False (a:as) = return (True, Done as (Just a))
    close True = error "head: called after result given"
    close False = return Nothing

peek :: Resource m => Sink a m (Maybe a)
peek =
    Sink $ return $ SinkData push close
  where
    push [] = return Processing
    push l@(a:_) = return $ Done l $ Just a
    close = return Nothing

map :: Monad m => (a -> b) -> Conduit a m b
map f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . fmap f
    , conduitClose = return []
    }

mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM f = Conduit $ return $ PreparedConduit
    { conduitPush = fmap Producing . lift . Prelude.mapM f
    , conduitClose = return []
    }

concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . (>>= f)
    , conduitClose = return []
    }

concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f = Conduit $ return $ PreparedConduit
    { conduitPush = fmap (Producing . Prelude.concat) . lift . Monad.mapM f
    , conduitClose = return []
    }

consume :: Resource m => Sink a m [a]
consume = sinkState
    id
    (\front input -> return (front . (input ++), Processing))
    (\front -> return $ front [])

isolate :: Resource m => Int -> Conduit a m a
isolate count0 = conduitState
    count0
    push
    close
  where
    close _ = return []
    push count cs = do
        if count == 0
            then return (count, Finished cs [])
            else do
                let (a, b) = splitAt count cs
                    count' = count - length a
                return (count',
                    if count' == 0
                        then Finished b a
                        else assert (null b) $ Producing a)

filter :: Resource m => (a -> Bool) -> Conduit a m a
filter f = Conduit $ return $ PreparedConduit
    { conduitPush = return . Producing . Prelude.filter f
    , conduitClose = return []
    }
