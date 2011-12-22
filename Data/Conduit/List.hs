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
    (\accum input -> return $ SinkResult [] (foldl' f accum input))

foldM :: Resource m
      => (b -> a -> m b)
      -> b
      -> Sink a m b
foldM f accum0 = sinkState
    accum0
    (\accum input -> do
        accum' <- lift $ Monad.foldM f accum input
        return (accum', Processing))
    (\accum input -> do
        output <- lift $ Monad.foldM f accum input
        return $ SinkResult [] output)

mapM_ :: Resource m
      => (a -> m ())
      -> Sink a m ()
mapM_ f = sinkState
    ()
    (\() input -> do
        lift (Monad.mapM_ f input)
        return ((), Processing))
    (\() input -> do
        lift (Monad.mapM_ f input)
        return (SinkResult [] ()))

sourceList :: Resource m => [a] -> Source m a
sourceList l0 = sourceState
    l0
    (\l -> return $ if null l
            then ([], SourceResult StreamClosed [])
            else ([], SourceResult StreamOpen l))

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
            res = if count' == 0 then Done (SinkResult b ()) else assert (null b) Processing
        return (count', res)
    close count cs = do
        let (_, b) = splitAt count cs
        return $ SinkResult b ()

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
            res = if count' == 0 then Done (SinkResult b $ front' []) else assert (null b) Processing
        return ((count', front'), res)
    close (count, front) cs = do
        let (a, b) = splitAt count cs
        return $ SinkResult b $ front a

head :: Resource m => Sink a m (Maybe a)
head = sinkState
    False
    push
    close
  where
    push True _ = error "head: called after result given"
    push False [] = return (False, Processing)
    push False (a:as) = return (True, Done $ SinkResult as (Just a))
    close True _ = error "head: called after result given"
    close False [] = return $ SinkResult [] Nothing
    close False (a:as) = return $ SinkResult as (Just a)

peek :: Resource m => Sink a m (Maybe a)
peek =
    Sink $ return $ SinkData push close
  where
    push [] = return Processing
    push l@(a:_) = return $ Done $ SinkResult l $ Just a
    close [] = return $ SinkResult [] Nothing
    close l@(a:_) = return $ SinkResult l $ Just a

map :: Monad m => (a -> b) -> Conduit a m b
map f = Conduit $ return $ PreparedConduit
    { conduitPush = return . ConduitResult Processing . fmap f
    , conduitClose = \x -> return $ ConduitResult [] $ fmap f x
    }

mapM :: Monad m => (a -> m b) -> Conduit a m b
mapM f = Conduit $ return $ PreparedConduit
    { conduitPush = fmap (ConduitResult Processing) . lift . Prelude.mapM f
    , conduitClose = fmap (ConduitResult []) . lift . Prelude.mapM f
    }

concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap f = Conduit $ return $ PreparedConduit
    { conduitPush = return . ConduitResult Processing . (>>= f)
    , conduitClose = \input -> return $ ConduitResult [] (input >>= f)
    }

concatMapM :: Monad m => (a -> m [b]) -> Conduit a m b
concatMapM f = Conduit $ return $ PreparedConduit
    { conduitPush = fmap (ConduitResult Processing . Prelude.concat) . lift . Monad.mapM f
    , conduitClose = \input -> do
        x <- lift $ Monad.mapM f input
        return $ ConduitResult [] $ Prelude.concat x
    }

consume :: Resource m => Sink a m [a]
consume = sinkState
    id
    (\front input -> return (front . (input ++), Processing))
    (\front input -> return $ SinkResult [] $ front input)

isolate :: Resource m => Int -> Conduit a m a
isolate count0 = conduitState
    count0
    push
    close
  where
    close count cs = do
        let (a, b) = splitAt count cs
        return $ ConduitResult b a
    push count cs = do
        if count == 0
            then return (count, ConduitResult (Done cs) [])
            else do
                let (a, b) = splitAt count cs
                    count' = count - length a
                return (count',
                    if count' == 0
                        then ConduitResult (Done b) a
                        else assert (null b) $ ConduitResult Processing a)

filter :: Resource m => (a -> Bool) -> Conduit a m a
filter f = Conduit $ return $ PreparedConduit
    { conduitPush = return . ConduitResult Processing . Prelude.filter f
    , conduitClose = return . ConduitResult [] . Prelude.filter f
    }
