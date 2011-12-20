{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.List
    ( fold
    , foldM
    , mapM_
    , fromList
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
     -> SinkM a m b
fold f accum0 = sinkMState
    accum0
    (\accum input -> return (foldl' f accum input, Processing))
    (\accum input -> return $ SinkResult [] (foldl' f accum input))

foldM :: Resource m
      => (b -> a -> m b)
      -> b
      -> SinkM a m b
foldM f accum0 = sinkMState
    accum0
    (\accum input -> do
        accum' <- lift $ Monad.foldM f accum input
        return (accum', Processing))
    (\accum input -> do
        output <- lift $ Monad.foldM f accum input
        return $ SinkResult [] output)

mapM_ :: Resource m
      => (a -> m ())
      -> SinkM a m ()
mapM_ f = sinkMState
    ()
    (\() input -> do
        lift (Monad.mapM_ f input)
        return ((), Processing))
    (\() input -> do
        lift (Monad.mapM_ f input)
        return (SinkResult [] ()))

fromList :: Resource m => [a] -> SourceM m a
fromList l0 = sourceMState
    l0
    (\l -> return $ if null l
            then ([], SourceResult StreamClosed [])
            else ([], SourceResult StreamOpen l))

drop :: Resource m
     => Int
     -> SinkM a m ()
drop count0 = sinkMState
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
     -> SinkM a m [a]
take count0 = sinkMState
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

head :: Resource m => SinkM a m (Maybe a)
head = sinkMState
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

peek :: Resource m => SinkM a m (Maybe a)
peek =
    SinkM $ return $ SinkData push close
  where
    push [] = return Processing
    push l@(a:_) = return $ Done $ SinkResult l $ Just a
    close [] = return $ SinkResult [] Nothing
    close l@(a:_) = return $ SinkResult l $ Just a

map :: Monad m => (a -> b) -> ConduitM a m b
map f = ConduitM $ return $ Conduit
    { conduitPush = return . ConduitResult Processing . fmap f
    , conduitClose = \x -> return $ ConduitResult [] $ fmap f x
    }

mapM :: Monad m => (a -> m b) -> ConduitM a m b
mapM f = ConduitM $ return $ Conduit
    { conduitPush = fmap (ConduitResult Processing) . lift . Prelude.mapM f
    , conduitClose = fmap (ConduitResult []) . lift . Prelude.mapM f
    }

concatMap :: Monad m => (a -> [b]) -> ConduitM a m b
concatMap f = ConduitM $ return $ Conduit
    { conduitPush = return . ConduitResult Processing . (>>= f)
    , conduitClose = \input -> return $ ConduitResult [] (input >>= f)
    }

concatMapM :: Monad m => (a -> m [b]) -> ConduitM a m b
concatMapM f = ConduitM $ return $ Conduit
    { conduitPush = fmap (ConduitResult Processing . Prelude.concat) . lift . Monad.mapM f
    , conduitClose = \input -> do
        x <- lift $ Monad.mapM f input
        return $ ConduitResult [] $ Prelude.concat x
    }

consume :: Resource m => SinkM a m [a]
consume = sinkMState
    id
    (\front input -> return (front . (input ++), Processing))
    (\front input -> return $ SinkResult [] $ front input)

isolate :: Resource m => Int -> ConduitM a m a
isolate count0 = conduitMState
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
