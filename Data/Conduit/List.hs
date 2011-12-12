{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.List
    ( fold
    , fromList
    , take
    , map
    , concatMap
    , concatMapM
    , head
    , consume
    , isolate
    ) where

import Prelude
    ( ($), return, length, splitAt, (==), (-), IO, Int
    , (.), id, const, Maybe (..), (>>=), fmap, (++), Monad
    , null, snd
    )
import qualified Prelude
import qualified Control.Monad as Monad
import Data.Conduit
import qualified Data.IORef as I
import Control.Applicative ((<$>))
import Data.List (foldl')
import Control.Monad.Trans.Class (lift)

fold :: MonadBaseControl IO m
     => (b -> a -> b)
     -> b
     -> SinkM a m b
fold f accum0 = sinkMState
    accum0
    (\accum input -> return (foldl' f accum input, SinkResult [] Nothing))
    (return . SinkResult [])

fromList :: MonadBaseControl IO m => [a] -> SourceM m a
fromList l0 = sourceMState
    l0
    (\l -> return $ if null l
            then ([], EOF)
            else ([], Chunks l))

take :: MonadBaseControl IO m
     => Int
     -> SinkM a m [a]
take count0 = sinkMState
    (count0, id)
    push
    (\(_, x) -> return $ SinkResult [] $ x [])
  where
    push (count, front) cs = do
        let (a, b) = splitAt count cs
            count' = count - length a
            front' = front . (a ++)
            res = if count' == 0 then Just (front' []) else Nothing
        return ((count', front'), SinkResult b res)

head :: MonadBaseControl IO m => SinkM a m (Maybe a)
head = SinkM $ return $ SinkData
    { sinkPush = return . push
    , sinkClose = return $ SinkResult [] Nothing
    }
  where
    push [] = SinkResult [] Nothing
    push (a:as) = SinkResult as (Just (Just a))

map :: Monad m => (a -> b) -> ConduitM a m b
map f = ConduitM $ return $ Conduit
    { conduitPush = return . ConduitResult [] . Chunks . fmap f
    , conduitClose = return []
    }

concatMap :: Monad m => (a -> [b]) -> ConduitM a m b
concatMap f = ConduitM $ return $ Conduit
    { conduitPush = return . ConduitResult [] . Chunks . (>>= f)
    , conduitClose = return []
    }

concatMapM :: Monad m => (a -> m [b]) -> ConduitM a m b
concatMapM f = ConduitM $ return $ Conduit
    { conduitPush = fmap (ConduitResult [] . Chunks . Prelude.concat) . lift . Monad.mapM f
    , conduitClose = return []
    }

consume :: MonadBaseControl IO m => SinkM a m [a]
consume = sinkMState
    id
    (\front input -> return (front . (input ++), SinkResult [] Nothing))
    (\front -> return $ SinkResult [] $ front [])

isolate :: MonadBaseControl IO m => Int -> ConduitM a m a
isolate count0 = conduitMState
    count0
    push
    close
  where
    close _ = return []
    push count cs = do
        if count == 0
            then return (count, ConduitResult cs EOF)
            else do
                let (a, b) = splitAt count cs
                return (count - length a, ConduitResult b (Chunks a))
