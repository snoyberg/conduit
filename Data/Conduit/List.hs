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
fold f accum0 = sinkM
    (liftBase $ I.newIORef accum0)
    (const $ return ())
    push
    close
  where
    close iaccum = SinkResult [] <$> liftBase (I.readIORef iaccum)
    push iaccum cs = do
        liftBase $ I.atomicModifyIORef iaccum $
            \accum -> (foldl' f accum cs, ())
        return $ SinkResult [] Nothing

fromList :: MonadBaseControl IO m => [a] -> SourceM m a
fromList l = sourceM
    (liftBase (I.newIORef l))
    (const (return ()))
    (\il -> do
        l' <- liftBase $ I.atomicModifyIORef il $ \x -> ([], x)
        return $ if null l' then EOF else Chunks l')

take :: MonadBaseControl IO m
     => Int
     -> SinkM a m [a]
take count0 = sinkM
    (liftBase $ I.newIORef (count0, []))
    (const $ return ())
    push
    close
  where
    close istate = SinkResult [] . snd <$> liftBase (I.readIORef istate)
    push istate cs = do
        (count, rest', b) <- liftBase $ I.atomicModifyIORef istate $ \(count, rest) ->
            let (a, b) = splitAt count cs
                count' = count - length a
                rest' = rest ++ a
             in ((count', rest'), (count', rest', b))
        return $ SinkResult b $ if count == 0 then Just rest' else Nothing

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
consume = sinkM
    (liftBase $ I.newIORef id)
    (const $ return ())
    push
    close
  where
    close ifront = SinkResult [] . ($ []) <$> liftBase (I.readIORef ifront)
    push ifront cs = do
        liftBase $ I.atomicModifyIORef ifront $ \front -> (front . (cs ++), ())
        return $ SinkResult [] Nothing

isolate :: MonadBaseControl IO m => Int -> ConduitM a m a
isolate count0 = conduitM
    (liftBase $ I.newIORef count0)
    (const $ return ())
    push
    close
  where
    close _ = return []
    push istate cs = do
        count <- liftBase $ I.readIORef istate
        if count == 0
            then return $ ConduitResult cs EOF
            else do
                let (a, b) = splitAt count cs
                liftBase $ I.writeIORef istate $ count - length a
                return $ ConduitResult b (Chunks a)
