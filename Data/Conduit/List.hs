{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.List
    ( fold
    , fromList
    , take
    , map
    , concatMap
    , head
    , consume
    ) where

import qualified Prelude
import Prelude hiding (take, map, concatMap, head)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (liftBase)
import qualified Data.IORef as I
import Control.Applicative ((<$>))
import Data.List (foldl')

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

map :: Monad m => (a -> b) -> Conduit a m b
map f a = return $ ConduitResult [] $ fmap f a

concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap _ EOF = return $ ConduitResult [] EOF
concatMap f (Chunks l) = return $ ConduitResult [] $ Chunks $ Prelude.concatMap f l

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
