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
     -> Sink a m b
fold f accum0 = Sink $ do
    iaccum <- liftBase $ I.newIORef accum0
    return $ SinkData $ go iaccum
  where
    go iaccum EOF = SinkResult [] . Just <$> liftBase (I.readIORef iaccum)
    go iaccum (Chunks cs) = do
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
     -> Sink a m [a]
take count0 = Sink $ do
    istate <- liftBase $ I.newIORef (count0, [])
    return $ SinkData $ go istate
  where
    go istate EOF = SinkResult [] . Just . snd <$> liftBase (I.readIORef istate)
    go istate (Chunks cs) = do
        (count, rest', b) <- liftBase $ I.atomicModifyIORef istate $ \(count, rest) ->
            let (a, b) = splitAt count cs
                count' = count - length a
                rest' = rest ++ a
             in ((count', rest'), (count', rest', b))
        return $ SinkResult b $ if count == 0 then Just rest' else Nothing

head :: MonadBaseControl IO m => Sink a m (Maybe a)
head =
    Sink $ return $ SinkData $ return . go
  where
    go EOF = SinkResult [] $ Just Nothing
    go (Chunks []) = SinkResult [] Nothing
    go (Chunks (a:as)) = SinkResult as (Just (Just a))

map :: Monad m => (a -> b) -> Conduit a m b
map f a = return $ ConduitResult [] $ fmap f a

concatMap :: Monad m => (a -> [b]) -> Conduit a m b
concatMap _ EOF = return $ ConduitResult [] EOF
concatMap f (Chunks l) = return $ ConduitResult [] $ Chunks $ Prelude.concatMap f l

consume :: MonadBaseControl IO m => Sink a m [a]
consume = Sink $ do
    ifront <- liftBase $ I.newIORef id
    return $ SinkData $ go ifront
  where
    go ifront EOF = SinkResult [] . Just . ($ []) <$> liftBase (I.readIORef ifront)
    go ifront (Chunks cs) = do
        liftBase $ I.atomicModifyIORef ifront $ \front -> (front . (cs ++), ())
        return $ SinkResult [] Nothing
