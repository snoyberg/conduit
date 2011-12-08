{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.List
    ( fold
    , fromList
    ) where

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
    return $ go iaccum
  where
    go iaccum EOF = SinkResult [] . Just <$> liftBase (I.readIORef iaccum)
    go iaccum (Chunks cs) = do
        liftBase $ I.atomicModifyIORef iaccum $
            \accum -> (foldl' f accum cs, ())
        return $ SinkResult [] Nothing

fromList :: MonadBaseControl IO m => [a] -> Source m a
fromList l = Source $ do
    il <- liftBase $ I.newIORef l
    unSource $ mkSource $ do
        l' <- liftBase $ I.atomicModifyIORef il $ \x -> ([], x)
        return $ if null l' then EOF else Chunks l'
  where
    unSource (Source s) = s

take :: MonadBaseControl IO m
     => Int
     -> Sink a m [a]
take count0 = Sink $ do
    istate <- liftBase $ I.newIORef (count0, [])
    return $ go istate
  where
    go istate EOF = SinkResult [] . Just . snd <$> liftBase (I.readIORef istate)
    go istate (Chunks cs) = do
        (count, rest) <- liftBase $ I.readIORef
        undefined
