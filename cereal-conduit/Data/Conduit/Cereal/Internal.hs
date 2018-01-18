{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}

module Data.Conduit.Cereal.Internal
  ( ConduitErrorHandler
  , SinkErrorHandler
  , SinkTerminationHandler

  , mkConduitGet
  , mkSinkGet
  ) where

import           Control.Monad (forever, when)
import qualified Data.ByteString as BS
import           Data.Conduit (ConduitT, await, leftover, yield)
import           Data.Serialize hiding (get, put)

-- | What should we do if the Get fails?
type ConduitErrorHandler m o = String -> ConduitT BS.ByteString o m ()
type SinkErrorHandler m r = forall o. String -> ConduitT BS.ByteString o m r

-- | What should we do if the stream is done before the Get is done?
type SinkTerminationHandler m r = forall o. (BS.ByteString -> Result r) -> ConduitT BS.ByteString o m r

-- | Construct a conduitGet with the specified 'ErrorHandler'
mkConduitGet :: Monad m
             => ConduitErrorHandler m o
             -> Get o
             -> ConduitT BS.ByteString o m ()
mkConduitGet errorHandler get = consume True (runGetPartial get) [] BS.empty
  where pull f b s
          | BS.null s = await >>= maybe (when (not $ null b) (leftover $ BS.concat $ reverse b)) (pull f b)
          | otherwise = consume False f b s
        consume initial f b s = case f s of
          Fail msg _ -> do
            when (not $ null b) (leftover $ BS.concat $ reverse consumed)
            errorHandler msg
          Partial p -> pull p consumed BS.empty
          Done a s' -> case initial of
                         -- this only works because the Get will either _always_ consume no input, or _never_ consume no input.
                         True  -> forever $ yield a
                         False -> yield a >> pull (runGetPartial get) [] s'
--                         False -> yield a >> leftover s' >> mkConduitGet errorHandler get
          where consumed = s : b

-- | Construct a sinkGet with the specified 'ErrorHandler' and 'TerminationHandler'
mkSinkGet :: Monad m
          => SinkErrorHandler m r
          -> SinkTerminationHandler m r
          -> Get r
          -> ConduitT BS.ByteString o m r
mkSinkGet errorHandler terminationHandler get = consume (runGetPartial get) [] BS.empty
  where pull f b s
          | BS.null s = await >>= \ x -> case x of
                          Nothing -> when (not $ null b) (leftover $ BS.concat $ reverse b) >> terminationHandler f
                          Just a -> pull f b a
          | otherwise = consume f b s
        consume f b s = case f s of
          Fail msg _ -> do
            when (not $ null b) (leftover $ BS.concat $ reverse consumed)
            errorHandler msg
          Partial p -> pull p consumed BS.empty
          Done r s' -> when (not $ BS.null s') (leftover s') >> return r
          where consumed = s : b
