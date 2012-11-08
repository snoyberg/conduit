{-# LANGUAGE Rank2Types #-}

module Data.Conduit.Cereal.Internal
  ( ConduitErrorHandler
  , SinkErrorHandler
  , SinkTerminationHandler

  , mkConduitGet
  , mkSinkGet
  ) where

import           Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import           Data.Serialize hiding (get, put)

-- | What should we do if the Get fails?
type ConduitErrorHandler m o = String -> C.GLConduit BS.ByteString m o
type SinkErrorHandler m r = String -> C.GLSink BS.ByteString m r

-- | What should we do if the stream is done before the Get is done?
type SinkTerminationHandler m r = (BS.ByteString -> Result r) -> C.GLSink BS.ByteString m r

-- | Construct a conduitGet with the specified 'ErrorHandler'
mkConduitGet :: C.MonadThrow m
             => ConduitErrorHandler m o
             -> Get o
             -> C.GLConduit BS.ByteString m o
mkConduitGet errorHandler get = consume True (runGetPartial get) [] BS.empty
  where pull f b s
          | BS.null s = C.await >>= \ x -> case x of
                          Nothing -> when (not $ null b) (C.leftover $ BS.concat $ reverse b)
                          Just a -> pull f b a
          | otherwise = consume False f b s
        consume initial f b s = case f s of
          Fail msg  -> do
            when (not $ null b) (C.leftover $ BS.concat $ reverse consumed)
            errorHandler msg
          Partial p -> pull p consumed BS.empty
          Done a s' -> case initial of
                         -- this only works because the Get will either _always_ consume no input, or _never_ consume no input.
                         True  -> sequence_ $ repeat $ C.yield a
                         False -> C.yield a >> pull (runGetPartial get) [] s'
--                         False -> C.yield a >> C.leftover s' >> mkConduitGet errorHandler get
          where consumed = s : b

-- | Construct a sinkGet with the specified 'ErrorHandler' and 'TerminationHandler'
mkSinkGet :: Monad m
          => SinkErrorHandler m r
          -> SinkTerminationHandler m r
          -> Get r
          -> C.GLSink BS.ByteString m r
mkSinkGet errorHandler terminationHandler get = consume (runGetPartial get) [] BS.empty
  where pull f b s
          | BS.null s = C.await >>= \ x -> case x of
                          Nothing -> do
                            when (not $ null b) (C.leftover $ BS.concat $ reverse b)
                            terminationHandler f
                          Just a -> pull f b a
          | otherwise = consume f b s
        consume f b s = case f s of
          Fail msg  -> do
            when (not $ null b) (C.leftover $ BS.concat $ reverse consumed)
            errorHandler msg
          Partial p -> pull p consumed BS.empty
          Done r s' -> when (not $ BS.null s') (C.leftover s') >> return r
          where consumed = s : b
