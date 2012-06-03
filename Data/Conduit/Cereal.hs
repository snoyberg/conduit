{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Turn a 'Get' into a 'Sink' and a 'Put' into a 'Source'
-- These functions are built upno the Data.Conduit.Cereal.Internal functions with default
-- implementations of 'ErrorHandler' and 'TerminationHandler'
--
-- The default 'ErrorHandler' and 'TerminationHandler' both throw a 'GetException'.

module Data.Conduit.Cereal ( GetException
                           , sinkGet
                           , conduitGet
                           , sourcePut
                           , conduitPut
                           ) where

import           Control.Exception.Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Serialize hiding (get, put)
import           Data.Typeable

import           Data.Conduit.Cereal.Internal

data GetException = GetException String
  deriving (Show, Typeable)

instance Exception GetException

-- | Run a 'Get' repeatedly on the input stream, producing an output stream of whatever the 'Get' outputs.
conduitGet :: C.MonadThrow m => Get output -> C.Conduit BS.ByteString m output
conduitGet = mkConduitGet errorHandler 
  where errorHandler msg _ = pipeError $ GetException msg 

-- | Convert a 'Get' into a 'Sink'. The 'Get' will be streamed bytes until it returns 'Done' or 'Fail'.
--
-- If 'Get' succeed it will return the data read and unconsumed part of the input stream.
-- If the 'Get' fails due to deserialization error or early termination of the input stream it raise an error.
sinkGet :: C.MonadThrow m => Get r -> C.Sink BS.ByteString m r
sinkGet = mkSinkGet errorHandler terminationHandler 
  where errorHandler msg _ = pipeError $ GetException msg 
        terminationHandler f _ = let Fail msg = f BS.empty in pipeError $ GetException msg 

pipeError :: (C.MonadThrow m, Exception e) => e -> C.Pipe i o m r
pipeError e = C.PipeM (C.monadThrow e) undefined 

-- | Convert a 'Put' into a 'Source'. Runs in constant memory.
sourcePut :: Monad m => Put -> C.Source m BS.ByteString
sourcePut put = CL.sourceList $ LBS.toChunks $ runPutLazy put

-- | Run a 'Putter' repeatedly on the input stream, producing a concatenated 'ByteString' stream.
conduitPut :: Monad m => Putter input -> C.Conduit input m BS.ByteString
conduitPut p = CL.map $ runPut . p
