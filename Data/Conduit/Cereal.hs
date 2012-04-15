{-# LANGUAGE FlexibleContexts #-}

-- | Turn a 'Get' into a 'Sink' and a 'Put' into a 'Source'

module Data.Conduit.Cereal (GetError, sinkGet, conduitGet, sourcePut) where

import           Control.Monad.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import           Data.Conduit.Cereal.Internal
import           Data.Conduit.List (sourceList)
import           Data.Serialize hiding (get, put)

data GetError = GetError String
  deriving (Show, Eq)

instance Error GetError where
  noMsg = GetError ""
  strMsg = GetError

-- | Run a 'Get' repeatedly on the input stream, producing an output stream of whatever the 'Get' outputs.
conduitGet :: MonadError GetError m => Get output -> C.Conduit BS.ByteString m output
conduitGet = mkConduitGet deserializarionError where
    deserializarionError msg _ = pipeError $ strMsg msg 

-- | Convert a 'Get' into a 'Sink'. The 'Get' will be streamed bytes until it returns 'Done' or 'Fail'.
--
-- If 'Get' succeed it will return the data read and unconsumed part of the input stream.
-- If the 'Get' fails due to deserialization error or early termination of the input stream it raise an error.
sinkGet :: MonadError GetError m => Get r -> C.Sink BS.ByteString m r
sinkGet = mkSinkGet deserializarionError earlyTermination where
    deserializarionError msg _ = pipeError $ strMsg msg 
    earlyTermination f _ = let Fail msg = f BS.empty in pipeError $ strMsg msg 

pipeError :: MonadError e m => e -> C.Pipe i o m r
pipeError e = C.PipeM trow (lift trow) where
    trow = throwError e

-- | Convert a 'Put' into a 'Source'. Runs in constant memory.
sourcePut :: Monad m => Put -> C.Source m BS.ByteString
sourcePut put = sourceList $ LBS.toChunks $ runPutLazy put