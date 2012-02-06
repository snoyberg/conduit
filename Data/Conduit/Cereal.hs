{-# LANGUAGE DeriveDataTypeable #-}
-- | Turn a 'Get' into a 'Sink' and a 'Put' into a 'Source'
module Data.Conduit.Cereal where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Monad.Trans
import qualified Data.Conduit as DC
import Data.Conduit.List (sourceList)
import Data.Serialize.Get
import Data.Serialize.Put

data GetError = GetError String
              | PrematureClose
  deriving (Show, Typeable)

instance Exception GetError

-- | Convert a 'Get' into a 'Sink'. The 'Get' will be streamed bytes until it returns 'Done' or 'Fail'.
--
-- If the 'Get' fails, a GetError will be thrown with 'resourceThrow'
sinkGet :: DC.ResourceThrow m => Get output -> DC.Sink BS.ByteString m output
sinkGet get = DC.SinkData { DC.sinkPush = push (runGetPartial get)
                          , DC.sinkClose = close
                          }
  where push f input = do
          case f input of
            Fail s -> lift $ DC.resourceThrow $ GetError s
            Partial f' -> return $ DC.Processing (push f') close
            Done r rest -> return $ DC.Done (if BS.null rest
                                               then Nothing
                                               else Just rest
                                            ) r
        close = lift $ DC.resourceThrow PrematureClose

-- | Convert a 'Put' into a 'Source'. Runs in constant memory.
sourcePut :: DC.Resource m => Put -> DC.Source m BS.ByteString
sourcePut put = sourceList $ LBS.toChunks $ runPutLazy put
