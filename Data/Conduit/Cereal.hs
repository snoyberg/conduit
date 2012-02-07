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
import Control.Exception (throw)

data GetException = GetException String
                  | GetDoesntConsumeInput
  deriving (Show, Typeable)

instance Exception GetException

-- | Convert a 'Get' into a 'Sink'. The 'Get' will be streamed bytes until it returns 'Done' or 'Fail'.
--
-- If the 'Get' fails, a GetException will be thrown with 'resourceThrow'. This function itself can also throw a GetException.
sinkGet :: DC.ResourceThrow m => Get output -> DC.Sink BS.ByteString m output
sinkGet get = case runGetPartial get BS.empty of
                Fail s -> throw $ GetException s
                Partial f -> DC.SinkData { DC.sinkPush = push f
                                         , DC.sinkClose = close f
                                         }
                Done _ _ -> throw GetDoesntConsumeInput
  where push f input
          | BS.null input = return $ DC.Processing (push f) (close f)
          | otherwise = case f input of
              Fail s -> lift $ DC.resourceThrow $ GetException s
              Partial f' -> return $ DC.Processing (push f') (close f')
              Done r rest -> return $ DC.Done (if BS.null rest
                                                 then Nothing
                                                 else Just rest
                                              ) r
        close f = let Fail s = f BS.empty in lift $ DC.resourceThrow $ GetException s

-- | Convert a 'Put' into a 'Source'. Runs in constant memory.
sourcePut :: DC.Resource m => Put -> DC.Source m BS.ByteString
sourcePut put = sourceList $ LBS.toChunks $ runPutLazy put
