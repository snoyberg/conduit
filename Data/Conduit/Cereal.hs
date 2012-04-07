-- | Turn a 'Get' into a 'Sink' and a 'Put' into a 'Source'

module Data.Conduit.Cereal where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import           Data.Conduit.List (sourceList)
import           Data.Serialize

-- | Convert a 'Get' into a 'Sink'. The 'Get' will be streamed bytes until it returns 'Done' or 'Fail'.
--
-- If 'Get' succeed it will return the data read and unconsumed part of the input stream.
-- If the 'Get' fails it will return message describing the error. 
sinkGet :: Monad m => Get output -> C.Sink BS.ByteString m (Either String output)
sinkGet get = consume (runGetPartial get) BS.empty
  where push f input
          | BS.null input = C.NeedInput (push f) (close f)
          | otherwise = consume f input
        consume f s = case f s of
          Fail msg   -> C.Done (streamToMaybe s) (Left msg)
          Partial f' -> C.NeedInput (push f') (close f')
          Done r s'  -> C.Done (streamToMaybe s') (Right r)
        close f = let Fail r = f BS.empty in C.Done Nothing (Left r)
        streamToMaybe s = if BS.null s
                            then Nothing
                            else Just s

-- | Convert a 'Put' into a 'Source'. Runs in constant memory.
sourcePut :: Monad m => Put -> C.Source m BS.ByteString
sourcePut put = sourceList $ LBS.toChunks $ runPutLazy put
