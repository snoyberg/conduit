-- | Turn a 'Get' into a 'Sink' and a 'Put' into a 'Source'

module Data.Conduit.Cereal (sinkGet, conduitGet, sourcePut) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import           Data.Conduit.List (sourceList)
import           Data.Serialize

-- | Convert a 'Get' into a 'Sink'. The 'Get' will be streamed bytes until it returns 'Done' or 'Fail'.
--
-- If 'Get' succeed it will return the data read and unconsumed part of the input stream.
-- If the 'Get' fails it will return message describing the error. 

data GetState a = GetState { getPartial :: BS.ByteString -> Result a 
                           , getConsumed   :: [BS.ByteString]
                           }

initialState :: Get a -> GetState a
initialState get = GetState { getPartial  = runGetPartial get
                            , getConsumed = [] 
                            }

sinkGetWithErrorHandler :: Monad m => (String -> m (Maybe output)) -> Get output ->  C.Sink BS.ByteString m (Maybe output)
sinkGetWithErrorHandler errorHandler get = sinkState (initialState get) push close where
    
    

-- I've decieded to remove exceptions stuff, let the user decide whenever he whants exections or not.
sinkGet :: Monad m => Get output -> C.Sink BS.ByteString m (Either String output)
sinkGet get = C.NeedInput (consume partialReader) (closeEarly partialReader) where
    partialReader = runGetPartial get
    
    consume f s | BS.null s = C.NeedInput (consume f) (closeLate f)
                | otherwise = case f s of 
                      Fail msg   -> C.Done (streamToMaybe s) (Left msg)
                      Partial f' -> C.NeedInput (consume f') (closeLate f')
                      Done r s'  -> C.Done (streamToMaybe s') (Right r)
    
    closeEarly = close closeLate

    closeLate  = close (const $ error "Unexcepted result from Cereal: Partial returned for an empty byte string.")

    close p f = case f BS.empty of 
                Fail msg   -> C.Done Nothing (Left msg)  -- unexcepted end of the stream - normal situation 
                Partial f' -> p f'
                Done r s   -> C.Done (streamToMaybe s) (Right r) -- producing result without consumin - strange but acceptable

conduitGet :: Monad m => Get output -> C.Conduit BS.ByteString m (Either String output)
conduitGet get = needInput where
    needInput = C.NeedInput consumeNew close

    consumeNew = consume $ runGetPartial get

    consume f s | BS.null s = C.NeedInput (consume f) close
                | otherwise = case f s of 
                      Fail msg   -> C.HaveOutput needInput (return ()) (Left msg)
                      Partial f' -> C.NeedInput  (consume f') close
                      Done r s'  -> C.HaveOutput (consumeNew s') (return ()) (Right r)

    close = C.Done Nothing ()

streamToMaybe :: BS.ByteString -> Maybe BS.ByteString
streamToMaybe s = if BS.null s then Nothing
                               else Just s

-- | Convert a 'Put' into a 'Source'. Runs in constant memory.
sourcePut :: Monad m => Put -> C.Source m BS.ByteString
sourcePut put = sourceList $ LBS.toChunks $ runPutLazy put