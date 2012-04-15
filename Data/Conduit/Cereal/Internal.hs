module Data.Conduit.Cereal.Internal 
    ( DeserializationErrorHandler
    , EarlyTerminationSinkHandler
    , EarlyTerminationConduitHandler

    , mkConduitGet
    , mkSinkGet
    ) where

import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import           Data.Serialize hiding (get, put)
import           Data.Void 

type DeserializationErrorHandler i o m r = String -> Maybe BS.ByteString -> C.Pipe i o m r

type EarlyTerminationConduitHandler i o m r = (BS.ByteString -> Result o) -> Maybe BS.ByteString -> C.Pipe i o m r

type EarlyTerminationSinkHandler i o m r = (BS.ByteString -> Result r) -> Maybe BS.ByteString -> C.Pipe i o m r

mkConduitGet 
    :: Monad m 
    => DeserializationErrorHandler BS.ByteString o m ()
    -> Get o 
    -> C.Conduit BS.ByteString m o
mkConduitGet deserializarionError get = consume True (runGetPartial get) [] BS.empty where
    push f b s 
        | BS.null s = C.NeedInput (push f b) (close b)
        | otherwise = consume False f b s

    consume initial f b s = case f s of
        Fail msg  -> deserializarionError msg (chunkedStreamToMaybe consumed)
        Partial p -> C.NeedInput (push p consumed) (close consumed)
        Done r s' -> case initial of
            True  -> infiniteSequence r
            False -> C.HaveOutput (push (runGetPartial get) [] s') (return ()) r
        where consumed = s : b
              infiniteSequence r = C.HaveOutput (infiniteSequence r) (return ()) r

    close b = C.Done (chunkedStreamToMaybe b) ()

mkSinkGet 
    :: Monad m 
    => DeserializationErrorHandler BS.ByteString Void m r
    -> EarlyTerminationSinkHandler BS.ByteString Void m r 
    -> Get r
    -> C.Sink BS.ByteString m r
mkSinkGet deserializarionError earlyTermination get = consume (runGetPartial get) [] BS.empty where
    push f b s
        | BS.null s = C.NeedInput (push f b) (close f b)
        | otherwise = consume f b s

    consume f b s = case f s of
        Fail msg  -> deserializarionError msg (chunkedStreamToMaybe consumed)
        Partial p -> C.NeedInput (push p consumed) (close p consumed)
        Done r s' -> C.Done (streamToMaybe s') r
        where consumed = s : b

    close f = earlyTermination f . chunkedStreamToMaybe

chunkedStreamToMaybe :: [BS.ByteString] -> Maybe BS.ByteString
chunkedStreamToMaybe = streamToMaybe . BS.concat . reverse

streamToMaybe :: BS.ByteString -> Maybe BS.ByteString
streamToMaybe s = if BS.null s
                    then Nothing
                    else Just s