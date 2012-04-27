module Data.Conduit.Cereal.Internal 
  ( ErrorHandler
  , TerminationHandler

  , mkConduitGet
  , mkSinkGet
  ) where

import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import           Data.Serialize hiding (get, put)
import           Data.Void 

-- | What should we do if the Get fails?
type ErrorHandler i o m r = String -> Maybe BS.ByteString -> C.Pipe i o m r

-- | What should we do if the stream is done before the Get is done?
type TerminationHandler i o m r = (BS.ByteString -> Result r) -> Maybe BS.ByteString -> C.Pipe i o m r

-- | Construct a conduitGet with the specified 'ErrorHandler'
mkConduitGet :: Monad m 
             => ErrorHandler BS.ByteString o m ()
             -> Get o
             -> C.Conduit BS.ByteString m o
mkConduitGet errorHandler get = consume True (runGetPartial get) [] BS.empty 
  where push f b s | BS.null s = C.NeedInput (push f b) (close b)
                   | otherwise = consume False f b s
        consume initial f b s = case f s of
          Fail msg  -> errorHandler msg (chunkedStreamToMaybe consumed)
          Partial p -> C.NeedInput (push p consumed) (close consumed)
          Done a s' -> case initial of
                         True  -> infiniteSequence a
                         False -> C.HaveOutput (push (runGetPartial get) [] s') (return ()) a
          where consumed = s : b
                infiniteSequence r = C.HaveOutput (infiniteSequence r) (return ()) r
                -- infinteSequence only works because the Get will either _always_ consume no input, or _never_ consume no input.
        close b = C.Done (chunkedStreamToMaybe b) ()

-- | Construct a sinkGet with the specified 'ErrorHandler' and 'TerminationHandler'
mkSinkGet :: Monad m 
          => ErrorHandler BS.ByteString Void m r
          -> TerminationHandler BS.ByteString Void m r 
          -> Get r
          -> C.Sink BS.ByteString m r
mkSinkGet errorHandler terminationHandler get = consume (runGetPartial get) [] BS.empty
  where push f b s
          | BS.null s = C.NeedInput (push f b) (close f b)
          | otherwise = consume f b s
        consume f b s = case f s of
          Fail msg  -> errorHandler msg (chunkedStreamToMaybe consumed)
          Partial p -> C.NeedInput (push p consumed) (close p consumed)
          Done r s' -> C.Done (streamToMaybe s') r
          where consumed = s : b
        close f = terminationHandler f . chunkedStreamToMaybe

chunkedStreamToMaybe :: [BS.ByteString] -> Maybe BS.ByteString
chunkedStreamToMaybe = streamToMaybe . BS.concat . reverse

streamToMaybe :: BS.ByteString -> Maybe BS.ByteString
streamToMaybe s = if BS.null s
                    then Nothing
                    else Just s
