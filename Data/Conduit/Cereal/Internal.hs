module Data.Conduit.Cereal.Internal 
  ( ErrorHandler
  , ResultMapper
  , TerminationHandler

  , mkConduitGet
  , mkSinkGet
  ) where

import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import           Data.Serialize hiding (get, put)
import           Data.Void 

type ErrorHandler i o m r = String -> Maybe BS.ByteString -> C.Pipe i o m r

type ResultMapper a b = a -> b

type TerminationHandler i o m r = (BS.ByteString -> Result r) -> Maybe BS.ByteString -> C.Pipe i o m r

mkConduitGet :: Monad m 
             => ResultMapper a o
             -> ErrorHandler BS.ByteString o m ()
             -> Get a
             -> C.Conduit BS.ByteString m o
mkConduitGet resultMapper errorHandler get = consume True (runGetPartial get) [] BS.empty 
  where push f b s | BS.null s = C.NeedInput (push f b) (close b)
                   | otherwise = consume False f b s
        consume initial f b s = case f s of
          Fail msg  -> errorHandler msg (chunkedStreamToMaybe consumed)
          Partial p -> C.NeedInput (push p consumed) (close consumed)
          Done a s' -> case initial of
                         True  -> infiniteSequence (resultMapper a)
                         False -> C.HaveOutput (push (runGetPartial get) [] s') (return ()) (resultMapper a)
          where consumed = s : b
                infiniteSequence r = C.HaveOutput (infiniteSequence r) (return ()) r

        close b = C.Done (chunkedStreamToMaybe b) ()

mkSinkGet :: Monad m 
          => ResultMapper a r
          -> ErrorHandler BS.ByteString Void m r
          -> TerminationHandler BS.ByteString Void m r 
          -> Get a
          -> C.Sink BS.ByteString m r
mkSinkGet resultMapper errorHandler terminationHandler get = consume (runGetPartial get) [] BS.empty
  where push f b s
          | BS.null s = C.NeedInput (push f b) (close f b)
          | otherwise = consume f b s
        consume f b s = case f s of
          Fail msg  -> errorHandler msg (chunkedStreamToMaybe consumed)
          Partial p -> C.NeedInput (push p consumed) (close p consumed)
          Done r s' -> C.Done (streamToMaybe s') (resultMapper r)
          where consumed = s : b
        close f = terminationHandler (fmap resultMapper . f) . chunkedStreamToMaybe

chunkedStreamToMaybe :: [BS.ByteString] -> Maybe BS.ByteString
chunkedStreamToMaybe = streamToMaybe . BS.concat . reverse

streamToMaybe :: BS.ByteString -> Maybe BS.ByteString
streamToMaybe s = if BS.null s
                    then Nothing
                    else Just s