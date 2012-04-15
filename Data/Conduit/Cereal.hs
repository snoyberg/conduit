--{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Turn a 'Get' into a 'Sink' and a 'Put' into a 'Source'
--
-- If you want to run a get on each item of input (disregarding overlap),
-- use Data.Conduit.mapOutput with Data.Serialize.runGet*

module Data.Conduit.Cereal (GetError, sinkGet, conduitGet, sourcePut) where

--import           Control.Exception
import           Control.Monad.Error
import           Control.Monad.Error.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import           Data.Conduit.List (sourceList)
import           Data.Serialize hiding (get, put)
--import           Data.Typeable

{-
data GetException = GetException String
     deriving (Show, Typeable)

instance Exception GetException
-}

data GetError = GetError String
  deriving (Show, Eq)

instance Error GetError where
  noMsg = GetError ""
  strMsg = GetError

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

-- | Run a 'Get' repeatedly on the input stream, producing an output stream of whatever the 'Get' outputs.
conduitGet :: MonadError GetError m => Get output -> C.Conduit BS.ByteString m output
conduitGet get = consume True (runGetPartial get) [] BS.empty
  where push f b input
          | BS.null input = C.NeedInput (push f b) (C.Done (streamToMaybe $ BS.concat $ reverse b) ())
          | otherwise = consume False f b input
        consume initial f b s = case f s of
          Fail msg -> C.PipeM (throwError $ strMsg msg) undefined
          Partial f' -> C.NeedInput (push f' consumed) $ C.Done (streamToMaybe $ BS.concat $ reverse consumed) ()
          Done r s' -> case initial of
                         True -> infiniteSequence r
                         False -> C.HaveOutput (push (runGetPartial get) [] s') (return ()) r
          where consumed = s : b
                infiniteSequence r = C.HaveOutput (infiniteSequence r) (return ()) r
                -- infinteSequence only works because the Get will either _always_ consume no input, or _never_ consume no input.

streamToMaybe :: BS.ByteString -> Maybe BS.ByteString
streamToMaybe s = if BS.null s
                    then Nothing
                    else Just s

-- | Convert a 'Put' into a 'Source'. Runs in constant memory.
sourcePut :: Monad m => Put -> C.Source m BS.ByteString
sourcePut put = sourceList $ LBS.toChunks $ runPutLazy put
