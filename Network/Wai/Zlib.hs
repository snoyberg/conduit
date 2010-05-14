{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Network.Wai.Zlib (compress) where

import Prelude hiding (length)
import Network.Wai hiding (status)

import Data.ByteString.Unsafe
import Data.ByteString.Lazy.Internal (defaultChunkSize)

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Control.Exception
import Control.Monad

import Data.ByteString.Char8 hiding (putStrLn)

chunkSize :: Int
chunkSize = defaultChunkSize

compress :: Enumerator -> Enumerator
compress enum =
    Enumerator $ \iter acc ->
        bracket
            c_create_z_stream
            c_free_z_stream
            (compressInner iter acc enum)

compressInner :: (acc -> ByteString -> IO (Either acc acc))
              -> acc
              -> Enumerator
              -> ZStream
              -> IO (Either acc acc)
compressInner iter acc enum stream = allocaBytes chunkSize $ \outbuff -> do
    when (stream == nullPtr) $ fail "Error from zlib library"
    eacc <- runEnumerator enum (compressIter iter stream outbuff) acc
    case eacc of
        Left acc' -> return $ Left acc'
        Right acc' -> finishStream iter stream outbuff acc'

finishStream :: (acc -> ByteString -> IO (Either acc acc))
             -> ZStream
             -> Ptr CChar
             -> acc
             -> IO (Either acc acc)
finishStream iter stream outbuff acc = do
    c_set_avail_out stream outbuff $ fromIntegral chunkSize
    status <- c_call_deflate_finish stream
    newAvail <- fromIntegral `fmap` c_get_avail_out stream
    bs <- unsafePackCStringLen (outbuff, chunkSize - newAvail)
    eacc <- iter acc bs
    if status == 0 || newAvail == 0
        then case eacc of
                Left acc' -> return $ Left acc'
                Right acc' -> finishStream iter stream outbuff acc'
        else return eacc

compressIter :: (acc -> ByteString -> IO (Either acc acc))
             -> ZStream
             -> Ptr CChar
             -> acc
             -> ByteString
             -> IO (Either acc acc)
compressIter iter stream outbuff accInit bsInput = do
    unsafeUseAsCStringLen bsInput $ \(cstr, len) ->
        c_set_avail_in stream cstr $ fromIntegral len
    go accInit
  where
    go acc = do
        c_set_avail_out stream outbuff $ fromIntegral chunkSize
        c_call_deflate_noflush stream
        newAvail <- fromIntegral `fmap` c_get_avail_out stream
        bs <- unsafePackCStringLen (outbuff, chunkSize - newAvail)
        eacc <- iter acc bs
        if newAvail == 0
            then case eacc of
                    Left acc' -> return $ Left acc'
                    Right acc' -> go acc'
            else return eacc

data ZStreamStruct
type ZStream = Ptr ZStreamStruct

foreign import ccall unsafe "create_z_stream"
    c_create_z_stream :: IO ZStream

foreign import ccall unsafe "free_z_stream"
    c_free_z_stream :: ZStream -> IO ()

foreign import ccall unsafe "set_avail_in"
    c_set_avail_in :: ZStream -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "set_avail_out"
    c_set_avail_out :: ZStream -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "call_deflate_noflush"
    c_call_deflate_noflush :: ZStream -> IO ()

foreign import ccall unsafe "call_deflate_finish"
    c_call_deflate_finish :: ZStream -> IO CInt

foreign import ccall unsafe "get_avail_out"
    c_get_avail_out :: ZStream -> IO CUInt
