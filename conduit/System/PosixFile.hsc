{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.PosixFile
    ( openRead
    , openWrite
    , read
    , write
    , close
    ) where

import Foreign.C.String (CString, withCString)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt (..))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1Retry)
import Foreign.Ptr (Ptr)
import Data.Bits (Bits, (.|.))
import Data.Word (Word8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as BU
import Prelude hiding (read)

#include <fcntl.h>

newtype Flag = Flag CInt
    deriving (Num, Bits, Show, Eq)

#{enum Flag, Flag
    , oRdonly = O_RDONLY
    , oWronly = O_WRONLY
    , oCreat  = O_CREAT
    }

foreign import ccall "open"
    c_open :: CString -> Flag -> CInt -> IO CInt

foreign import ccall "read"
    c_read :: FD -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "write"
    c_write :: FD -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "close"
    close :: FD -> IO ()

newtype FD = FD CInt

openRead :: FilePath -> IO FD
openRead fp = do
    h <- withCString fp $ \str -> c_open str oRdonly 438 -- == octal 666
    if h < 0
        then throwErrno $ "Could not open file: " ++ fp
        else return $ FD h

openWrite :: FilePath -> IO FD
openWrite fp = do
    h <- withCString fp $ \str -> c_open str (oWronly .|. oCreat) 438 -- == octal 666
    if h < 0
        then throwErrno $ "Could not open file: " ++ fp
        else return $ FD h

read :: FD -> IO (Maybe S.ByteString)
read fd = do
    cstr <- mallocBytes 4096
    len <- c_read fd cstr 4096
    if len == 0
        then free cstr >> return Nothing
        else fmap Just $ BU.unsafePackCStringFinalizer
                cstr
                (fromIntegral len)
                (free cstr)

write :: FD -> S.ByteString -> IO ()
write _ bs | S.null bs = return ()
write fd bs = do
    (written, len) <- BU.unsafeUseAsCStringLen bs $ \(cstr, len') -> do
        let len = fromIntegral len'
        written <- throwErrnoIfMinus1Retry "System.PosixFile.write"
                 $ c_write fd (castPtr cstr) len
        return (written, len)
    case () of
        ()
            | written == len -> return ()
            | written <= 0 -> throwErrno $ "Error writing to file"
            | otherwise -> write fd $ BU.unsafeDrop (fromIntegral $ len - written) bs
