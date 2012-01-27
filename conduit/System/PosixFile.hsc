{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.PosixFile
    ( openRead
    , read
    , close
    ) where

import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.C.Types (CInt)
import Foreign.C.Error (throwErrno)
import Foreign.Ptr (Ptr)
import Data.Bits (Bits)
import Data.Word (Word8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as BU
import Prelude hiding (read)
import Data.Conduit.Util.Source (SourceIOResult (..))

#include <fcntl.h>

newtype Flag = Flag CInt
    deriving (Num, Bits, Show, Eq)

#{enum Flag, Flag
    , oRdonly = O_RDONLY
    }

foreign import ccall "open"
    c_open :: CString -> Flag -> IO CInt

foreign import ccall "read"
    c_read :: FD -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "close"
    close :: FD -> IO ()

newtype FD = FD CInt

openRead :: FilePath -> IO FD
openRead fp = do
    h <- withCString fp $ \str -> c_open str oRdonly
    if h < 0
        then throwErrno $ "Could not open file: " ++ fp
        else return $ FD h

read :: FD -> IO (SourceIOResult S.ByteString)
read fd = do
    cstr <- mallocBytes 4096
    len <- c_read fd cstr 4096
    if len == 0
        then free cstr >> return IOClosed
        else fmap IOOpen $ BU.unsafePackCStringFinalizer
                cstr
                (fromIntegral len)
                (free cstr)
