{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Win32File
    ( openRead
    , read
    , close
    ) where

import Foreign.C.String (CString)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt (..))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.Ptr (Ptr)
import Data.Bits (Bits, (.|.))
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Internal as BI
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf16LE)
import Data.Word (Word8)
import Prelude hiding (read)
import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)


#include <fcntl.h>
#include <Share.h>
#include <SYS/Stat.h>
#include <errno.h>

newtype OFlag = OFlag CInt
    deriving (Num, Bits, Show, Eq)

#{enum OFlag, OFlag
    , oBinary = _O_BINARY
    , oRdonly = _O_RDONLY
    , oWronly = _O_WRONLY
    , oCreat  = _O_CREAT
    }

newtype SHFlag = SHFlag CInt
    deriving (Num, Bits, Show, Eq)

#{enum SHFlag, SHFlag
    , shDenyno = _SH_DENYNO
    }

newtype PMode = PMode CInt
    deriving (Num, Bits, Show, Eq)

#{enum PMode, PMode
    , pIread = _S_IREAD
    , pIwrite = _S_IWRITE
    }

foreign import ccall "_wsopen"
    c_wsopen :: CString -> OFlag -> SHFlag -> PMode -> IO CInt

foreign import ccall "_read"
    c_read :: FD -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "_write"
    c_write :: FD -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "_close"
    close :: FD -> IO ()

newtype FD = FD CInt

openRead :: FilePath -> IO FD
openRead fp = do
    -- need to append a null char
    -- note that useAsCString is not sufficient, as we need to have two
    -- null octets to account for UTF16 encoding
    let bs = encodeUtf16LE $ pack $ fp ++ "\0"
    h <- BU.unsafeUseAsCString bs $ \str ->
            throwErrnoIfMinus1Retry "System.Win32File.openRead" $
            c_wsopen
                str
                (oBinary .|. oRdonly)
                shDenyno
                pIread
    return $ FD h

read :: FD -> IO (Maybe S.ByteString)
read fd = do
    fp <- mallocPlainForeignPtrBytes 4096
    withForeignPtr fp $ \p -> do
        len <- throwErrnoIfMinus1Retry "System.Win32File.read" $ c_read fd p 4096
        if len == 0
            then return $! Nothing
            else return $! Just $! BI.PS fp 0 (fromIntegral len)
