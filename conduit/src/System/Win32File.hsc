{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Win32File
    ( openFile
    , readChunk
    , closeFile
    , ReadHandle
    ) where

import Foreign.C.String (CString)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.C.Types (CInt (..))
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
import Data.ByteString.Lazy.Internal (defaultChunkSize)


#include <fcntl.h>
#include <share.h>
#include <sys/stat.h>
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
    c_read :: ReadHandle -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "_write"
    c_write :: ReadHandle -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "_close"
    closeFile :: ReadHandle -> IO ()

newtype ReadHandle = ReadHandle CInt

openFile :: FilePath -> IO ReadHandle
openFile fp = do
    -- need to append a null char
    -- note that useAsCString is not sufficient, as we need to have two
    -- null octets to account for UTF16 encoding
    let bs = encodeUtf16LE $ pack $ fp ++ "\0"
    h <- BU.unsafeUseAsCString bs $ \str ->
            throwErrnoIfMinus1Retry "Data.Streaming.FileRead.openFile" $
            c_wsopen
                str
                (oBinary .|. oRdonly)
                shDenyno
                pIread
    return $ ReadHandle h

readChunk :: ReadHandle -> IO S.ByteString
readChunk fd = do
    fp <- mallocPlainForeignPtrBytes defaultChunkSize
    withForeignPtr fp $ \p -> do
        len <- throwErrnoIfMinus1Retry "System.Win32File.read" $ c_read fd p
            (fromIntegral defaultChunkSize)
        if len == 0
            then return $! S.empty
            else return $! BI.PS fp 0 (fromIntegral len)
