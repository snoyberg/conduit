{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Win32File
    ( openRead
    , read
    , close
    ) where

import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (mallocBytes, free)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt (..))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.C.Error (throwErrno)
import Foreign.Ptr (Ptr)
import Data.Bits (Bits, (.|.))
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as BU
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf16LE)
import Data.Word (Word8)
import Prelude hiding (read)

#include <fcntl.h>
#include <Share.h>
#include <SYS/Stat.h>
#include <errno.h>

newtype OFlag = OFlag CInt
    deriving (Num, Bits, Show, Eq)

#{enum OFlag, OFlag
    , oBinary = _O_BINARY
    , oRdonly = _O_RDONLY
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
    }

foreign import ccall "_wsopen"
    c_wsopen :: CString -> OFlag -> SHFlag -> PMode -> IO CInt

foreign import ccall "_read"
    c_read :: FD -> Ptr Word8 -> CInt -> IO CInt

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
            c_wsopen
                str
                (oBinary .|. oRdonly)
                shDenyno
                pIread
    if h < 0
        then throwErrno $ "Could not open file: " ++ fp
        else return $ FD h

read :: FD -> IO (Maybe S.ByteString)
read fd = do
    cstr <- mallocBytes 4096
    len <- c_read fd cstr 4096
    if len == 0
        then do
            free cstr
            return Nothing
        else do
            fmap Just $ BU.unsafePackCStringFinalizer
                cstr
                (fromIntegral len)
                (free cstr)
