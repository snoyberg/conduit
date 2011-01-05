{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Warp.Util
    ( getFileSize
    ) where


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

import System.Win32.File

getFileSize :: FilePath -> IO Integer
getFileSize path = do
  hnd <- createFile path
           gENERIC_READ fILE_SHARE_READ Nothing oPEN_EXISTING 0 Nothing
  size <- fmap bhfiSize $ getFileInformationByHandle hnd
  closeHandle hnd
  return $ fromIntegral size

#else

import System.Posix.Files

getFileSize :: FilePath -> IO Integer
getFileSize = fmap (fromIntegral . fileSize) . getFileStatus

#endif
