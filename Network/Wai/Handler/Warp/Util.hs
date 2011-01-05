{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Wai.Handler.Warp.Util
    ( getFileSize
    , mimeTypes
    ) where

import UtilHelper (mimeTypes')
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Language.Haskell.TH.Syntax (qRunIO, lift)
import qualified Data.ByteString.Char8 as S8

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

mimeTypes :: Map.Map ByteString ByteString
mimeTypes =
    Map.fromAscList $ map go $(qRunIO mimeTypes' >>= lift)
  where
    go (x, y) = (S8.pack x, S8.pack y)
