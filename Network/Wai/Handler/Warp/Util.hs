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
import System.PosixCompat.Files (getFileStatus, fileSize)
import System.Posix.Types (FileOffset)

getFileSize :: FilePath -> IO FileOffset
getFileSize = fmap fileSize . getFileStatus

mimeTypes :: Map.Map ByteString ByteString
mimeTypes =
    Map.fromAscList $ map go $(qRunIO mimeTypes' >>= lift)
  where
    go (x, y) = (S8.pack x, S8.pack y)
