{-# LANGUAGE RankNTypes #-}
module Data.Conduit.Filesystem
    ( sourceDirectory
    , sourceDirectoryDeep
    ) where

import Data.Conduit
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))
import qualified Data.Streaming.Filesystem as F

-- | Stream the contents of the given directory, without traversing deeply.
--
-- This function will return /all/ of the contents of the directory, whether
-- they be files, directories, etc.
--
-- Note that the generated filepaths will be the complete path, not just the
-- filename. In other words, if you have a directory @foo@ containing files
-- @bar@ and @baz@, and you use @sourceDirectory@ on @foo@, the results will be
-- @foo/bar@ and @foo/baz@.
--
-- Since 1.1.0
sourceDirectory :: MonadResource m => FilePath -> Producer m FilePath
sourceDirectory dir =
    bracketP (F.openDirStream dir) F.closeDirStream go
  where
    go ds =
        loop
      where
        loop = do
            mfp <- liftIO $ F.readDirStream ds
            case mfp of
                Nothing -> return ()
                Just fp -> do
                    yield $ dir </> fp
                    loop

-- | Deeply stream the contents of the given directory.
--
-- This works the same as @sourceDirectory@, but will not return directories at
-- all. This function also takes an extra parameter to indicate whether
-- symlinks will be followed.
--
-- Since 1.1.0
sourceDirectoryDeep :: MonadResource m
                    => Bool -- ^ Follow directory symlinks
                    -> FilePath -- ^ Root directory
                    -> Producer m FilePath
sourceDirectoryDeep followSymlinks =
    start
  where
    start :: MonadResource m => FilePath -> Producer m FilePath
    start dir = sourceDirectory dir =$= awaitForever go

    go :: MonadResource m => FilePath -> Producer m FilePath
    go fp = do
        ft <- liftIO $ F.getFileType fp
        case ft of
            F.FTFile -> yield fp
            F.FTFileSym -> yield fp
            F.FTDirectory -> start fp
            F.FTDirectorySym
                | followSymlinks -> start fp
                | otherwise -> return ()
            F.FTOther -> return ()
