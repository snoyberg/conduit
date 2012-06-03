{-# LANGUAGE CPP #-}

-- |
-- Copyright: 2011 John Millikin
--            2011 Michael Snoyman
-- License: MIT
--
-- Maintainer: michael@snoyman.com
-- Portability: portable
--
-- Conduit-based API for manipulating the filesystem.
--
-- Parts of this code were taken from filesystem-enumerator and adapted for
-- conduits.
module Data.Conduit.Filesystem
    ( traverse
    , sourceFile
    , sinkFile
    ) where

import           Prelude hiding (FilePath)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Filesystem
import           Filesystem.Path.CurrentOS (FilePath, encodeString)
import qualified Data.ByteString as S

#ifdef CABAL_OS_WINDOWS
#else
import qualified System.Posix as Posix
#endif

-- | Starting at some root directory, traverse the filesystem and enumerate
-- every file (or symlink to a file) found.
--
-- Note: the option of whether to follow symlinks is currently only checked
-- on POSIX platforms, as the @Win32@ package does not support querying
-- symlink status. On Windows, symlinks will always be followed.
traverse :: MonadIO m
         => Bool -- ^ Follow directory symlinks (only used on POSIX platforms)
         -> FilePath -- ^ Root directory
         -> Source m FilePath
traverse followSymlinks root =
    liftIO (listDirectory root) >>= pull
  where
    pull [] = return ()
    pull (p:ps) = do
        isFile' <- liftIO $ isFile p
        if isFile'
            then yield p >> pull ps
            else do
                follow' <- liftIO $ follow p
                if follow'
                    then do
                        ps' <- liftIO $ listDirectory p
                        pull ps
                        pull ps'
                    else pull ps

    follow :: FilePath -> IO Bool
#ifdef CABAL_OS_WINDOWS
    follow = isDirectory
#else
    follow p = do
        let path = encodeString p
        stat <- if followSymlinks
            then Posix.getFileStatus path
            else Posix.getSymbolicLinkStatus path
        return (Posix.isDirectory stat)
#endif

-- | Same as 'CB.sourceFile', but uses system-filepath\'s @FilePath@ type.
sourceFile :: MonadResource m
           => FilePath
           -> Source m S.ByteString
sourceFile = CB.sourceFile . encodeString

-- | Same as 'CB.sinkFile', but uses system-filepath\'s @FilePath@ type.
sinkFile :: MonadResource m
         => FilePath
         -> Sink S.ByteString m ()
sinkFile = CB.sinkFile . encodeString
