{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

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

import           Control.Monad.IO.Class (MonadIO)
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
         -> MonadSource m FilePath
traverse _followSymlinks root =
    liftStreamIO (listDirectory root) >>= pull
  where
    pull [] = return ()
    pull (p:ps) = do
        isFile' <- liftStreamIO $ isFile p
        if isFile'
            then yield p >> pull ps
            else do
                follow' <- liftStreamIO $ follow p
                if follow'
                    then do
                        ps' <- liftStreamIO $ listDirectory p
                        pull ps
                        pull ps'
                    else pull ps

    follow :: FilePath -> IO Bool
#ifdef CABAL_OS_WINDOWS
    follow = isDirectory
#else
    follow p = do
        let path = encodeString p
        stat <- if _followSymlinks
            then Posix.getFileStatus path
            else Posix.getSymbolicLinkStatus path
        return (Posix.isDirectory stat)
#endif

-- | Same as 'CB.sourceFile', but uses system-filepath\'s @FilePath@ type.
sourceFile :: MonadResource m
           => FilePath
           -> MonadSource m S.ByteString
sourceFile = CB.sourceFile . encodeString

-- | Same as 'CB.sinkFile', but uses system-filepath\'s @FilePath@ type.
sinkFile :: MonadResource m
         => FilePath
         -> MonadSink S.ByteString m ()
sinkFile = CB.sinkFile . encodeString
