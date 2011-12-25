{-# LANGUAGE CPP #-}

-- |
-- Module: Filesystem.Enumerator
-- Copyright: 2011 John Millikin
--            2011 Michael Snoyman
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Enumerator-based API for manipulating the filesystem.
module Data.Conduit.Filesystem
    ( traverse
    , sourceFile
    , sinkFile
    ) where

import           Prelude hiding (FilePath)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Conduit as C
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
traverse :: C.ResourceIO m
         => Bool -- ^ Follow directory symlinks (only used on POSIX platforms)
         -> FilePath -- ^ Root directory
         -> C.Source m FilePath
traverse followSymlinks root = C.Source $ do
    seq0 <- liftIO $ listDirectory root
    C.prepareSource $ C.sourceState seq0 pull
  where
    pull [] = return ([], C.Closed)
    pull (p:ps) = do
        isFile' <- liftIO $ isFile p
        if isFile'
            then return (ps, C.Open p)
            else do
                follow' <- liftIO $ follow p
                if follow'
                    then do
                        ps' <- liftIO $ listDirectory p
                        pull $ ps ++ ps'
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

sourceFile :: C.ResourceIO m
           => FilePath
           -> C.Source m S.ByteString
sourceFile = CB.sourceFile . encodeString

sinkFile :: C.ResourceIO m
         => FilePath
         -> C.Sink S.ByteString m ()
sinkFile = CB.sinkFile . encodeString
