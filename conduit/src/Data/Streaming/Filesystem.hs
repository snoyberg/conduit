{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Streaming functions for interacting with the filesystem.
module Data.Streaming.Filesystem
    ( DirStream
    , openDirStream
    , readDirStream
    , closeDirStream
    , FileType (..)
    , getFileType
    ) where

import Data.Typeable (Typeable)

#if WINDOWS

import qualified System.Win32 as Win32
import System.FilePath ((</>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory (doesFileExist, doesDirectoryExist)

data DirStream = DirStream !Win32.HANDLE !Win32.FindData !(IORef Bool)
    deriving Typeable

openDirStream :: FilePath -> IO DirStream
openDirStream fp = do
    (h, fdat) <- Win32.findFirstFile $ fp </> "*"
    imore <- newIORef True -- always at least two records, "." and ".."
    return $! DirStream h fdat imore

closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream h _ _) = Win32.findClose h

readDirStream :: DirStream -> IO (Maybe FilePath)
readDirStream ds@(DirStream h fdat imore) = do
    more <- readIORef imore
    if more
        then do
            filename <- Win32.getFindDataFileName fdat
            Win32.findNextFile h fdat >>= writeIORef imore
            if filename == "." || filename == ".."
                then readDirStream ds
                else return $ Just filename
        else return Nothing

isSymlink :: FilePath -> IO Bool
isSymlink _ = return False

getFileType :: FilePath -> IO FileType
getFileType fp = do
    isFile <- doesFileExist fp
    if isFile
        then return FTFile
        else do
            isDir <- doesDirectoryExist fp
            return $ if isDir then FTDirectory else FTOther

#else

import System.Posix.Directory (DirStream, openDirStream, closeDirStream)
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as PosixF
import Control.Exception (try, IOException)

readDirStream :: DirStream -> IO (Maybe FilePath)
readDirStream ds = do
    fp <- Posix.readDirStream ds
    case fp of
        "" -> return Nothing
        "." -> readDirStream ds
        ".." -> readDirStream ds
        _ -> return $ Just fp

getFileType :: FilePath -> IO FileType
getFileType fp = do
    s <- PosixF.getSymbolicLinkStatus fp
    case () of
        ()
            | PosixF.isRegularFile s -> return FTFile
            | PosixF.isDirectory s -> return FTDirectory
            | PosixF.isSymbolicLink s -> do
                es' <- try $ PosixF.getFileStatus fp
                case es' of
                    Left (_ :: IOException) -> return FTOther
                    Right s'
                        | PosixF.isRegularFile s' -> return FTFileSym
                        | PosixF.isDirectory s' -> return FTDirectorySym
                        | otherwise -> return FTOther
            | otherwise -> return FTOther

#endif

data FileType
    = FTFile
    | FTFileSym -- ^ symlink to file
    | FTDirectory
    | FTDirectorySym -- ^ symlink to a directory
    | FTOther
    deriving (Show, Read, Eq, Ord, Typeable)
