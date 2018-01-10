{-# LANGUAGE CPP #-}
-- | The standard @openFile@ call on Windows causing problematic file locking
-- in some cases. This module provides a cross-platform file reading API
-- without the file locking problems on Windows.
--
-- This module /always/ opens files in binary mode.
--
-- @readChunk@ will return an empty @ByteString@ on EOF.
module Data.Streaming.FileRead
    ( ReadHandle
    , openFile
    , closeFile
    , readChunk
    ) where

#if WINDOWS

import System.Win32File

#else

import qualified System.IO as IO
import qualified Data.ByteString as S
import Data.ByteString.Lazy.Internal (defaultChunkSize)

newtype ReadHandle = ReadHandle IO.Handle

openFile :: FilePath -> IO ReadHandle
openFile fp = ReadHandle `fmap` IO.openBinaryFile fp IO.ReadMode

closeFile :: ReadHandle -> IO ()
closeFile (ReadHandle h) = IO.hClose h

readChunk :: ReadHandle -> IO S.ByteString
readChunk (ReadHandle h) = S.hGetSome h defaultChunkSize

#endif
