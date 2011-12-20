{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.Binary
    ( sourceFile
    , sinkFile
    , conduitFile
    , isolate
    ) where

import Prelude hiding (FilePath)
import System.IO (hClose)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Filesystem (openFile, IOMode (ReadMode, WriteMode))
import Data.Conduit
import Data.Int (Int64)
import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)

sourceFile :: ResourceIO m
           => FilePath
           -> SourceM m S.ByteString
sourceFile fp = sourceMIO
    (openFile fp ReadMode)
    hClose
    (\handle -> do
        bs <- liftIO $ S.hGetSome handle 4096
        if S.null bs
            then return $ SourceResult StreamClosed []
            else return $ SourceResult StreamOpen [bs])

sinkFile :: ResourceIO m
         => FilePath
         -> SinkM S.ByteString m ()
sinkFile fp = sinkMIO
    (openFile fp WriteMode)
    hClose
    (\handle bss -> liftIO (L.hPut handle $ L.fromChunks bss) >> return Processing)
    (\handle bss -> do
        liftIO $ L.hPut handle $ L.fromChunks bss
        return $ SinkResult [] ())

-- | Stream the contents of the input to a file, and also send it along the
-- pipeline. Similar in concept to the Unix command @tee@.
conduitFile :: ResourceIO m
            => FilePath
            -> ConduitM S.ByteString m S.ByteString
conduitFile fp = conduitMIO
    (openFile fp WriteMode)
    hClose
    (\handle bss -> do
        liftIO $ L.hPut handle $ L.fromChunks bss
        return $ ConduitResult Processing bss)
    (\handle bss -> do
        liftIO $ L.hPut handle $ L.fromChunks bss
        return $ ConduitResult [] bss)

isolate :: Resource m
        => Int64
        -> ConduitM S.ByteString m S.ByteString
isolate count0 = conduitMState
    count0
    push
    close
  where
    push 0 bss = return (0, ConduitResult (Done bss) [])
    push count bss = do
        let (a, b) = L.splitAt count $ L.fromChunks bss
        let count' = count - L.length a
        return (count',
            if count' == 0
                then ConduitResult (Done $ L.toChunks b) (L.toChunks a)
                else assert (L.null b) $ ConduitResult Processing (L.toChunks a))
    close count bss = do
        let (a, b) = L.splitAt count $ L.fromChunks bss
        return $ ConduitResult (L.toChunks b) (L.toChunks a)
