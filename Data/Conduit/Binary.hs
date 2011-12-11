{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.Binary
    ( sourceFile
    , sinkFile
    ) where

import Prelude hiding (FilePath)
import System.IO (hClose)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Filesystem (openFile, IOMode (ReadMode, WriteMode))
import Data.Conduit
import Control.Monad.Trans.Resource (with, release)

sourceFile :: MonadBaseControl IO m
           => FilePath
           -> SourceM m S.ByteString
sourceFile fp = sourceM
    (with (liftBase $ openFile fp ReadMode) (liftBase . hClose))
    (\(key, _) -> release key)
    (\(_, handle) -> do
        bs <- liftBase $ S.hGetSome handle 4096
        if S.null bs
            then return EOF
            else return $ Chunks [bs])

sinkFile :: MonadBaseControl IO m
         => FilePath
         -> SinkM S.ByteString m ()
sinkFile fp = sinkM
    (with (liftBase $ openFile fp WriteMode) (liftBase . hClose))
    (\(key, _) -> release key)
    (\(_, handle) bss -> liftBase (L.hPut handle $ L.fromChunks bss) >> return (SinkResult [] Nothing))
    (const $ return $ SinkResult [] ())
