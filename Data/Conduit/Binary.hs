{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.Binary
    ( sourceFile
    , sinkFile
    ) where

import Prelude hiding (FilePath)
import System.IO (hClose)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Data.ByteString as S
import Filesystem (openFile, IOMode (ReadMode, WriteMode))
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Resource (with, release)

sourceFile :: MonadBaseControl IO m
           => FilePath
           -> SourceM m S.ByteString
sourceFile fp = sourceM
    (with (liftBase $ openFile fp ReadMode) (liftBase . hClose))
    (\(key, _) -> release key)
    (\(_, handle) -> do
        bs <- liftBase $ S.hGetSome handle 50
        if S.null bs
            then return EOF
            else return $ Chunks [bs])

sinkFile :: MonadBaseControl IO m
         => FilePath
         -> Sink S.ByteString m ()
sinkFile fp = Sink $ do
    (key, handle) <- with (liftBase $ openFile fp WriteMode) (liftBase . hClose)
    return $ SinkData $ \stream -> do
        case stream of
            EOF -> release key >> return (SinkResult [] (Just ()))
            Chunks bss -> mapM_ (liftBase . S.hPut handle) bss >> return (SinkResult [] Nothing)
