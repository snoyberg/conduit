{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.Binary
    ( sourceFile
    , sourceFileRange
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
import qualified System.IO as IO
import Control.Monad.Trans.Resource (withIO, release, newRef, readRef, writeRef)

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

sourceFileRange :: ResourceIO m
                => FilePath
                -> Maybe Integer -- ^ Offset
                -> Maybe Integer -- ^ Maximum count
                -> SourceM m S.ByteString
sourceFileRange fp offset count = SourceM $ do
    (key, handle) <- withIO (openFile fp ReadMode) hClose
    case offset of
        Nothing -> return ()
        Just off -> liftIO $ IO.hSeek handle IO.AbsoluteSeek off
    pull <-
        case count of
            Nothing -> return $ pullUnlimited handle key
            Just c -> do
                ic <- newRef c
                return $ pullLimited ic handle key
    return Source
        { sourcePull = pull
        , sourceClose = release key
        }
  where
    pullUnlimited handle key = do
        bs <- liftIO $ S.hGetSome handle 4096
        if S.null bs
            then do
                release key
                return $ SourceResult StreamClosed []
            else return $ SourceResult StreamOpen [bs]
    pullLimited ic handle key = do
        c <- fmap fromInteger $ readRef ic
        bs <- liftIO $ S.hGetSome handle (min c 4096)
        let c' = c - S.length bs
        assert (c' >= 0) $
            if S.null bs || c' == 0
                then do
                    release key
                    return $ SourceResult StreamClosed
                        (if S.null bs then [] else [bs])
                else do
                    writeRef ic $ toInteger c'
                    return $ SourceResult StreamOpen [bs]

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
