{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Conduit.LogFile
    ( RotatingLog
    , openRotatingLog
    , addChunk
    , close
    , defaultMaxTotal
    ) where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception              (bracket, bracketOnError,
                                                 finally)
import           Control.Monad                  (void, when)
import qualified Data.ByteString                as S
import           Data.Time                      (UTCTime, getCurrentTime)
import           Data.Word                      (Word)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesFileExist, renameFile)
import           System.FilePath                ((<.>), (</>))
import qualified System.IO                      as SIO
import           System.Mem.Weak                (addFinalizer)

data Command = AddChunk !S.ByteString
             | Close

-- | Represents a folder used for totating log files.
--
-- Since 0.2.1
data RotatingLog = RotatingLog !(TVar State)
-- Use a data instead of a newtype so that we can attach a finalizer.

data State = Closed
           | Running !SIO.Handle !(TBQueue Command)

queue :: Command -> RotatingLog -> IO ()
queue cmd (RotatingLog ts) = atomically $ do
    s <- readTVar ts
    case s of
        Closed -> return ()
        Running _ q -> writeTBQueue q cmd

addChunk :: RotatingLog -> S.ByteString -> IO ()
addChunk lf bs = queue (AddChunk bs) lf

close :: RotatingLog -> IO ()
close = queue Close

-- | Create a new @RotatingLog@.
--
-- Since 0.2.1
openRotatingLog :: FilePath -- ^ folder to contain logs
                -> Word -- ^ maximum log file size, in bytes
                -> IO RotatingLog
openRotatingLog dir maxTotal = do
    createDirectoryIfMissing True dir
    bracketOnError (moveCurrent dir) SIO.hClose $ \handle -> do
        queue <- newTBQueueIO 5
        let s = Running handle queue
        ts <- newTVarIO s
        void $ forkIO $ loop dir ts maxTotal
        let rl = RotatingLog ts
        addFinalizer rl (atomically (writeTBQueue queue Close))
        return rl

current :: FilePath -- ^ folder containing logs
        -> FilePath
current = (</> "current.log")

moveCurrent :: FilePath -- ^ folder containing logs
            -> IO SIO.Handle -- ^ new handle
moveCurrent dir = do
    let curr = current dir
    x <- doesFileExist curr
    when x $ do
        now <- getCurrentTime
        renameFile curr $ dir </> suffix now
    SIO.openFile curr SIO.WriteMode

suffix :: UTCTime -> FilePath
suffix now =
    (concatMap fix $ takeWhile (/= '.') $ show now) <.> "log"
  where
    fix ' ' = "_"
    fix c | '0' <= c && c <= '9' = [c]
    fix _ = ""

loop :: FilePath -- ^ folder containing logs
     -> TVar State
     -> Word -- ^ maximum total log size
     -> IO ()
loop dir ts maxTotal =
    go 0 `finally` (closeCurrentHandle `finally` atomically (writeTVar ts Closed))
  where
    closeCurrentHandle = bracket
        (atomically $ do
            s <- readTVar ts
            case s of
                Closed -> return Nothing
                Running h _ -> return $! Just h)
        (maybe (return ()) SIO.hClose)
        (const $ return ())

    go total = do
        res <- atomically $ do
            s <- readTVar ts
            case s of
                Closed -> return Nothing
                Running handle queue -> do
                    cmd <- readTBQueue queue
                    case cmd of
                        Close -> return Nothing
                        AddChunk bs -> return $! Just (handle, queue, bs)
        case res of
            Nothing -> return ()
            Just (handle, queue, bs) -> do
                let total' = total + fromIntegral (S.length bs)
                S.hPut handle bs
                SIO.hFlush handle
                if total' > maxTotal
                    then do
                        bracket
                            (SIO.hClose handle >> moveCurrent dir)
                            (\handle' -> atomically $ writeTVar ts $ Running handle' queue)
                            (const $ return ())
                        go 0
                    else go total'

defaultMaxTotal :: Word
defaultMaxTotal = 5 * 1024 * 1024 -- 5 MB
