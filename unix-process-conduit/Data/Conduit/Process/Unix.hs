{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Data.Conduit.Process.Unix
    ( -- * Starting processes
      forkExecuteFile
    , killProcess
    , terminateProcess
    , waitForProcess
    , ProcessStatus (..)
    , signalProcessHandle
    , signalProcessHandleGroup
      -- * Process tracking
      -- $processTracker

      -- ** Types
    , ProcessTracker
    , TrackedProcess
    , ProcessTrackerException (..)
      -- ** Functions
    , initProcessTracker
    , trackProcess
    , untrackProcess
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Arrow                   ((***))
import           Control.Concurrent              (forkIO)
import           Control.Concurrent.MVar         (readMVar)
import           Control.Exception               (Exception, SomeException,
                                                  finally, handle, mask, mask_,
                                                  onException, throwIO)
import           Control.Monad                   (unless, void, when, zipWithM_)
import           Control.Monad.Trans.Class       (lift)
import           Data.ByteString                 (ByteString, append, concat,
                                                  null, singleton)
import qualified Data.ByteString.Char8           as S8
import           Data.ByteString.Unsafe          (unsafePackCStringFinalizer,
                                                  unsafeUseAsCString,
                                                  unsafeUseAsCStringLen)
import           Data.Conduit                    (Sink, Source, yield, ($$))
import           Data.Conduit.Binary             (sinkHandle, sourceHandle)
import           Data.Conduit.List               (mapM_)
import           Data.IORef                      (IORef, newIORef, readIORef,
                                                  writeIORef)
import           Data.Typeable                   (Typeable)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc           (allocaBytes, free,
                                                  mallocBytes)
import           Foreign.Ptr                     (Ptr, castPtr, nullPtr)
import           Foreign.Storable                (pokeElemOff, sizeOf)
import           Prelude                         (Bool (..), IO, Maybe (..),
                                                  Monad (..), Show, const,
                                                  error, flip, fmap,
                                                  fromIntegral, fst, head, id,
                                                  length, map, maybe, snd, ($),
                                                  ($!), (*), (.), (==))
import           System.IO                       (hClose)
import           System.Posix.IO.ByteString      (FdOption (CloseOnExec),
                                                  closeFd, createPipe,
                                                  fdReadBuf, fdWriteBuf,
                                                  setFdOption)
import           System.Posix.Process.ByteString (ProcessStatus (..),
                                                  getProcessGroupIDOf,
                                                  getProcessStatus)
import           System.Posix.Signals            (Signal, sigKILL,
                                                  signalProcess,
                                                  signalProcessGroup)
import           System.Posix.Types              (CPid (..))
import           System.Posix.Types              (Fd)
import           System.Posix.Types              (ProcessID)
import           System.Process
import           System.Process.Internals

-- | Kill a process by sending it the KILL (9) signal.
--
-- Since 0.1.0
killProcess :: ProcessHandle -> IO ()
killProcess ph = withProcessHandle_ ph $ \p_ ->
    case p_ of
        ClosedHandle _ -> return p_
        OpenHandle h -> do
            signalProcess sigKILL h
            return p_

signalProcessHandle :: Signal -> ProcessHandle -> IO ()
signalProcessHandle signal ph = withProcessHandle_ ph $ \p_ ->
    case p_ of
        ClosedHandle _ -> return p_
        OpenHandle h -> do
            signalProcess signal h
            return p_

signalProcessHandleGroup :: Signal -> ProcessHandle -> IO ()
signalProcessHandleGroup signal ph = withProcessHandle_ ph $ \p_ ->
    case p_ of
        ClosedHandle _ -> return p_
        OpenHandle h -> do
            pgid <- getProcessGroupIDOf h
            signalProcessGroup signal pgid
            return p_

-- | Fork a new process and execute the given command.
--
-- This is a wrapper around with fork() and exec*() syscalls, set up to work
-- with @conduit@ datatypes for standard input, output, and error. If @Nothing@
-- is provided for any of those arguments, then the original file handles will
-- remain open to the child process.
--
-- If you would like to simply discard data provided by the child process,
-- provide @sinkNull@ for stdout and/or stderr. To provide an empty input
-- stream, use @return ()@.
--
-- Since 0.1.0
forkExecuteFile :: ByteString -- ^ command
                -> [ByteString] -- ^ args
                -> Maybe [(ByteString, ByteString)] -- ^ environment
                -> Maybe ByteString -- ^ working directory
                -> Maybe (Source IO ByteString) -- ^ stdin
                -> Maybe (Sink ByteString IO ()) -- ^ stdout
                -> Maybe (Sink ByteString IO ()) -- ^ stderr
                -> IO ProcessHandle
forkExecuteFile cmd args menv mwdir mstdin mstdout mstderr = do
    (min, mout, merr, ph) <- createProcess cp
    case (,) <$> mstdin <*> min of
        Just (source, h) -> void $ forkIO $ ignoreExceptions $
            (source $$ sinkHandle h) `finally` hClose h
        Nothing -> return ()
    case (,) <$> mstdout <*> mout of
        Just (sink, h) -> void $ forkIO $ ignoreExceptions $
            (sourceHandle h $$ sink) `finally` hClose h
        Nothing -> return ()
    case (,) <$> mstderr <*> merr of
        Just (sink, h) -> void $ forkIO $ ignoreExceptions $
            (sourceHandle h $$ sink) `finally` hClose h
        Nothing -> return ()
    return ph
  where
    ignoreExceptions = handle (\(_ :: SomeException) -> return ())
    cp = CreateProcess
        { cmdspec = RawCommand (S8.unpack cmd) (map S8.unpack args)
        , cwd = S8.unpack <$> mwdir
        , env = map (S8.unpack *** S8.unpack) <$> menv
        , std_in = maybe Inherit (const CreatePipe) mstdin
        , std_out = maybe Inherit (const CreatePipe) mstdout
        , std_err = maybe Inherit (const CreatePipe) mstderr
        , close_fds = True
        , create_group = True
        }

closeOnExec :: Fd -> IO ()
closeOnExec fd = setFdOption fd CloseOnExec True

withMString :: Maybe ByteString -> (CString -> IO a) -> IO a
withMString Nothing f = f nullPtr
withMString (Just bs) f = unsafeUseAsCString (bs `append` singleton 0) f

withEnv :: Maybe [(ByteString, ByteString)] -> (Ptr CString -> IO a) -> IO a
withEnv Nothing f = f nullPtr
withEnv (Just pairs) f =
    withArgs (map toBS pairs) f
  where
    toBS (x, y) = concat [x, "=", y]

withArgs :: [ByteString] -> (Ptr CString -> IO a) -> IO a
withArgs bss0 f =
    loop bss0 id
  where
    loop [] front = run (front [nullPtr])
    loop (bs:bss) front =
        unsafeUseAsCString (bs `append` singleton 0) $ \ptr ->
        loop bss (front . (ptr:))

    run ptrs = allocaBytes (length ptrs * sizeOf (head ptrs)) $ \res ->
        zipWithM_ (pokeElemOff res) [0..] ptrs >> f res

-- $processTracker
--
-- Ensure that child processes are killed, regardless of how the parent process exits.
--
-- The technique used here is:
--
-- * Create a pipe.
--
-- * Fork a new child process that listens on the pipe.
--
-- * In the current process, send updates about processes that should be auto-killed.
--
-- * When the parent process dies, listening on the pipe in the child process will get an EOF.
--
-- * When the child process receives that EOF, it kills all processes it was told to auto-kill.
--
-- This code was originally written for Keter, but was moved to unix-process
-- conduit in the 0.2.1 release.

foreign import ccall unsafe "launch_process_tracker"
    c_launch_process_tracker :: IO CInt

foreign import ccall unsafe "track_process"
    c_track_process :: ProcessTracker -> CPid -> CInt -> IO ()

-- | Represents the child process which handles process cleanup.
--
-- Since 0.2.1
newtype ProcessTracker = ProcessTracker CInt

-- | Represents a child process which is currently being tracked by the cleanup
-- child process.
--
-- Since 0.2.1
data TrackedProcess = TrackedProcess !ProcessTracker !(IORef MaybePid)

data MaybePid = NoPid | Pid !CPid

-- | Fork off the child cleanup process.
--
-- This will ideally only be run once for your entire application.
--
-- Since 0.2.1
initProcessTracker :: IO ProcessTracker
initProcessTracker = do
    i <- c_launch_process_tracker
    if i == -1
        then throwIO CannotLaunchProcessTracker
        else return $! ProcessTracker i

-- | Since 0.2.1
data ProcessTrackerException = CannotLaunchProcessTracker
    deriving (Show, Typeable)
instance Exception ProcessTrackerException

-- | Begin tracking the given process. If the 'ProcessHandle' refers to a
-- closed process, no tracking will occur. If the process is closed, then it
-- will be untracked automatically.
--
-- Note that you /must/ compile your program with @-threaded@; see
-- 'waitForProcess'.
--
-- Since 0.2.1
trackProcess :: ProcessTracker -> ProcessHandle -> IO TrackedProcess
trackProcess pt ph@(ProcessHandle mph) = mask_ $ do
    mpid <- readMVar mph
    mpid' <- case mpid of
        ClosedHandle{} -> return NoPid
        OpenHandle pid -> do
            c_track_process pt pid 1
            return $ Pid pid
    ipid <- newIORef mpid'
    let tp = TrackedProcess pt ipid
    case mpid' of
        NoPid -> return ()
        Pid _ -> void $ forkIO $ do
            void $ waitForProcess ph
            untrackProcess tp
    return $! tp

-- | Explicitly remove the given process from the tracked process list in the
-- cleanup process.
--
-- Since 0.2.1
untrackProcess :: TrackedProcess -> IO ()
untrackProcess (TrackedProcess pt ipid) = mask_ $ do
    mpid <- readIORef ipid
    case mpid of
        NoPid -> return ()
        Pid pid -> do
            c_track_process pt pid 0
            writeIORef ipid NoPid
