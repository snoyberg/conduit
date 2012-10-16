{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, ScopedTypeVariables #-}
module Data.Conduit.Process.Unix
    ( forkExecuteFile
    , killProcess
    , terminateProcess
    , waitForProcess
    , ProcessStatus (..)
    , signalProcessHandle
    , signalProcessHandleGroup
    ) where

import           Control.Applicative               ((<$>), (<*>))
import           Control.Arrow                     ((***))
import           Control.Concurrent                (forkIO)
import           Control.Exception                 (finally, mask, onException, handle, SomeException)
import           Control.Monad                     (unless, void, zipWithM_, when)
import           Control.Monad.Trans.Class         (lift)
import           Data.ByteString                   (ByteString, null, concat, append, singleton)
import qualified Data.ByteString.Char8             as S8
import           Data.ByteString.Unsafe            (unsafePackCStringFinalizer,
                                                    unsafeUseAsCStringLen,
                                                    unsafeUseAsCString)
import           Data.Conduit                      (Sink, Source, yield, ($$))
import           Data.Conduit.Binary               (sinkHandle, sourceHandle)
import           Data.Conduit.List                 (mapM_)
import           Foreign.Marshal.Alloc             (free, mallocBytes, allocaBytes)
import           Foreign.Ptr                       (castPtr, Ptr, nullPtr)
import           Foreign.Storable                  (sizeOf, pokeElemOff)
import           Prelude                           (Bool (..), IO, Maybe (..),
                                                    Monad (..), flip,
                                                    fromIntegral, fst, maybe,
                                                    snd, ($), (.), (==), error, id, length, (*),
                                                    head, map, const, fmap)
import           System.Posix.Types                (Fd)
import           System.Posix.IO.ByteString        (closeFd, createPipe,
                                                    fdReadBuf, fdWriteBuf,
                                                    setFdOption, FdOption (CloseOnExec))
import           System.Posix.Process.ByteString   (ProcessStatus (..),
                                                    getProcessStatus, getProcessGroupIDOf)
import           System.Posix.Signals              (sigKILL, signalProcess, Signal, signalProcessGroup)
import           System.Posix.Types                (ProcessID)
import System.IO (hClose)
import Foreign.C.Types
import Foreign.C.String
import System.Process
import System.Process.Internals

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
