{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Data.Conduit.Process.Unix
    ( forkExecuteFile
    , killProcess
    , waitForProcess
    , ProcessStatus (..)
    ) where

import           Control.Concurrent                (forkIO)
import           Control.Exception                 (finally, mask, onException)
import           Control.Monad                     (unless, void, zipWithM_, when)
import           Control.Monad.Trans.Class         (lift)
import           Data.ByteString                   (ByteString, null, concat, append, singleton)
import           Data.ByteString.Unsafe            (unsafePackCStringFinalizer,
                                                    unsafeUseAsCStringLen,
                                                    unsafeUseAsCString)
import           Data.Conduit                      (Sink, Source, yield, ($$))
import           Data.Conduit.List                 (mapM_)
import           Foreign.Marshal.Alloc             (free, mallocBytes, allocaBytes)
import           Foreign.Ptr                       (castPtr, Ptr, nullPtr)
import           Foreign.Storable                  (sizeOf, pokeElemOff)
import           Prelude                           (Bool (..), IO, Maybe (..),
                                                    Monad (..), flip,
                                                    fromIntegral, fst, maybe,
                                                    snd, ($), (.), (==), error, id, length, (*),
                                                    head, map)
import           System.Posix.Types                (Fd)
import           System.Posix.IO.ByteString        (closeFd, createPipe,
                                                    fdReadBuf, fdWriteBuf,
                                                    setFdOption, FdOption (CloseOnExec))
import           System.Posix.Process.ByteString   (ProcessStatus (..),
                                                    getProcessStatus)
import           System.Posix.Signals              (sigKILL, signalProcess)
import           System.Posix.Types                (ProcessID)
import Foreign.C.Types
import Foreign.C.String

-- | Kill a process by sending it the KILL (9) signal.
--
-- Since 0.1.0
killProcess :: ProcessID -> IO ()
killProcess = signalProcess sigKILL

foreign import ccall "forkExecuteFile"
    c_forkExecuteFile :: Ptr CString
                      -> CInt
                      -> CString
                      -> Ptr CString
                      -> CInt
                      -> CInt
                      -> CInt
                      -> IO CInt

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
                -> Bool -- ^ search on PATH?
                -> [ByteString] -- ^ args
                -> Maybe [(ByteString, ByteString)] -- ^ environment
                -> Maybe ByteString -- ^ working directory
                -> Maybe (Source IO ByteString) -- ^ stdin
                -> Maybe (Sink ByteString IO ()) -- ^ stdout
                -> Maybe (Sink ByteString IO ()) -- ^ stderr
                -> IO ProcessID
forkExecuteFile cmd path args menv mwdir mstdin mstdout mstderr = do
    min  <- withIn  mstdin
    mout <- withOut mstdout
    merr <- withOut mstderr

    maybe (return ()) (closeOnExec . snd) min
    maybe (return ()) (closeOnExec . fst) mout
    maybe (return ()) (closeOnExec . fst) merr

    pid <- withArgs (cmd : args) $ \args' ->
           withMString mwdir $ \mwdir' ->
           withEnv menv $ \menv' ->
           c_forkExecuteFile
                args'
                (if path then 1 else 0)
                mwdir'
                menv'
                (maybe (-1) (fromIntegral . fst) min)
                (maybe (-1) (fromIntegral . snd) mout)
                (maybe (-1) (fromIntegral . snd) merr)
    when (pid == -1) $ error "Failure with forkExecuteFile"
    maybe (return ()) (closeFd . fst) min
    maybe (return ()) (closeFd . snd) mout
    maybe (return ()) (closeFd . snd) merr
    return $ fromIntegral pid
  where
    withIn Nothing = return Nothing
    withIn (Just src) = do
        (fdRead, fdWrite) <- createPipe
        let sink = mapM_ $ flip unsafeUseAsCStringLen $ \(ptr, size) -> void $ fdWriteBuf fdWrite (castPtr ptr) (fromIntegral size)
        void $ forkIO $ (src $$ sink) `finally` closeFd fdWrite
        return $ Just (fdRead, fdWrite)
    withOut Nothing = return Nothing
    withOut (Just sink) = do
        (fdRead, fdWrite) <- createPipe
        let buffSize = 4096
        let src = do
                bs <- lift $ mask $ \restore -> do
                    ptr <- mallocBytes buffSize
                    bytesRead <- restore (fdReadBuf fdRead ptr $ fromIntegral buffSize) `onException` free ptr
                    unsafePackCStringFinalizer ptr (fromIntegral bytesRead) (free ptr)
                unless (null bs) $ do
                    yield bs
                    src
        void $ forkIO $ (src $$ sink) `finally` closeFd fdRead
        return $ Just (fdRead, fdWrite)

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

-- | Wait until the given process has died, and return its @ProcessStatus@.
--
-- Since 0.1.0
waitForProcess :: ProcessID -> IO ProcessStatus
waitForProcess pid =
    loop
  where
    loop = getProcessStatus True False pid >>= maybe loop return
