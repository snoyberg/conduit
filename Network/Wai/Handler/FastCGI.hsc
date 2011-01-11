{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Wai.Handler.FastCGI
-- Copyright   :  (c) Bjorn Bringert 2004-2005, (c) Lemmih 2006, (c) Michael Snoyman 2010
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  michael@snoyman.com
-- Stability   :  experimental
-- Portability :  non-portable (uses FFI)
--
-- Interface for FastCGI <http://fastcgi.com/>, using the fcgiapp API.
-- Totally ripped off by Michael Snoyman to work with Hack, then WAI.
--
-----------------------------------------------------------------------------
module Network.Wai.Handler.FastCGI
    ( run
    , runSendfile
    , runFork
    ) where

import Control.Monad    ( liftM, forever )
import Data.Word (Word8)
import Foreign          ( Ptr, castPtr, nullPtr, peekArray0
                        , throwIfNeg_, mallocBytes, free )
import Foreign.C        (CInt, CString, CStringLen)
import Control.Exception (finally)
import Foreign.Storable ( Storable (..) )

import qualified Network.Wai as W
import qualified Network.Wai.Handler.CGI as CGI

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSB
import qualified Data.ByteString.Unsafe   as BSB

import Control.Arrow ((***))

#include <fcgiapp.h>

------------------------------------------------------------------------

data FCGX_Stream
type StreamPtr = Ptr FCGX_Stream
type Environ = Ptr CString

------------------------------------------------------------------------

foreign import ccall unsafe "fcgiapp.h FCGX_GetStr" fcgx_getStr
    :: CString -> CInt -> StreamPtr -> IO CInt

foreign import ccall unsafe "fcgiapp.h FCGX_PutStr" fcgx_putStr
    :: CString -> CInt -> StreamPtr -> IO CInt


------------------------------------------------------------------------

foreign import ccall unsafe "fcgiapp.h FCGX_Init" fcgx_init
    :: IO CInt

runFork :: Maybe S.ByteString -> (IO () -> IO a) -> Int -> W.Application -> IO ()
runFork sf fork threads app = do
    testReturn "FCGX_Init" $ fcgx_init
    let oneThread = forever $ oneRequest app sf
    mapM_ fork $ replicate (threads - 1) oneThread
    oneThread

-- | Handle FastCGI requests in an infinite loop.
run :: W.Application -> IO ()
run = runFork Nothing id 1

-- | Handle FastCGI requests in an infinite loop. For a server which supports
-- the X-Sendfile header.
runSendfile :: S.ByteString -> W.Application -> IO ()
runSendfile sf = runFork (Just sf) id 1

oneRequest :: W.Application
           -> Maybe S.ByteString -- X-Sendfile
           -> IO ()
oneRequest app xsendfile = withRequest $ \r -> do
    putStrLn "Received 1 request"
    env    <- peekEnvp r
    ins    <- peekIn r
    outs   <- peekOut r
    handleRequest app ins outs env xsendfile

peekIn, peekOut :: Ptr FCGX_Request -> IO (Ptr FCGX_Stream)
peekIn  = (#peek FCGX_Request, in)
peekOut = (#peek FCGX_Request, out)

peekEnvp :: Ptr FCGX_Request -> IO Environ
peekEnvp = (#peek FCGX_Request, envp)

foreign import ccall unsafe "fcgiapp.h FCGX_InitRequest" fcgx_initrequest
    :: Ptr FCGX_Request -> CInt -> CInt -> IO CInt

foreign import ccall safe "fcgiapp.h FCGX_Accept_r" fcgx_accept_r
    :: Ptr FCGX_Request -> IO CInt

acceptRequest :: IO (Ptr FCGX_Request)
acceptRequest = do
    reqp <- mallocBytes (#size FCGX_Request)
    initAndAccept reqp
    return reqp
  where initAndAccept reqp = do
          testReturn "FCGX_InitRequest" $ fcgx_initrequest reqp 0 0
          testReturn "FCGX_Accept_r" $ fcgx_accept_r reqp

withRequest :: (Ptr FCGX_Request -> IO ()) -> IO ()
withRequest f = do
    req <- acceptRequest
    f req `finally` finishRequest req

foreign import ccall unsafe "fcgiapp.h FCGX_Finish_r" fcgx_finish_r
    :: Ptr FCGX_Request -> IO ()

finishRequest :: Ptr FCGX_Request -> IO ()
finishRequest reqp = do
                     fcgx_finish_r reqp
                     free reqp

handleRequest :: W.Application
              -> StreamPtr
              -> StreamPtr
              -> Environ
              -> Maybe S.ByteString -- sendfile
              -> IO ()
handleRequest f ins outs env xsendfile = do
    vars <- environToTable env
    let vars' = map (S8.unpack *** S8.unpack) vars
    let input = const $ sRead ins
    let hPut = sPutStr' outs
    CGI.run'' vars' (CGI.requestBodyFunc input) hPut xsendfile f

data FCGX_Request

--
-- * Stream IO
--

sPutStr' :: StreamPtr -> BS.ByteString -> IO ()
sPutStr' h str =
    BSB.unsafeUseAsCStringLen str $ fcgxPutCStringLen h

fcgxPutCStringLen :: StreamPtr -> CStringLen -> IO ()
fcgxPutCStringLen h (cs,len) =
    testReturn "FCGX_PutStr" $ fcgx_putStr cs (fromIntegral len) h

sRead :: StreamPtr -> IO (Maybe BS.ByteString)
sRead h = buildByteString (fcgxGetBuf h) 4096

fcgxGetBuf :: StreamPtr -> Ptr a -> Int -> IO Int
fcgxGetBuf h p c =
    liftM fromIntegral $ fcgx_getStr (castPtr p) (fromIntegral c) h

--
-- * ByteString utilities
--

-- | Data.ByteString.Lazy.hGetContentsN generalized to arbitrary 
--   reading functions.
buildByteString :: (Ptr Word8 -> Int -> IO Int) -> Int -> IO (Maybe BS.ByteString)
buildByteString f k = do
    ps <- BSB.createAndTrim k $ \p -> f p k
    case BS.length ps of
        0         -> return Nothing
        _         -> return $ Just ps

--
-- * Utilities
--

testReturn :: String -> IO CInt -> IO ()
testReturn e = throwIfNeg_ (\n -> e ++ " failed with error code: "++ show n)

environToTable :: Environ -> IO [(S.ByteString, S.ByteString)]
environToTable arr = do
    css <- peekArray0 nullPtr arr
    ss <- mapM S.packCString css
    return $ map splitEq ss
  where
    splitEq s =
        let (a, b) = S.breakByte 61 s
         in (a, S.drop 1 b)
