{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Wai.Handler.FastCGI
-- Copyright   :  (c) Bjorn Bringert 2004-2005, (c) Lemmih 2006
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
    ) where

import Data.Maybe
import Control.Monad    ( liftM )
import Data.Word (Word8)
import Foreign          ( Ptr, castPtr, nullPtr, peekArray0
                        , alloca, throwIfNeg_)
import Foreign.C        ( CInt, CString, CStringLen
                        , peekCString )
import Foreign.Storable ( Storable (..) )

import qualified Network.Wai as W
import qualified Network.Wai.Handler.CGI as CGI
import qualified Network.Wai.Handler.Helper as CGI

import qualified Data.ByteString as BS
#if __GLASGOW_HASKELL__ >= 608
import qualified Data.ByteString.Internal as BSB
import qualified Data.ByteString.Unsafe   as BSB
#else
import qualified Data.ByteString.Base as BSB
#endif

-- For debugging
import Prelude hiding     ( log, catch )

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

foreign import ccall safe "fcgiapp.h FCGX_Accept" fcgx_accept
    :: Ptr StreamPtr -> Ptr StreamPtr -> Ptr StreamPtr -> Ptr Environ -> IO CInt
foreign import ccall unsafe "fcgiapp.h FCGX_Finish" fcgx_finish
    :: IO ()

------------------------------------------------------------------------

-- | Handle FastCGI requests in an infinite loop.
run :: W.Application -> IO ()
run f = runOneFastCGI Nothing f >> run f

-- | Handle FastCGI requests in an infinite loop. For a server which supports
-- the X-Sendfile header.
runSendfile :: String -> W.Application -> IO ()
runSendfile sf f = runOneFastCGI (Just sf) f >> runSendfile sf f

-- | Handle a single FastCGI request.
runOneFastCGI :: Maybe String -- X-Sendfile
              -> W.Application -> IO ()
runOneFastCGI xsendfile f = do
    alloca (\inp ->
            alloca (\outp ->
                    alloca (\errp ->
                            alloca (\envp ->
                                    oneRequest f inp outp errp envp
                                               xsendfile))))

oneRequest :: W.Application
           -> Ptr StreamPtr
           -> Ptr StreamPtr
           -> Ptr StreamPtr
           -> Ptr Environ
           -> Maybe String -- X-Sendfile
           -> IO ()
oneRequest f inp outp errp envp xsendfile =
    do
    testReturn "FCGX_Accept" $ fcgx_accept inp outp errp envp
    ins  <- peek inp
    outs <- peek outp
    errs <- peek errp
    env  <- peek envp
    handleRequest f ins outs errs env xsendfile
    fcgx_finish

handleRequest :: W.Application
              -> StreamPtr
              -> StreamPtr
              -> StreamPtr
              -> Environ
              -> Maybe String -- sendfile
              -> IO ()
handleRequest f ins outs _errs env xsendfile =
    do
    vars <- environToTable env
    let input = const $ sRead ins
    let hPut = sPutStr' outs
    CGI.run'' vars (CGI.requestBodyFunc input) hPut xsendfile f

data FCGX_Request

_peekErr :: Ptr FCGX_Request -> IO (Ptr FCGX_Stream)
_peekErr = (#peek FCGX_Request, err)

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

environToTable :: Environ -> IO [(String,String)]
environToTable arr =
    do css <- peekArray0 nullPtr arr
       ss <- mapM peekCString css
       return $ map (splitBy '=') ss

-- | Split a list at the first occurence of a marker.
--   Do not include the marker in any of the resulting lists.
--   If the marker does not occur in the list, the entire
--   input with be in the first list.
splitBy :: Eq a => a -> [a] -> ([a],[a])
splitBy x xs = (y, drop 1 z)
    where (y,z) = break (==x) xs
