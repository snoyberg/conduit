{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Wai.Handler.SCGI
    ( run
    , runSendfile
    ) where

import Network.Wai
import Network.Wai.Handler.CGI (runGeneric, requestBodyFunc)
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Char8 as S8
import Data.IORef
import Data.ByteString.Lazy.Internal (defaultChunkSize)

run :: Application -> IO ()
run app = runOne Nothing app >> run app

runSendfile :: ByteString -> Application -> IO ()
runSendfile sf app = runOne (Just sf) app >> runSendfile sf app

runOne :: Maybe ByteString -> Application -> IO ()
runOne sf app = do
    socket <- c'accept 0 nullPtr nullPtr
    headersBS <- readNetstring socket
    let headers@((_, conLenS):_) = parseHeaders $ S.split 0 headersBS
    let conLen = case reads conLenS of
                    (i, _):_ -> i
                    [] -> 0
    conLenI <- newIORef conLen
    runGeneric headers (requestBodyFunc $ input socket conLenI)
              (write socket) sf app
    drain socket conLenI
    _ <- c'close socket
    return ()

write :: CInt -> S.ByteString -> IO ()
write socket bs = S.unsafeUseAsCStringLen bs $ \(s, l) -> do
    _ <- c'write socket s (fromIntegral l)
    return ()

input :: CInt -> IORef Int -> Int -> IO (Maybe S.ByteString)
input socket ilen rlen = do
    len <- readIORef ilen
    case len of
        0 -> return Nothing
        _ -> do
            bs <- readByteString socket
                $ minimum [defaultChunkSize, len, rlen]
            writeIORef ilen $ len - S.length bs
            return $ Just bs

drain :: CInt -> IORef Int -> IO () -- FIXME do it in chunks
drain socket ilen = do
    len <- readIORef ilen
    _ <- readByteString socket len
    return ()

parseHeaders :: [S.ByteString] -> [(String, String)]
parseHeaders (x:y:z) = (S8.unpack x, S8.unpack y) : parseHeaders z
parseHeaders _ = []

readNetstring :: CInt -> IO S.ByteString
readNetstring socket = do
    len <- readLen 0
    bs <- readByteString socket len
    _ <- readByteString socket 1 -- the comma
    return bs
  where
    readLen l = do
        bs <- readByteString socket 1
        let [c] = S8.unpack bs
        if c == ':'
            then return l
            else readLen $ l * 10 + (fromEnum c - fromEnum '0')

readByteString :: CInt -> Int -> IO S.ByteString
readByteString socket len = do
    buf <- mallocBytes len
    _ <- c'read socket buf $ fromIntegral len
    S.unsafePackCStringFinalizer (castPtr buf) len $ free buf

foreign import ccall unsafe "accept"
    c'accept :: CInt -> Ptr a -> Ptr a -> IO CInt

foreign import ccall unsafe "close"
    c'close :: CInt -> IO CInt

foreign import ccall unsafe "write"
    c'write :: CInt -> Ptr CChar -> CInt -> IO CInt

foreign import ccall unsafe "read"
    c'read :: CInt -> Ptr CChar -> CInt -> IO CInt
