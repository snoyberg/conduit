{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Launch (run) where

import Network.Wai
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import Data.IORef
import Control.Concurrent
import Foreign
import Foreign.C.String
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as S
import Data.Enumerator (($$), enumList, joinI)
import Blaze.ByteString.Builder (fromByteString)
#if !WINDOWS
import System.Cmd (rawSystem)
#endif
import Codec.Zlib.Enum (ungzip)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import qualified Data.Enumerator.List as EL

ping :: IORef Bool -> Middleware
ping  var app req
    | pathInfo req == ["_ping"] = do
        liftIO $ writeIORef var True
        return $ responseLBS status200 [] ""
    | otherwise = do
        res <- app req
        let renum = responseEnumerator res
        return $ ResponseEnumerator $ \f -> do
            renum $ \status headers -> do
                case lookup "content-type" headers of
                    Just ct
                        | "text/html" `S.isPrefixOf` ct -> do
                            let (isEnc, headers') = fixHeaders id headers
                            let fixEnc x =
                                    if isEnc
                                        then joinI $ builderToByteString $$ joinI $ ungzip $$ joinI $ EL.map fromByteString $$ x
                                        else x
                            fixEnc $ enumList 1 [fromByteString "<script>setInterval(function(){var x;if(window.XMLHttpRequest){x=new XMLHttpRequest();}else{x=new ActiveXObject(\"Microsoft.XMLHTTP\");}x.open(\"GET\",\"/_ping\",false);x.send();},60000)</script>"] $$ f status headers'
                    _ -> f status headers

fixHeaders front [] = (False, front [])
fixHeaders front (("content-encoding", "gzip"):rest) = (True, front rest)
fixHeaders front (x:xs) = fixHeaders (front . (:) x) xs

#if WINDOWS
foreign import ccall "launch"
    launch :: IO ()
#else
launch :: IO ()
launch = forkIO (rawSystem
#if MAC
    "open"
#else
    "xdg-open"
#endif
    ["http://localhost:4587/"] >> return ()) >> return ()
#endif

run :: Application -> IO ()
run app = do
    x <- newIORef True
    forkIO $ Warp.runSettings Warp.defaultSettings
        { Warp.settingsPort = 4587
        , Warp.settingsOnException = const $ return ()
        } $ ping x app
    launch
    loop x

loop :: IORef Bool -> IO ()
loop x = do
    let seconds = 120
    threadDelay $ 1000000 * seconds
    b <- readIORef x
    if b
        then writeIORef x False >> loop x
        else return ()
