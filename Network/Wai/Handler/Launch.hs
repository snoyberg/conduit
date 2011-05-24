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
import Data.Enumerator (($$), enumList)
import Blaze.ByteString.Builder (fromByteString)

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
                            enumList 1 [fromByteString "<script>setInterval(function(){var x;if(window.XMLHttpRequest){x=new XMLHttpRequest();}else{x=new ActiveXObject(\"Microsoft.XMLHTTP\");}x.open(\"GET\",\"/_ping\",false);x.send();},1000)</script>"] $$ f status headers
                    _ -> f status headers

#if WINDOWS
foreign import ccall "launch"
    launch :: IO ()
#else
#endif

run :: Application -> IO ()
run app = do
    x <- newIORef True
    forkIO $ Warp.run 4587 $ ping x app
    launch
    loop x

loop :: IORef Bool -> IO ()
loop x = do
    let seconds = 2
    threadDelay $ 1000000 * seconds
    b <- readIORef x
    if b
        then writeIORef x False >> loop x
        else return ()
