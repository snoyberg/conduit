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
import Data.Enumerator (($$), enumList, joinI, Enumeratee, Stream (..), Iteratee (..), Step (..))
import Blaze.ByteString.Builder (fromByteString)
#if !WINDOWS
import System.Cmd (rawSystem)
#endif
import Codec.Zlib.Enum (ungzip)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import qualified Data.Enumerator.List as EL
import Control.Monad.Trans.Class (lift)

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
                                        then joinI $ ungzip $$ x
                                        else x
                            joinI $ builderToByteString $$ fixEnc $ joinI $ insideHead "<script>setInterval(function(){var x;if(window.XMLHttpRequest){x=new XMLHttpRequest();}else{x=new ActiveXObject(\"Microsoft.XMLHTTP\");}x.open(\"GET\",\"/_ping\",false);x.send();},60000)</script>" $$ joinI $ EL.map fromByteString $$ f status headers'
                    _ -> f status headers

insideHead :: S.ByteString -> Enumeratee S.ByteString S.ByteString IO a
insideHead toInsert =
    go "" whole
  where
    whole = "<head>"
    go :: S.ByteString -> S.ByteString -> Step S.ByteString IO a -> Iteratee S.ByteString IO (Step S.ByteString IO a)
    go held atFront step = do
        mx <- EL.head
        case mx of
            Nothing -> feedDone $ Chunks [held, toInsert]
            Just x
                | atFront `S.isPrefixOf` x -> do
                    let y = S.drop (S.length atFront) x
                    let stream = Chunks [held, atFront, toInsert, y]
                    feedDone stream
                | whole `S.isInfixOf` x -> do
                    let (before, rest) = S.breakSubstring whole x
                    let after = S.drop (S.length whole) rest
                    feedDone $ Chunks [held, before, whole, toInsert, after]
                | x `S.isPrefixOf` atFront -> go
                    (held `S.append` x)
                    (S.drop (S.length x) atFront)
                    step
                | otherwise -> do
                    let (held', atFront', x') = getOverlap whole x
                    feedCont held' atFront' $ Chunks [held, x']
      where
        --feedDone :: Stream S.ByteString -> Iteratee S.ByteString IO (Step S.ByteString IO a)
        feedDone stream =
            case step of
                Continue k -> do
                    step' <- lift $ runIteratee $ k stream
                    EL.map id step'
                Yield b s -> return $ Yield b s
                Error e -> return $ Error e

        --feedCont :: Monad m => S.ByteString -> S.ByteString -> Stream S.ByteString -> Iteratee S.ByteString m (Step S.ByteString m a)
        feedCont held atFront stream = do
            case step of
                Continue k -> do
                    step' <- lift $ runIteratee $ k stream
                    go held atFront step'
                Yield b s -> return $ Yield b s
                Error e -> return $ Error e

getOverlap :: S.ByteString -> S.ByteString -> (S.ByteString, S.ByteString, S.ByteString)
getOverlap whole x =
    go whole
  where
    go piece
        | S.null piece = ("", whole, x)
        | piece `S.isSuffixOf` x =
            let x' = S.take (S.length x - S.length piece) x
                atFront = S.drop (S.length piece) whole
             in (piece, atFront, x')
        | otherwise = go $ S.init piece

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
