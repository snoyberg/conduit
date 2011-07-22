{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Wai.Handler.Webkit (run) where

import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as S
import Control.Concurrent (forkOS)
import Control.Concurrent.MVar
import Foreign.C.String (CString, withCString)
import Control.Exception (finally)

run :: String -- ^ Title to show in titlebar
    -> Application -> IO ()
run title app = do
    mvar <- newEmptyMVar
    _ <- forkOS $ finally (S.run 3000 app) (putMVar mvar ())
    _ <- forkOS $ finally (withCString title startBrowser) (putMVar mvar ())
    _ <- takeMVar mvar
    return ()

foreign import ccall "start_browser"
    startBrowser :: CString -> IO ()
