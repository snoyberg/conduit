{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Wai.Handler.Webkit (run) where

import Network.Wai (Application)
import qualified Network.Wai.Handler.SimpleServer as S
import Control.Concurrent (forkIO)

run :: Application -> IO ()
run app = do
    forkIO $ S.run 3000 app
    startBrowser

foreign import ccall "start_browser"
    startBrowser :: IO ()
