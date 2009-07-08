module Hack.Handler.Webkit (run) where

import Webkit (startBrowser)
import Hack (Application)
import qualified Hack.Handler.SimpleServer as S
import Control.Concurrent (forkIO)

run :: Application -> IO ()
run app = do
    forkIO $ S.run 3000 app
    startBrowser
