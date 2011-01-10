module Network.Wai.Middleware.Debug (debug) where

import Network.Wai (Middleware, requestMethod, pathInfo)
import Data.ByteString.Char8 (unpack)
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (liftIO)

-- | Prints a message to 'stderr' for each request.
debug :: Middleware
debug app req = do
    liftIO $ hPutStrLn stderr $ concat
        [ unpack $ requestMethod req
        , " "
        , unpack $ pathInfo req
        ]
    app req
