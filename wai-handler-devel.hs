{-# LANGUAGE DeriveDataTypeable #-}
import Network.Wai.Handler.DevelServer (runQuit)
import System.Console.CmdArgs
import Control.Concurrent (forkIO)

data Devel = Devel
    { port :: Int
    , moduleName :: String
    , function :: String
    }
    deriving (Show, Data, Typeable)

main :: IO ()
main = do
    Devel p m f <- cmdArgs Devel
        { port = 3000 &= argPos 0 &= typ "PORT"
        , moduleName = "" &= argPos 1 &= typ "MODULE"
        , function = "" &= argPos 2 &= typ "FUNCTION"
        } &= summary "WAI development web server"
    runQuit p m f $ const $ return []
