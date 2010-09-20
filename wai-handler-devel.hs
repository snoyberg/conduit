{-# LANGUAGE DeriveDataTypeable #-}
import Network.Wai.Handler.DevelServer (run)
import System.Console.CmdArgs
import Control.Concurrent (forkIO)

data Devel = Devel
    { port :: Int
    , moduleName :: String
    , function :: String
    , yesod :: Bool
    }
    deriving (Show, Data, Typeable)

main :: IO ()
main = do
    Devel p m f y <- cmdArgs Devel
        { port = 3000 &= argPos 0 &= typ "PORT"
        , moduleName = "" &= argPos 1 &= typ "MODULE"
        , function = "" &= argPos 2 &= typ "FUNCTION"
        , yesod = False &= help "Monitor typical Yesod folders (hamlet, etc)"
        } &= summary "WAI development web server"
    _ <- forkIO $ run p m f $ folders y
    go
  where
    folders False = []
    folders True = ["hamlet", "cassius", "julius"]
    go = do
        x <- getLine
        case x of
            'q':_ -> putStrLn "Quitting, goodbye!"
            _ -> go
