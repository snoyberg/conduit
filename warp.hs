{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import Network.Wai.Application.Static
    ( StaticSettings (..), staticApp, defaultMimeTypeByExt, defaultListing
    )
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import System.Console.CmdArgs

data Args = Args
    { docroot :: FilePath
    , index :: [FilePath]
    , port :: Int
    , noindex :: Bool
    }
    deriving (Show, Data, Typeable)

defaultArgs = Args "." ["index.html", "index.htm"] 3000 False

main :: IO ()
main = do
    Args {..} <- cmdArgs defaultArgs
    args <- getArgs
    run port $ staticApp StaticSettings
        { ssFolder = docroot
        , ssIndices = if noindex then [] else index
        , ssListing = Just defaultListing
        , ssGetMimeType = return . defaultMimeTypeByExt
        }
