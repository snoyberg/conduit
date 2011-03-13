{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import Network.Wai.Application.Static
    ( StaticSettings (..), staticApp, defaultMimeTypeByExt, defaultListing
    )
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import System.Console.CmdArgs
import Text.Printf (printf)
import System.Directory (canonicalizePath)
import Control.Monad (unless)

data Args = Args
    { docroot :: FilePath
    , index :: [FilePath]
    , port :: Int
    , noindex :: Bool
    , quiet :: Bool
    }
    deriving (Show, Data, Typeable)

defaultArgs = Args "." ["index.html", "index.htm"] 3000 False False

main :: IO ()
main = do
    Args {..} <- cmdArgs defaultArgs
    docroot' <- canonicalizePath docroot
    args <- getArgs
    unless quiet $ printf "Serving directory %s on port %d with %s index files.\n" docroot' port (if noindex then "no" else show index)
    run port $ staticApp StaticSettings
        { ssFolder = docroot
        , ssIndices = if noindex then [] else index
        , ssListing = Just defaultListing
        , ssGetMimeType = return . defaultMimeTypeByExt
        }
