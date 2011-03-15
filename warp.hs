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
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Debug
import Network.Wai.Middleware.Gzip

data Args = Args
    { docroot :: FilePath
    , index :: [FilePath]
    , port :: Int
    , noindex :: Bool
    , quiet :: Bool
    , verbose :: Bool
    }
    deriving (Show, Data, Typeable)

defaultArgs = Args "." ["index.html", "index.htm"] 3000 False False False

main :: IO ()
main = do
    Args {..} <- cmdArgs defaultArgs
    docroot' <- canonicalizePath docroot
    args <- getArgs
    unless quiet $ printf "Serving directory %s on port %d with %s index files.\n" docroot' port (if noindex then "no" else show index)
    let middle = gzip False
               . (if verbose then debug else id)
               . autohead
    run port $ middle $ staticApp StaticSettings
        { ssFolder = docroot
        , ssIndices = if noindex then [] else index
        , ssListing = Just defaultListing
        , ssGetMimeType = return . defaultMimeTypeByExt
        }
