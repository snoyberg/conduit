import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import qualified Data.Text as T

main = run 3000 $ staticApp StaticSettings
    { ssFolder = fileSystemLookup "."
    , ssMkRedirect = defaultMkRedirect
    , ssGetMimeType = return . defaultMimeTypeByExt . T.unpack . fileName
    , ssDirListing = defaultDirListing
    , ssCacheSettings = NoCache
    }
