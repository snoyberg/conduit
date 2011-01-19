import Network.Wai.Application.Static
    ( StaticSettings (..), staticApp, defaultMimeTypeByExt, defaultListing
    )
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    (folder, port) <- return $
        case args of
            [] -> (".", 3000)
            [f] -> (f, 3000)
            [f, p] -> (f, read p)
            _ -> error "Usage: warp-static [root [port]]"
    run port $ staticApp StaticSettings
        { ssFolder = folder
        , ssIndices = ["index.html", "index.htm"]
        , ssListing = Just defaultListing
        , ssGetMimeType = return . defaultMimeTypeByExt
        }
