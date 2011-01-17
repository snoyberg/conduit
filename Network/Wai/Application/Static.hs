{-# LANGUAGE OverloadedStrings #-}
-- | Static file serving for WAI.
module Network.Wai.Application.Static
    ( -- * Generic, non-WAI code
      -- ** Mime types
      MimeType
    , defaultMimeType
      -- ** Mime type by file extension
    , Extension
    , MimeMap
    , takeExtensions
    , defaultMimeTypes
    , mimeTypeByExt
    , defaultMimeTypeByExt
      -- ** Finding files
    , CheckPieces
    , checkPieces
      -- * WAI application
    , StaticSettings (..)
    , staticApp
    ) where

import qualified Network.Wai as W
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import System.Directory (doesFileExist, doesDirectoryExist)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Web.Routes.Base (decodePathInfo, encodePathInfo)
import System.PosixCompat.Files (fileSize, getFileStatus)
import Control.Monad.IO.Class (liftIO)

-- | A list of all possible extensions, starting from the largest.
takeExtensions :: FilePath -> [String]
takeExtensions s =
    case break (== '.') s of
        (_, '.':x) -> x : takeExtensions x
        (_, _) -> []

type MimeType = ByteString
type Extension = String
type MimeMap = Map Extension MimeType

defaultMimeType :: MimeType
defaultMimeType = "application/octet-stream"

-- taken from snap-core Snap.Util.FileServer
defaultMimeTypes :: MimeMap
defaultMimeTypes = Map.fromList [
  ( "asc"     , "text/plain"                        ),
  ( "asf"     , "video/x-ms-asf"                    ),
  ( "asx"     , "video/x-ms-asf"                    ),
  ( "avi"     , "video/x-msvideo"                   ),
  ( "bz2"     , "application/x-bzip"                ),
  ( "c"       , "text/plain"                        ),
  ( "class"   , "application/octet-stream"          ),
  ( "conf"    , "text/plain"                        ),
  ( "cpp"     , "text/plain"                        ),
  ( "css"     , "text/css"                          ),
  ( "cxx"     , "text/plain"                        ),
  ( "dtd"     , "text/xml"                          ),
  ( "dvi"     , "application/x-dvi"                 ),
  ( "gif"     , "image/gif"                         ),
  ( "gz"      , "application/x-gzip"                ),
  ( "hs"      , "text/plain"                        ),
  ( "htm"     , "text/html"                         ),
  ( "html"    , "text/html"                         ),
  ( "jar"     , "application/x-java-archive"        ),
  ( "jpeg"    , "image/jpeg"                        ),
  ( "jpg"     , "image/jpeg"                        ),
  ( "js"      , "text/javascript"                   ),
  ( "log"     , "text/plain"                        ),
  ( "m3u"     , "audio/x-mpegurl"                   ),
  ( "mov"     , "video/quicktime"                   ),
  ( "mp3"     , "audio/mpeg"                        ),
  ( "mpeg"    , "video/mpeg"                        ),
  ( "mpg"     , "video/mpeg"                        ),
  ( "ogg"     , "application/ogg"                   ),
  ( "pac"     , "application/x-ns-proxy-autoconfig" ),
  ( "pdf"     , "application/pdf"                   ),
  ( "png"     , "image/png"                         ),
  ( "ps"      , "application/postscript"            ),
  ( "qt"      , "video/quicktime"                   ),
  ( "sig"     , "application/pgp-signature"         ),
  ( "spl"     , "application/futuresplash"          ),
  ( "swf"     , "application/x-shockwave-flash"     ),
  ( "tar"     , "application/x-tar"                 ),
  ( "tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( "tar.gz"  , "application/x-tgz"                 ),
  ( "tbz"     , "application/x-bzip-compressed-tar" ),
  ( "text"    , "text/plain"                        ),
  ( "tgz"     , "application/x-tgz"                 ),
  ( "torrent" , "application/x-bittorrent"          ),
  ( "ttf"     , "application/x-font-truetype"       ),
  ( "txt"     , "text/plain"                        ),
  ( "wav"     , "audio/x-wav"                       ),
  ( "wax"     , "audio/x-ms-wax"                    ),
  ( "wma"     , "audio/x-ms-wma"                    ),
  ( "wmv"     , "video/x-ms-wmv"                    ),
  ( "xbm"     , "image/x-xbitmap"                   ),
  ( "xml"     , "text/xml"                          ),
  ( "xpm"     , "image/x-xpixmap"                   ),
  ( "xwd"     , "image/x-xwindowdump"               ),
  ( "zip"     , "application/zip"                   )]

mimeTypeByExt :: MimeMap
              -> MimeType -- ^ default mime type
              -> FilePath
              -> MimeType
mimeTypeByExt mm def =
    go . takeExtensions
  where
    go [] = def
    go (e:es) =
        case Map.lookup e mm of
            Nothing -> go es
            Just mt -> mt

defaultMimeTypeByExt :: FilePath -> MimeType
defaultMimeTypeByExt = mimeTypeByExt defaultMimeTypes defaultMimeType

data CheckPieces
    = Redirect [String]
    | Forbidden
    | NotFound
    | FileResponse FilePath
    | DirectoryResponse FilePath
    deriving Show

anyButLast :: (a -> Bool) -> [a] -> Bool
anyButLast _ [] = False
anyButLast _ [_] = False
anyButLast p (x:xs)
    | p x == True = True
    | otherwise = anyButLast p xs

filterButLast :: (a -> Bool) -> [a] -> [a]
filterButLast _ [] = []
filterButLast _ [x] = [x]
filterButLast f (x:xs)
    | f x = x : filterButLast f xs
    | otherwise = filterButLast f xs

unsafe :: FilePath -> Bool
unsafe ('.':_) = True
unsafe s = any (== '/') s

stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash "/" = ""
stripTrailingSlash "" = ""
stripTrailingSlash (x:xs) = x : stripTrailingSlash xs

checkPieces :: FilePath -- ^ static file prefix
            -> [String] -- ^ parsed request
            -> IO CheckPieces
checkPieces prefix pieces
    | any unsafe pieces = return Forbidden
    | anyButLast null pieces =
        return $ Redirect $ filterButLast (not . null) pieces
    | otherwise = do
        let fp = concat $ prefix : map ((:) '/') pieces
        let (isFile, isFolder) =
                case () of
                    ()
                        | null pieces -> (True, True)
                        | null (last pieces) -> (False, True)
                        | otherwise -> (True, False)
        fe <- doesFileExist $ stripTrailingSlash fp
        case (fe, isFile) of
            (True, True) -> return $ FileResponse fp
            (True, False) -> return $ Redirect $ init pieces
            (False, _) -> do
                de <- doesDirectoryExist fp
                case (de, isFolder) of
                    (True, True) -> return $ DirectoryResponse fp
                    (True, False) -> return $ Redirect $ pieces ++ [""]
                    (False, _) -> return NotFound

type Listing = FilePath -> IO L.ByteString

data StaticSettings = StaticSettings
    { ssFolder :: FilePath
    , ssIndices :: [FilePath]
    , ssListing :: Maybe Listing
    , ssGetMimeType :: FilePath -> IO MimeType
    }

staticApp :: StaticSettings -> W.Application
staticApp (StaticSettings folder indices mlisting getmime) req = liftIO $ do
    let pieces = decodePathInfo $ S8.unpack $ W.pathInfo req
    cp <- checkPieces folder pieces
    case cp of
        Redirect pieces' -> do
            let loc = S8.pack $ '/' : encodePathInfo pieces' []
            return $ W.responseLBS W.status301
                [ ("Content-Type", "text/plain")
                , ("Location", loc)
                ] "Redirect"
        Forbidden -> return $ W.responseLBS W.status403
                        [ ("Content-Type", "text/plain")
                        ] "Forbidden"
        NotFound -> return $ W.responseLBS W.status404
                        [ ("Content-Type", "text/plain")
                        ] "File not found"
        FileResponse fp -> sendfile fp
        DirectoryResponse fp -> sendfolder fp indices
  where
    sendfolder fp (i:is) = do
        let fp' = fp ++ '/' : i
        fe <- doesFileExist fp'
        if fe
            then sendfile fp'
            else sendfolder fp is
    sendfolder fp [] =
        case mlisting of
            Just listing -> do
                lbs <- listing fp
                return $ W.responseLBS W.status200
                    [ ("Content-Type", "text/html")
                    ] lbs
            Nothing -> return $ W.responseLBS W.status403
                    [ ("Content-Type", "text/plain")
                    ] "Directory listings disabled"
    sendfile fp = do
        mimetype <- getmime fp
        filesize <- fileSize `fmap` getFileStatus fp
        return $ W.ResponseFile W.status200
                    [ ("Content-Type", mimetype)
                    , ("Content-Length", S8.pack $ show filesize)
                    ] fp
