{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
      -- ** File/folder metadata
    , MetaData (..)
    , mdIsFile
    , getMetaData
      -- ** Directory listings
    , Listing
    , defaultListing
      -- * WAI application
    , StaticSettings (..)
    , staticApp
    ) where

import qualified Network.Wai as W
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Web.Routes.Base (decodePathInfo, encodePathInfo)
import System.PosixCompat.Files (fileSize, getFileStatus, modificationTime)
import System.Posix.Types (FileOffset, EpochTime)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)

import           Text.Blaze                  ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Renderer.Utf8    as HU
import qualified Text.Blaze.Html5.Attributes as A

import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)

import Data.List (sortBy, intercalate)
import Data.FileEmbed (embedFile)

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
    | SendContent MimeType L.ByteString
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
            -> [FilePath] -- ^ List of default index files. Cannot contain slashes.
            -> [String] -- ^ parsed request
            -> IO CheckPieces
checkPieces _ _ [".hidden", "folder.png"] =
    return $ SendContent "image/png" $ L.fromChunks [$(embedFile "folder.png")]
checkPieces _ _ [".hidden", "haskell.png"] =
    return $ SendContent "image/png" $ L.fromChunks [$(embedFile "haskell.png")]
checkPieces prefix indices pieces
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
                if de
                    then do
                        x <- checkIndices fp indices
                        case x of
                            Just index -> return $ Redirect $ setLast pieces index
                            Nothing ->
                                if isFolder
                                    then return $ DirectoryResponse fp
                                    else return $ Redirect $ pieces ++ [""]
                    else return NotFound
  where
    setLast [] x = [x]
    setLast [""] x = [x]
    setLast (a:b) x = a : setLast b x
    checkIndices _ [] = return Nothing
    checkIndices fp (i:is) = do
        let fp' = fp ++ '/' : i
        fe <- doesFileExist fp'
        if fe
            then return $ Just i
            else checkIndices fp is

type Listing = [String] -> FilePath -> IO L.ByteString

data StaticSettings = StaticSettings
    { ssFolder :: FilePath
    , ssIndices :: [FilePath]
    , ssListing :: Maybe Listing
    , ssGetMimeType :: FilePath -> IO MimeType
    }

staticApp :: StaticSettings -> W.Application
staticApp (StaticSettings folder indices mlisting getmime) req = liftIO $ do
    let pieces = decodePathInfo $ S8.unpack $ W.pathInfo req
    cp <- checkPieces folder indices pieces
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
        FileResponse fp -> do
            mimetype <- getmime fp
            filesize <- fileSize `fmap` getFileStatus fp
            return $ W.ResponseFile W.status200
                        [ ("Content-Type", mimetype)
                        , ("Content-Length", S8.pack $ show filesize)
                        ] fp
        DirectoryResponse fp ->
            case mlisting of
                Just listing -> do
                    lbs <- listing pieces fp
                    return $ W.responseLBS W.status200
                        [ ("Content-Type", "text/html; charset=utf-8")
                        ] lbs
                Nothing -> return $ W.responseLBS W.status403
                        [ ("Content-Type", "text/plain")
                        ] "Directory listings disabled"
        SendContent mt lbs -> return $ W.responseLBS W.status200
                        [ ("Content-Type", mt)
                        ] lbs

-- Code below taken from Happstack: http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-server/src/Happstack/Server/FileServe/BuildingBlocks.hs
defaultListing :: Listing
defaultListing pieces localPath = do
    fps <- getDirectoryContents localPath
    fps' <- mapM (getMetaData localPath) fps
    let isTop = null pieces || pieces == [""]
    let fps'' = if isTop then fps' else Just (FolderMetaData "..") : fps'
    return $ HU.renderHtml
           $ H.html $ do
             H.head $ do
                 let title = intercalate "/" pieces
                 let title' = if null title then "root folder" else title
                 H.title $ H.string title'
                 H.style $ H.string $ unlines [ "table { margin: 0 auto; width: 760px; border-collapse: collapse; font-family: 'sans-serif'; }"
                                              , "table, th, td { border: 1px solid #353948; }" 
                                              , "td.size { text-align: right; font-size: 0.7em; width: 50px }"
                                              , "td.date { text-align: right; font-size: 0.7em; width: 130px }"
                                              , "td { padding-right: 1em; padding-left: 1em; }"
                                              , "th.first { background-color: white; width: 24px }"
                                              , "td.first { padding-right: 0; padding-left: 0; text-align: center }"
                                              , "tr { background-color: white; }"
                                              , "tr.alt { background-color: #A3B5BA}"
                                              , "th { background-color: #3C4569; color: white; font-size: 1.125em; }"
                                              , "h1 { width: 760px; margin: 1em auto; font-size: 1em }"
                                              , "img { width: 20px }"
                                              , "a { text-decoration: none }"
                                              ]
             H.body $ do
                 H.h1 $ showFolder $ "" : filter (not . null) pieces
                 renderDirectoryContentsTable haskellSrc folderSrc $ catMaybes fps''
  where
    image x = concatMap (const "../") (drop 1 pieces) ++ ".hidden/" ++ x ++ ".png"
    folderSrc = image "folder"
    haskellSrc = image "haskell"
    showName "" = "root"
    showName x = x
    showFolder [] = H.string "FIXME: Unexpected showFolder []"
    showFolder [x] = H.string $ showName x
    showFolder (x:xs) = do
        let href = concat $ replicate (length xs) "../"
        H.a ! A.href (H.stringValue href) $ H.string $ showName x
        H.string " / "
        showFolder xs

-- | a function to generate an HTML table showing the contents of a directory on the disk
--
-- This function generates most of the content of the
-- 'renderDirectoryContents' page. If you want to style the page
-- differently, or add google analytics code, etc, you can just create
-- a new page template to wrap around this HTML.
--
-- see also: 'getMetaData', 'renderDirectoryContents'
renderDirectoryContentsTable :: String
                             -> String
                             -> [MetaData] -- ^ list of files+meta data, see 'getMetaData'
                             -> H.Html
renderDirectoryContentsTable haskellSrc folderSrc fps =
           H.table $ do H.thead $ do H.th ! (A.class_ $ H.stringValue "first") $ H.img ! (A.src $ H.stringValue haskellSrc)
                                     H.th $ H.string "Name"
                                     H.th $ H.string "Modified"
                                     H.th $ H.string "Size"
                        H.tbody $ mapM_ mkRow (zip (sortBy sortMD fps) $ cycle [False, True])
    where
      sortMD FolderMetaData{} FileMetaData{} = LT
      sortMD FileMetaData{} FolderMetaData{} = GT
      sortMD x y = mdName x `compare` mdName y
      mkRow :: (MetaData, Bool) -> H.Html
      mkRow (md, alt) =
          (if alt then (! A.class_ (H.stringValue "alt")) else id) $
          H.tr $ do
                   H.td ! A.class_ (H.stringValue "first")
                        $ if mdIsFile md
                              then return ()
                              else H.img ! A.src (H.stringValue folderSrc)
                                         ! A.alt (H.stringValue "Folder")
                   H.td (H.a ! A.href (H.stringValue $ mdName md ++ if mdIsFile md then "" else "/")  $ H.string $ mdName md)
                   H.td ! A.class_ (H.stringValue "date") $ H.string $
                       if mdIsFile md
                           then formatCalendarTime defaultTimeLocale "%d-%b-%Y %X" $ mdModified md
                           else ""
                   H.td ! A.class_ (H.stringValue "size") $ H.string $
                       if mdIsFile md
                           then prettyShow $ mdSize md
                           else ""
      formatCalendarTime a b c =  formatTime a b $ posixSecondsToUTCTime (realToFrac c :: POSIXTime)
      prettyShow x
        | x > 1024 = prettyShowK $ x `div` 1024
        | otherwise = addCommas "B" x
      prettyShowK x
        | x > 1024 = prettyShowM $ x `div` 1024
        | otherwise = addCommas "KB" x
      prettyShowM x
        | x > 1024 = prettyShowG $ x `div` 1024
        | otherwise = addCommas "MB" x
      prettyShowG x = addCommas "GB" x
      addCommas s = (++ (' ' : s)) . reverse . addCommas' . reverse . show
      addCommas' (a:b:c:d:e) = a : b : c : ',' : addCommas' (d : e)
      addCommas' x = x


data MetaData =
    FileMetaData
        { mdName :: FilePath
        , mdModified :: EpochTime
        , mdSize :: FileOffset
        }
  | FolderMetaData
        { mdName :: FilePath
        }
  deriving Show

mdIsFile :: MetaData -> Bool
mdIsFile FileMetaData{} = True
mdIsFile FolderMetaData{} = False

-- | look up the meta data associated with a file
getMetaData :: FilePath -- ^ path to directory on disk containing the entry
            -> FilePath -- ^ entry in that directory
            -> IO (Maybe MetaData)
getMetaData _ ('.':_) = return Nothing
getMetaData localPath fp = do
    let fp' = localPath ++ '/' : fp
    fe <- doesFileExist fp'
    if fe
        then do
            fs <- getFileStatus fp'
            let modTime = modificationTime fs
            let count = fileSize fs
            return $ Just $ FileMetaData fp modTime count
        else do
            de <- doesDirectoryExist fp'
            return $ if de then Just (FolderMetaData fp) else Nothing
