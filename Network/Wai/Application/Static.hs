{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
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
    , Pieces
    , pathFromPieces
      -- ** File/folder metadata
    , MetaData (..)
    , mdIsFile
    , getMetaData
      -- ** Directory listings
    , Listing
    , defaultListing
    , defaultDirListing
      -- * WAI application
    , staticApp
    , staticAppPieces
      -- ** Settings
    , StaticSettings (..)
    , defaultStaticSettings
    , defaultPublicSettings
    , CacheSettings (..)
      -- should be moved to common helper
    , unfixPathName
      -- new stuff to be sorted
    , fileSystemLookup
    , embeddedLookup
    , defaultMkRedirect
    , File (..)
    , toEmbedded
    ) where

import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import System.PosixCompat.Files (fileSize, getFileStatus, modificationTime)
import System.Posix.Types (FileOffset, EpochTime)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes, isNothing, isJust)

import           Text.Blaze                  ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Renderer.Utf8    as HU
import qualified Text.Blaze.Html5.Attributes as A

import Blaze.ByteString.Builder (toByteString, copyByteString, fromByteString)
import Data.Monoid (mappend)

import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)

import Data.List (sortBy)
import Data.FileEmbed (embedFile)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Control.Arrow ((&&&), second)
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)

#ifdef PRINT
import Debug.Trace
debug :: (Show a) => a -> a
debug a = trace ("DEBUG: " ++ show a) a
#else
trace :: String -> a -> a 
trace _ x = x
debug :: a -> a
debug = id
#endif

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
  ( "apk"     , "application/vnd.android.package-archive" ),
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
  ( "epub"    , "application/epub+zip"              ),
  ( "gif"     , "image/gif"                         ),
  ( "gz"      , "application/x-gzip"                ),
  ( "hs"      , "text/plain"                        ),
  ( "htm"     , "text/html"                         ),
  ( "html"    , "text/html"                         ),
  ( "ico"     , "image/vnd.microsoft.icon"          ),
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
  ( "svg"     , "image/svg+xml"                     ),
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
    = Redirect Pieces
    | Forbidden
    | NotFound
    | FileResponse File
    | NotModified
    | DirectoryResponse Folder
    -- TODO: add file size
    | SendContent MimeType L.ByteString

safeInit  :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

filterButLast :: (a -> Bool) -> [a] -> [a]
filterButLast _ [] = []
filterButLast _ [x] = [x]
filterButLast f (x:xs)
    | f x = x : filterButLast f xs
    | otherwise = filterButLast f xs


unsafe :: T.Text -> Bool
unsafe s | T.null s = False
         | T.head s == '.' = True
         | otherwise = T.any (== '/') s

stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash "/" = ""
stripTrailingSlash "" = ""
stripTrailingSlash (x:xs) = x : stripTrailingSlash xs

type Pieces = [T.Text]
relativeDirFromPieces :: Pieces -> T.Text
relativeDirFromPieces pieces = T.concat $ map (const "../") (drop 1 pieces) -- last piece is not a dir

pathFromPieces :: FilePath -> Pieces -> FilePath
pathFromPieces prefix pieces =
        concat $ prefix : map ((:) '/') (map unfixPathName $ map T.unpack pieces)

checkPieces :: (Pieces -> IO FileLookup) -- ^ file lookup function
            -> [T.Text]                  -- ^ List of default index files. Cannot contain slashes.
            -> Pieces                    -- ^ parsed request
            -> CacheSettings
            -> W.Request
            -> IO CheckPieces
checkPieces _ _ [".hidden", "folder.png"] _ _ =
    return $ SendContent "image/png" $ L.fromChunks [$(embedFile "folder.png")]
checkPieces _ _ [".hidden", "haskell.png"] _ _ =
    return $ SendContent "image/png" $ L.fromChunks [$(embedFile "haskell.png")]
checkPieces fileLookup indices pieces cache req
    | any unsafe pieces = return Forbidden
    | any T.null $ safeInit pieces =
        return $ Redirect $ filterButLast (not . T.null) pieces
    | otherwise = do
        let (isFile, isFolder) =
                case () of
                    ()
                        | null pieces -> (True, True)
                        | T.null (last pieces) -> (False, True)
                        | otherwise -> (True, False)

        let fp = undefined
        if not isFile then uncached isFile isFolder
           else
             case cache of
               ETag ioLookup -> do
                 -- No support for If-Match
                 let mlastEtag = lookup "If-None-Match" (W.requestHeaders req)
                 metag <- ioLookup fp
                 case debug (metag, mlastEtag) of
                   (Just hash, Just lastHash) | hash == lastHash -> return NotModified
                   _ -> trace "ETAG: no cache match" uncached isFile isFolder
               Forever isStaticFile -> 
                   if isStaticFile fp (S8.drop 1 $ W.rawQueryString req) &&
                       (isJust $ lookup "If-Modified-Since" (W.requestHeaders req)) &&
                       (isNothing $ lookup "If-Unmodified-Since" (W.requestHeaders req))
                       then return NotModified
                       else trace "Static: no cache match" uncached isFile isFolder
               NoCache    -> trace "NoCache" uncached isFile isFolder


  where
    uncached isFile isFolder = do
      fl <- fileLookup pieces
      case (fl, isFile) of
          (FLDoesNotExist, _) -> return NotFound
          (FLFile file, True)  -> return $ FileResponse file
          (FLFile{}, False) -> return $ Redirect $ init pieces
          (FLFolder folder@(Folder contents), _) -> do
              case checkIndices $ map feName $ filter feIsFile contents of
                  Just index -> return $ Redirect $ setLast pieces index
                  Nothing ->
                      if isFolder
                          then return $ DirectoryResponse folder
                          else return $ Redirect $ pieces ++ [""]

    setLast [] x = [x]
    setLast [""] x = [x]
    setLast (a:b) x = a : setLast b x

    checkIndices :: [T.Text] -> Maybe T.Text
    checkIndices contents =
        go indices
      where
        go [] = Nothing
        go (i:is)
            | i `elem` contents = Just i
            | otherwise = go is

type Listing = (Pieces -> Folder -> IO L.ByteString)

data StaticDirListing = ListingForbidden | StaticDirListing {
    ssListing :: Listing
  , ssIndices :: [T.Text]
}

defaultDirListing :: StaticDirListing
defaultDirListing = StaticDirListing defaultListing []

-- IO is for development mode
type CheckHashParam = (File -> S8.ByteString -> Bool)
data CacheSettings = NoCache | Forever CheckHashParam | ETag (File -> IO (Maybe S8.ByteString))

oneYear :: Int
oneYear = 60 * 60 * 24 * 365

data FileLookup = FLFolder Folder | FLFile File | FLDoesNotExist

data Folder = Folder [FolderEntry] -- Bool: True = file, False = folder

data FolderEntry = FolderEntry
    { feName :: T.Text
    , feIsFile :: Bool
    , feGetMetaData :: IO (Maybe MetaData)
    }

data File = File
    { fileGetSize :: IO Int
    , fileToResponse :: H.Status -> H.ResponseHeaders -> W.Response
    , fileName :: T.Text
    }

data StaticSettings = StaticSettings
    { ssFolder :: Pieces -> IO FileLookup
    , ssMkRedirect :: Pieces -> ByteString -> S8.ByteString
    , ssGetMimeType :: File -> IO MimeType
    , ssDirListing :: StaticDirListing
    , ssCacheSettings :: CacheSettings
    }

defaultMkRedirect :: Pieces -> ByteString -> S8.ByteString
defaultMkRedirect pieces newPath =
  let relDir = TE.encodeUtf8 (relativeDirFromPieces pieces) in
    S8.append relDir (if (S8.last relDir) == '/' && (S8.head newPath) == '/'
                       then S8.tail newPath
                       else newPath)

defaultStaticSettings :: CacheSettings -> StaticSettings
defaultStaticSettings isStaticFile = StaticSettings { ssFolder = fileSystemLookup "static"
  , ssMkRedirect = defaultMkRedirect
  , ssGetMimeType = return . defaultMimeTypeByExt . T.unpack . fileName
  , ssDirListing = defaultDirListing
  , ssCacheSettings = isStaticFile
}
defaultPublicSettings :: CacheSettings -> StaticSettings
defaultPublicSettings etags = StaticSettings { ssFolder = fileSystemLookup "public"
  , ssMkRedirect = defaultMkRedirect
  , ssGetMimeType = return . defaultMimeTypeByExt . T.unpack . fileName
  , ssDirListing = ListingForbidden
  , ssCacheSettings = etags
}

fileSystemLookup :: FilePath -> Pieces -> IO FileLookup
fileSystemLookup prefix pieces = do
    let fp = pathFromPieces prefix pieces
    fe <- doesFileExist fp
    if fe
        then do
            return $ FLFile File
                { fileGetSize = fmap (fromIntegral . fileSize) $ getFileStatus fp
                , fileToResponse = \s h -> W.ResponseFile s h fp Nothing
                , fileName = last pieces
                }
        else do
            de <- doesDirectoryExist fp
            if de
                then do
                    entries <- getDirectoryContents fp >>= (mapM $ \name -> do
                        let name' = T.pack name -- FIXME fix/unfix, not sure which
                        let fp' = fp ++ '/' : name
                        fe' <- doesFileExist fp'
                        return FolderEntry
                            { feName = name'
                            , feIsFile = fe'
                            , feGetMetaData = getMetaData fp name
                            })
                    return $ FLFolder $ Folder entries
                else return FLDoesNotExist

type Embedded = Map.Map T.Text EmbeddedEntry

data EmbeddedEntry = EEFile S8.ByteString | EEFolder Embedded

embeddedLookup :: Embedded -> Pieces -> IO FileLookup -- FIXME broken for unicode path names
embeddedLookup root pieces =
    return $ elookup pieces root
  where
    elookup [] x = FLFolder $ Folder $ map toEntry $ Map.toList x
    elookup (p:ps) x =
        case Map.lookup p x of
            Nothing -> FLDoesNotExist
            Just (EEFile f) ->
                case ps of
                    [] -> FLFile $ bsToFile p f
                    _ -> FLDoesNotExist
            Just (EEFolder y) -> elookup ps y

toEntry :: (T.Text, EmbeddedEntry) -> FolderEntry
toEntry (name, ee) = FolderEntry
    { feName = name
    , feIsFile =
        case ee of
            EEFile{} -> True
            EEFolder{} -> False
    , feGetMetaData = return $ Just $
        case ee of
            EEFile bs -> FileMetaData
                { mdName = T.unpack name
                , mdModified = 0 -- FIXME
                , mdSize = fromIntegral $ S8.length bs
                }
            EEFolder _ -> FolderMetaData
                { mdName = T.unpack name
                }
    }

toEmbedded :: [(FilePath, S8.ByteString)] -> Embedded
toEmbedded fps =
    go texts
  where
    texts = map (\(x, y) -> (filter (not . T.null) $ toPieces x, y)) fps
    toPieces "" = []
    toPieces x =
        let (y, z) = break (== '/') x
         in T.pack y : toPieces (drop 1 z)
    go :: [(Pieces, S8.ByteString)] -> Embedded
    go orig =
        Map.fromList $ map (second go') hoisted
      where
        next = map (\(x, y) -> (head x, (tail x, y))) orig
        grouped :: [[(T.Text, (Pieces, S8.ByteString))]]
        grouped = groupBy ((==) `on` fst) $ sortBy (comparing fst) next
        hoisted :: [(T.Text, [(Pieces, S8.ByteString)])]
        hoisted = map (fst . head &&& map snd) grouped
    go' :: [(Pieces, S8.ByteString)] -> EmbeddedEntry
    go' [([], content)] = EEFile content
    go' x = EEFolder $ go $ filter (\y -> not $ null $ fst y) x

bsToFile :: T.Text -> S8.ByteString -> File
bsToFile name bs = File
    { fileGetSize = return $ S8.length bs
    , fileToResponse = \s h -> W.ResponseBuilder s h $ fromByteString bs
    , fileName = name
    }

staticApp :: StaticSettings -> W.Application
staticApp set req = do
    let pieces = W.pathInfo req
    staticAppPieces set pieces req

status304, statusNotModified :: H.Status
status304 = H.Status 304 "Not Modified"
statusNotModified = status304

staticAppPieces :: StaticSettings -> Pieces -> W.Application
staticAppPieces _ _ req
    | W.requestMethod req /= "GET" = return $ W.responseLBS
        H.status405
        [("Content-Type", "text/plain")]
        "Only GET is supported"
staticAppPieces ss@StaticSettings{} pieces req = liftIO $ do
    let cache = ssCacheSettings ss
    let indices = case ssDirListing ss of
                      StaticDirListing _ is -> is
                      ListingForbidden      -> []
    cp <- checkPieces (ssFolder ss) indices pieces cache req
    case cp of
        FileResponse file -> do
            mimetype <- ssGetMimeType ss file
            filesize <- fileGetSize file
            ch <- setCacheHeaders cache file
            let headers = ("Content-Type", mimetype)
                        : ("Content-Length", S8.pack $ show filesize)
                        : ch
            return $ fileToResponse file H.status200 headers
        NotModified ->
            return $ W.responseLBS statusNotModified
                        [ ("Content-Type", "text/plain")
                        ] "Not Modified"
        DirectoryResponse fp ->
            case ssDirListing ss of
                StaticDirListing f _ -> do
                    lbs <- f pieces fp
                    return $ W.responseLBS H.status200
                        [ ("Content-Type", "text/html; charset=utf-8")
                        ] lbs
                ListingForbidden -> return $ W.responseLBS H.status403
                        [ ("Content-Type", "text/plain")
                        ] "Directory listings disabled"
        SendContent mt lbs -> do
            -- TODO: set caching headers
            return $ W.responseLBS H.status200
                [ ("Content-Type", mt)
                  -- TODO: set Content-Length
                ] lbs
        Redirect pieces' -> do
            let loc = (ssMkRedirect ss) pieces' $ toByteString (H.encodePathSegments pieces')

            let loc' =
                    -- relativeDirFromPieces pieces = T.concat $ map (const "../") (drop 1 pieces) -- last piece is not a dir
                    -- (ssMkRedirect ss) pieces' $ encodePathInfo pieces' [] 
                    toByteString $
                    foldr mappend (H.encodePathSegments pieces') -- FIXME use Text
                    $ map (const $ copyByteString "../") $ drop 1 pieces
            return $ W.responseLBS H.status301
                [ ("Content-Type", "text/plain")
                , ("Location", loc)
                ] "Redirect"
        Forbidden -> return $ W.responseLBS H.status403
                        [ ("Content-Type", "text/plain")
                        ] "Forbidden"
        NotFound -> return $ W.responseLBS H.status404
                        [ ("Content-Type", "text/plain")
                        ] "File not found"
    where
      -- expires header: formatTime "%a, %d-%b-%Y %X GMT"
        setCacheHeaders :: CacheSettings -> File -> IO H.ResponseHeaders
        setCacheHeaders (Forever isStaticFile) fp = return $
            if isStaticFile fp (S8.drop 1 $ W.rawQueryString req)
              then [("Cache-Control", S8.append "max-age=" $ S8.pack $ show oneYear)]
              else []
        setCacheHeaders NoCache _ = return []
        setCacheHeaders (ETag ioLookup) fp = do
            etag <- ioLookup fp
            return $ case etag of
              Just hash -> [("ETag", hash)]
              Nothing -> []

{-
The problem is that the System.Directory functions are a lie: they
claim to be using String, but it's really just a raw byte sequence.
We're assuming that non-Windows systems use UTF-8 encoding (there was
a discussion regarding this, it wasn't an arbitrary decision). So we
need to encode/decode the byte sequence to/from UTF8. That's the use
case for fixPathName/unfixPathName. I'm starting to use John
Millikin's system-filepath package for some stuff with work, and might
consider migrating over to it for this in the future.
-}
fixPathName :: FilePath -> FilePath
#if defined(mingw32_HOST_OS)
fixPathName = id
#else
fixPathName = T.unpack . TE.decodeUtf8With TEE.lenientDecode . S8.pack
#endif

unfixPathName :: FilePath -> FilePath
#if defined(mingw32_HOST_OS)
unfixPathName = id
#else
unfixPathName = S8.unpack . TE.encodeUtf8 . T.pack
#endif

-- Code below taken from Happstack: http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-server/src/Happstack/Server/FileServe/BuildingBlocks.hs
defaultListing :: Listing
defaultListing pieces (Folder contents) = do
    fps' <- mapM feGetMetaData contents
    let isTop = null pieces || pieces == [""]
    let fps'' = if isTop then fps' else Just (FolderMetaData "..") : fps'
    return $ HU.renderHtml
           $ H.html $ do
             H.head $ do
                 let title = T.unpack $ T.intercalate "/" pieces
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
                                              , "h1 { width: 760px; margin: 1em auto; font-size: 1em; font-family: sans-serif }"
                                              , "img { width: 20px }"
                                              , "a { text-decoration: none }"
                                              ]
             H.body $ do
                 H.h1 $ showFolder $ map T.unpack $ filter (not . T.null) pieces
                 renderDirectoryContentsTable haskellSrc folderSrc $ catMaybes fps''
  where
    image x = T.unpack $ T.concat [(relativeDirFromPieces pieces), ".hidden/", x, ".png"]
    folderSrc = image "folder"
    haskellSrc = image "haskell"
    showName "" = "root"
    showName x = x
    showFolder [] = H.string "error: Unexpected showFolder []"
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
                   H.td (H.a ! A.href (H.stringValue $ mdName' md ++ if mdIsFile md then "" else "/")  $ H.string $ mdName' md)
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

mdName' :: MetaData -> FilePath
mdName' = fixPathName . mdName

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
