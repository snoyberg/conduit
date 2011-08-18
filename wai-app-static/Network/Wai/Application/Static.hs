{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
-- | Static file serving for WAI.
module Network.Wai.Application.Static
    ( -- * WAI application
      staticApp
      -- ** Settings
    , defaultWebAppSettings
    , defaultFileServerSettings
    , StaticSettings
    , ssFolder
    , ssMkRedirect
    , ssGetMimeType
    , ssListing
    , ssIndices
    , ssMaxAge
      -- * Generic, non-WAI code
      -- ** Mime types
    , MimeType
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
      -- ** Directory listings
    , Listing
    , defaultListing
      -- ** Lookup functions
    , fileSystemLookup
    , fileSystemLookupHash
    , embeddedLookup
      -- ** Embedded
    , Embedded
    , EmbeddedEntry (..)
    , toEmbedded
      -- ** Redirecting
    , defaultMkRedirect
      -- * Other data types
    , File (..)
    , FilePath (..)
    , toFilePath
    , fromFilePath
    , MaxAge (..)
    ) where

import Prelude hiding (FilePath)
import qualified Prelude
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
import System.Posix.Types (EpochTime)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.MD5 as MD5
import Control.Monad (filterM)

import           Text.Blaze                  ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Renderer.Utf8    as HU
import qualified Text.Blaze.Html5.Attributes as A

import Blaze.ByteString.Builder (toByteString, fromByteString)

import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)

import Data.FileEmbed (embedFile)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Control.Arrow ((&&&), second)
import Data.List (groupBy, sortBy, find, foldl')
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.ByteString.Base64 as B64
import Data.Either (rights)
import Data.Maybe (isJust, fromJust)
import Network.HTTP.Date (parseHTTPDate, epochTimeToHTTPDate, formatHTTPDate)
import Data.String (IsString (..))

newtype FilePath = FilePath { unFilePath :: Text }
    deriving (Ord, Eq, Show)
instance IsString FilePath where
    fromString = toFilePath

(</>) :: FilePath -> FilePath -> FilePath
(FilePath a) </> (FilePath b) = FilePath $ T.concat [a, "/", b]

-- | A list of all possible extensions, starting from the largest.
takeExtensions :: FilePath -> [FilePath]
takeExtensions (FilePath s) =
    case T.break (== '.') s of
        (_, "") -> []
        (_, x) -> FilePath (T.drop 1 x) : takeExtensions (FilePath $ T.drop 1 x)

type MimeType = ByteString
type Extension = FilePath
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
  ( "bmp"     , "image/bmp"                         ),
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
  ( "xhtml"   , "application/xhtml+xml"             ),
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

data CheckPieces =
      -- | Just the etag hash or Nothing for no etag hash
      Redirect Pieces (Maybe ByteString)
    | Forbidden
    | NotFound
    | FileResponse File H.ResponseHeaders
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


unsafe :: FilePath -> Bool
unsafe (FilePath s)
    | T.null s = False
    | T.head s == '.' = True
    | otherwise = T.any (== '/') s

nullFilePath :: FilePath -> Bool
nullFilePath = T.null . unFilePath

stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash fp@(FilePath t)
    | T.null t || T.last t /= '/' = fp
    | otherwise = FilePath $ T.init t

type Pieces = [FilePath]

relativeDirFromPieces :: Pieces -> T.Text
relativeDirFromPieces pieces = T.concat $ map (const "../") (drop 1 pieces) -- last piece is not a dir

pathFromPieces :: FilePath -> Pieces -> FilePath
pathFromPieces = foldl' (</>)

checkSpecialDirListing :: Pieces -> Maybe CheckPieces
checkSpecialDirListing [".hidden", "folder.png"]  =
    Just $ SendContent "image/png" $ L.fromChunks [$(embedFile "folder.png")]
checkSpecialDirListing [".hidden", "haskell.png"] =
    Just $ SendContent "image/png" $ L.fromChunks [$(embedFile "haskell.png")]
checkSpecialDirListing _ =  Nothing

checkPieces :: (Pieces -> IO FileLookup) -- ^ file lookup function
            -> [FilePath]                -- ^ List of default index files. Cannot contain slashes.
            -> Pieces                    -- ^ parsed request
            -> W.Request
            -> MaxAge
            -> Bool                      -- ^ use hash?
            -> IO CheckPieces
checkPieces fileLookup indices pieces req maxAge useHash
    | any unsafe pieces = return Forbidden
    | any nullFilePath $ safeInit pieces =
        return $ Redirect (filterButLast (not . nullFilePath) pieces) Nothing
    | otherwise = do
        let (isFile, isFolder) =
                case () of
                    ()
                        | null pieces -> (True, True)
                        | nullFilePath (last pieces) -> (False, True)
                        | otherwise -> (True, False)

        fl <- fileLookup pieces
        case (fl, isFile) of
            (Nothing, _) -> return NotFound
            (Just (Right file), True)  -> handleCache file
            (Just Right{}, False) -> return $ Redirect (init pieces) Nothing
            (Just (Left folder@(Folder _ contents)), _) -> do
                case checkIndices $ map fileName $ rights contents of
                    Just index -> return $ Redirect (setLast pieces index) Nothing
                    Nothing ->
                        if isFolder
                            then return $ DirectoryResponse folder
                            else return $ Redirect (pieces ++ [""]) Nothing
  where
    headers = W.requestHeaders req
    queryString = W.queryString req

    -- HTTP caching has a cache control header that you can set an expire time for a resource.
    --   Max-Age is easiest because it is a simple number
    --   a cache-control asset will only be downloaded once (if the browser maintains its cache)
    --   and the server will never be contacted for the resource again (until it expires)
    --
    -- A second caching mechanism is ETag and last-modified
    --   this form of caching is not as good as the static- the browser can avoid downloading the file, but it always need to send a request with the etag value or the last-modified value to the server to see if its copy is up to date
    --
    -- We should set a cache control and one of ETag or last-modifed whenever possible
    --
    -- In a Yesod web application we can append an etag parameter to static assets.
    -- This signals that both a max-age and ETag header should be set
    -- if there is no etag parameter
    -- * don't set the max-age
    -- * set ETag or last-modified
    --   * ETag must be calculated ahead of time.
    --   * last-modified is just the file mtime.
    handleCache file =
      if not useHash then lastModifiedCache file
        else do
          let mGetHash  = fileGetHash file
          let etagParam = lookup "etag" queryString

          case (etagParam, mGetHash) of
            (Just mEtag, Just getHash) -> do
                hash <- getHash
                if isJust mEtag && hash == fromJust mEtag
                  then return $ FileResponse file $ ("ETag", hash):cacheControl
                  else return $ Redirect pieces (Just hash)
            -- a file used to have an etag parameter, but no longer does
            (Just _, Nothing) -> return $ Redirect pieces Nothing

            _ -> 
                case (lookup "if-none-match" headers, mGetHash) of
                    -- etag
                    (mLastHash, Just getHash) -> do
                        hash <- getHash
                        case mLastHash of
                          Just lastHash ->
                            if hash == lastHash
                                then return NotModified
                                else return $ FileResponse file $ [("ETag", hash)]
                          Nothing -> return $ FileResponse file $ [("ETag", hash)]
                    (_, Nothing) -> lastModifiedCache file

    lastModifiedCache file =
      case (lookup "if-modified-since" headers >>= parseHTTPDate, fileGetModified file) of
          (mLastSent, Just modified) -> do
            let mdate = epochTimeToHTTPDate modified in
              case mLastSent of
                Just lastSent ->
                  if lastSent == mdate
                      then return NotModified
                      else return $ FileResponse file $ [("last-modified", formatHTTPDate mdate)]
                Nothing -> return $ FileResponse file $ [("last-modified", formatHTTPDate mdate)]
          _ -> return $ FileResponse file []

    setLast :: Pieces -> FilePath -> Pieces
    setLast [] x = [x]
    setLast [""] x = [x]
    setLast (a:b) x = a : setLast b x

    checkIndices :: [FilePath] -> Maybe FilePath
    checkIndices contents = find (flip elem indices) contents

    cacheControl = case ccInt of
        Nothing -> []
        Just i  -> [("Cache-Control", S8.append "max-age=" $ S8.pack $ show i)]
      where
        ccInt =
            case maxAge of
                NoMaxAge -> Nothing
                MaxAgeSeconds i -> Just i
                MaxAgeForever -> Just oneYear
        oneYear :: Int
        oneYear = 60 * 60 * 24 * 365

type Listing = (Pieces -> Folder -> IO L.ByteString)


type FileLookup = Maybe (Either Folder File)

data Folder = Folder
    { folderName :: FilePath
    , folderContents :: [Either Folder File]
    }

data File = File
    { fileGetSize :: Int
    , fileToResponse :: H.Status -> H.ResponseHeaders -> W.Response
    , fileName :: FilePath
    , fileGetHash :: Maybe (IO ByteString)
    , fileGetModified :: Maybe EpochTime
    }

data StaticSettings = StaticSettings
    { ssFolder :: Pieces -> IO FileLookup
    , ssMkRedirect :: Pieces -> ByteString -> ByteString
    , ssGetMimeType :: File -> IO MimeType
    , ssListing :: Maybe Listing
    , ssIndices :: [T.Text] -- index.html
    , ssMaxAge :: MaxAge
    , ssUseHash :: Bool
    }

data MaxAge = NoMaxAge | MaxAgeSeconds Int | MaxAgeForever

defaultMkRedirect :: Pieces -> ByteString -> S8.ByteString
defaultMkRedirect pieces newPath
    | S8.null newPath || S8.null relDir ||
      S8.last relDir /= '/' || S8.head newPath /= '/' =
        relDir `S8.append` newPath
    | otherwise = relDir `S8.append` S8.tail newPath
  where
    relDir = TE.encodeUtf8 (relativeDirFromPieces pieces)

defaultWebAppSettings :: StaticSettings
defaultWebAppSettings = StaticSettings
    { ssFolder = fileSystemLookup "static"
    , ssMkRedirect  = defaultMkRedirect
    , ssGetMimeType = return . defaultMimeTypeByExt . fileName
    , ssMaxAge  = MaxAgeForever
    , ssListing = Nothing
    , ssIndices = []
    , ssUseHash = True
    }

defaultFileServerSettings :: StaticSettings
defaultFileServerSettings = StaticSettings
    { ssFolder = fileSystemLookup "static"
    , ssMkRedirect = defaultMkRedirect
    , ssGetMimeType = return . defaultMimeTypeByExt . fileName
    , ssMaxAge = MaxAgeSeconds $ 60 * 60
    , ssListing = Just defaultListing
    , ssIndices = ["index.html", "index.htm"]
    , ssUseHash = False
    }

fileHelper :: (FilePath -> Maybe (IO ByteString)) -- ^ hash function
           -> FilePath -> FilePath -> IO File
fileHelper hashFunc fp name = do
    fs <- getFileStatus $ fromFilePath fp
    return File
        { fileGetSize = fromIntegral $ fileSize fs
        , fileToResponse = \s h -> W.ResponseFile s h (fromFilePath fp) Nothing
        , fileName = name
        , fileGetHash = hashFunc fp
        , fileGetModified = Just $ modificationTime fs
        }

defaultFileSystemHash :: FilePath -> Maybe (IO ByteString)
defaultFileSystemHash fp = Just $ do
    -- FIXME replace lazy IO with enumerators
    -- FIXME let's use a dictionary to cache these values?
    l <- L.readFile $ fromFilePath fp
    return $ runHashL l

fileSystemLookup :: FilePath -> Pieces -> IO FileLookup
fileSystemLookup = fileSystemLookupHash defaultFileSystemHash

fileSystemLookupHash :: (FilePath -> Maybe (IO ByteString)) -- ^ hash function
                     -> FilePath -> Pieces -> IO FileLookup
fileSystemLookupHash hashFunc prefix pieces = do
    let fp = pathFromPieces prefix pieces
    fe <- doesFileExist $ fromFilePath fp
    if fe
        then fmap (Just . Right) $ fileHelper hashFunc fp $ last pieces
        else do
            de <- doesDirectoryExist $ fromFilePath fp
            if de
                then do
                    let isVisible ('.':_) = return False
                        isVisible "" = return False
                        isVisible _ = return True
                    entries <- getDirectoryContents (fromFilePath fp) >>= filterM isVisible >>= mapM (\nameRaw -> do
                        let name = toFilePath nameRaw
                        let fp' = fp </> name
                        fe' <- doesFileExist $ fromFilePath fp'
                        if fe'
                            then fmap Right $ fileHelper hashFunc fp' name
                            else return $ Left $ Folder name [])
                    return $ Just $ Left $ Folder (error "Network.Wai.Application.Static.fileSystemLookup") entries
                else return Nothing

type Embedded = Map.Map FilePath EmbeddedEntry

data EmbeddedEntry = EEFile S8.ByteString | EEFolder Embedded

embeddedLookup :: Embedded -> Pieces -> IO FileLookup
embeddedLookup root pieces =
    return $ elookup "<root>" pieces root
  where
    elookup  :: FilePath -> [FilePath] -> Embedded -> FileLookup
    elookup p [] x = Just $ Left $ Folder p $ map toEntry $ Map.toList x
    elookup p [""] x = elookup p [] x
    elookup _ (p:ps) x =
        case Map.lookup p x of
            Nothing -> Nothing
            Just (EEFile f) ->
                case ps of
                    [] -> Just $ Right $ bsToFile p f
                    _ -> Nothing
            Just (EEFolder y) -> elookup p ps y

toEntry :: (FilePath, EmbeddedEntry) -> Either Folder File
toEntry (name, EEFolder{}) = Left $ Folder name []
toEntry (name, EEFile bs) = Right $ File
    { fileGetSize = S8.length bs
    , fileToResponse = \s h -> W.ResponseBuilder s h $ fromByteString bs
    , fileName = name
    , fileGetHash = Just $ return $ runHash bs
    , fileGetModified = Nothing
    }

toEmbedded :: [(Prelude.FilePath, S8.ByteString)] -> Embedded
toEmbedded fps =
    go texts
  where
    texts = map (\(x, y) -> (filter (not . T.null . unFilePath) $ toPieces x, y)) fps
    toPieces "" = []
    toPieces x =
        let (y, z) = break (== '/') x
         in toFilePath y : toPieces (drop 1 z)
    go :: [([FilePath], S8.ByteString)] -> Embedded
    go orig =
        Map.fromList $ map (second go') hoisted
      where
        next = map (\(x, y) -> (head x, (tail x, y))) orig
        grouped :: [[(FilePath, ([FilePath], S8.ByteString))]]
        grouped = groupBy ((==) `on` fst) $ sortBy (comparing fst) next
        hoisted :: [(FilePath, [([FilePath], S8.ByteString)])]
        hoisted = map (fst . head &&& map snd) grouped
    go' :: [([FilePath], S8.ByteString)] -> EmbeddedEntry
    go' [([], content)] = EEFile content
    go' x = EEFolder $ go $ filter (\y -> not $ null $ fst y) x

bsToFile :: FilePath -> S8.ByteString -> File
bsToFile name bs = File
    { fileGetSize = S8.length bs
    , fileToResponse = \s h -> W.ResponseBuilder s h $ fromByteString bs
    , fileName = name
    , fileGetHash = Just $ return $ runHash bs
    , fileGetModified = Nothing
    }

runHash :: S8.ByteString -> S8.ByteString
runHash = B64.encode . MD5.hash

runHashL :: L.ByteString -> ByteString
runHashL = B64.encode . MD5.hashlazy

staticApp :: StaticSettings -> W.Application
staticApp set req = staticAppPieces set (map FilePath $ W.pathInfo req) req

status304, statusNotModified :: H.Status
status304 = H.Status 304 "Not Modified"
statusNotModified = status304

-- alist helper functions
replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replace k v [] = [(k,v)]
replace k v (x:xs) | fst x == k = (k,v):xs
                   | otherwise  = x:replace k v xs

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove _ [] = []
remove k (x:xs) | fst x == k = xs
                  | otherwise  = x:remove k xs


staticAppPieces :: StaticSettings -> Pieces -> W.Application
staticAppPieces _ _ req
    | W.requestMethod req /= "GET" = return $ W.responseLBS
        H.status405
        [("Content-Type", "text/plain")]
        "Only GET is supported"
staticAppPieces ss pieces req = liftIO $ do
    let indices = ssIndices ss
    case checkSpecialDirListing pieces of
         Just res ->  response res
         Nothing  ->  checkPieces (ssFolder ss) (map FilePath indices) pieces req (ssMaxAge ss) (ssUseHash ss) >>= response
  where
    response cp = case cp of
        FileResponse file ch -> do
            mimetype <- ssGetMimeType ss file
            let filesize = fileGetSize file
            let headers = ("Content-Type", mimetype)
                        : ("Content-Length", S8.pack $ show filesize)
                        : ch
            return $ fileToResponse file H.status200 headers
        NotModified ->
            return $ W.responseLBS statusNotModified
                        [ ("Content-Type", "text/plain")
                        ] "Not Modified"
        DirectoryResponse fp -> do
            case ssListing ss of
                (Just f) -> do
                    lbs <- f pieces fp
                    return $ W.responseLBS H.status200
                        [ ("Content-Type", "text/html; charset=utf-8")
                        ] lbs
                Nothing -> return $ W.responseLBS H.status403
                        [ ("Content-Type", "text/plain")
                        ] "Directory listings disabled"
        SendContent mt lbs -> do
            -- TODO: set caching headers
            return $ W.responseLBS H.status200
                [ ("Content-Type", mt)
                  -- TODO: set Content-Length
                ] lbs

        Redirect pieces' mHash -> do
            let loc = (ssMkRedirect ss) pieces' $ toByteString (H.encodePathSegments $ map unFilePath pieces')
            let qString = case mHash of
                  Just hash -> replace "etag" (Just hash) (W.queryString req)
                  Nothing   -> remove "etag" (W.queryString req)

            return $ W.responseLBS H.status301
                [ ("Content-Type", "text/plain")
                , ("Location", S8.append loc $ H.renderQuery True qString)
                ] "Redirect"
        Forbidden -> return $ W.responseLBS H.status403
                        [ ("Content-Type", "text/plain")
                        ] "Forbidden"
        NotFound -> return $ W.responseLBS H.status404
                        [ ("Content-Type", "text/plain")
                        ] "File not found"

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
toFilePath :: Prelude.FilePath -> FilePath
#if defined(mingw32_HOST_OS)
toFilePath = FilePath . T.pack
#else
toFilePath = FilePath . TE.decodeUtf8With TEE.lenientDecode . S8.pack
#endif

fromFilePath :: FilePath -> Prelude.FilePath
#if defined(mingw32_HOST_OS)
fromFilePath = T.unpack . unFilePath
#else
fromFilePath = S8.unpack . TE.encodeUtf8 . unFilePath
#endif

-- Code below taken from Happstack: http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-server/src/Happstack/Server/FileServe/BuildingBlocks.hs
defaultListing :: Listing
defaultListing pieces (Folder _ contents) = do
    let isTop = null pieces || pieces == [""]
    let fps'' :: [Either Folder File]
        fps'' = (if isTop then id else (Left (Folder ".." []) :)) contents
    return $ HU.renderHtml
           $ H.html $ do
             H.head $ do
                 let title = T.unpack $ T.intercalate "/" $ map unFilePath pieces
                 let title' = if null title then "root folder" else title
                 H.title $ H.toHtml title'
                 H.style $ H.toHtml $ unlines [ "table { margin: 0 auto; width: 760px; border-collapse: collapse; font-family: 'sans-serif'; }"
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
                 H.h1 $ showFolder $ map unFilePath $ filter (not . nullFilePath) pieces
                 renderDirectoryContentsTable haskellSrc folderSrc fps''
  where
    image x = T.unpack $ T.concat [(relativeDirFromPieces pieces), ".hidden/", x, ".png"]
    folderSrc = image "folder"
    haskellSrc = image "haskell"
    showName "" = "root"
    showName x = x
    showFolder [] = "/"
    showFolder [x] = H.toHtml $ showName x
    showFolder (x:xs) = do
        let href = concat $ replicate (length xs) "../" :: String
        H.a ! A.href (H.toValue href) $ H.toHtml $ showName x
        " / " :: H.Html
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
                             -> [Either Folder File]
                             -> H.Html
renderDirectoryContentsTable haskellSrc folderSrc fps =
           H.table $ do H.thead $ do H.th ! (A.class_ "first") $ H.img ! (A.src $ H.toValue haskellSrc)
                                     H.th "Name"
                                     H.th "Modified"
                                     H.th "Size"
                        H.tbody $ mapM_ mkRow (zip (sortBy sortMD fps) $ cycle [False, True])
    where
      sortMD :: Either Folder File -> Either Folder File -> Ordering
      sortMD Left{} Right{} = LT
      sortMD Right{} Left{} = GT
      sortMD (Left a) (Left b) = compare (folderName a) (folderName b)
      sortMD (Right a) (Right b) = compare (fileName a) (fileName b)
      mkRow :: (Either Folder File, Bool) -> H.Html
      mkRow (md, alt) =
          (if alt then (! A.class_ "alt") else id) $
          H.tr $ do
                   H.td ! A.class_ "first"
                        $ case md of
                            Left{} -> H.img ! A.src (H.toValue folderSrc)
                                            ! A.alt "Folder"
                            Right{} -> return ()
                   let name = either folderName fileName md
                   let isFile = either (const False) (const True) md
                   H.td (H.a ! A.href (H.toValue $ unFilePath name `T.append` if isFile then "" else "/") $ H.toHtml $ unFilePath name)
                   H.td ! A.class_ "date" $ H.toHtml $
                       case md of
                           Right File { fileGetModified = Just t } ->
                                   formatCalendarTime defaultTimeLocale "%d-%b-%Y %X" t
                           _ -> ""
                   H.td ! A.class_ "size" $ H.toHtml $
                       case md of
                           Right File { fileGetSize = s } -> prettyShow s
                           Left{} -> ""
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

{-
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
    let fpPretty = T.pack $ fixPathName fp
    if fe
        then do
            fs <- getFileStatus fp'
            let modTime = modificationTime fs
            let count = fileSize fs
            return $ Just $ FileMetaData fpPretty (Just modTime) count
        else do
            de <- doesDirectoryExist fp'
            return $ if de then Just (FolderMetaData fpPretty) else Nothing
-}
