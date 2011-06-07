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
      -- should be moved to common helper
    , unfixPathName
      -- new stuff to be sorted
    , fileSystemLookup
    , embeddedLookup
    , defaultMkRedirect
    , File (..)
    , toEmbedded
    , StaticDirListing (..)
    , MaxAge (..)
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
import qualified Crypto.Hash.MD5 as MD5
import Control.Monad (filterM)

import           Text.Blaze                  ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Renderer.Utf8    as HU
import qualified Text.Blaze.Html5.Attributes as A

import Blaze.ByteString.Builder (toByteString, copyByteString, fromByteString)
import Data.Monoid (mappend)

import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)

import Data.FileEmbed (embedFile)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Control.Arrow ((&&&), second)
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.ByteString.Base64 as B64
import Data.Either (rights)

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


unsafe :: T.Text -> Bool
unsafe s | T.null s = False
         | T.head s == '.' = True
         | otherwise = T.any (== '/') s

stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash "/" = ""
stripTrailingSlash "" = ""
stripTrailingSlash (x:xs) = x : stripTrailingSlash xs

data Piece = Piece
    { pieceRaw :: FilePath
    , piecePretty :: T.Text
    }
    deriving (Eq, Ord)
type Pieces = [Piece]

relativeDirFromPieces :: Pieces -> T.Text
relativeDirFromPieces pieces = T.concat $ map (const "../") (drop 1 pieces) -- last piece is not a dir

pathFromPieces :: FilePath -> Pieces -> FilePath
pathFromPieces prefix pieces =
        concat $ prefix : map ((:) '/') (map pieceRaw pieces)

checkPieces :: (Pieces -> IO FileLookup) -- ^ file lookup function
            -> [T.Text]                  -- ^ List of default index files. Cannot contain slashes.
            -> Pieces                    -- ^ parsed request
            -> W.Request
            -> StaticSettings
            -> IO CheckPieces
checkPieces _ _ [Piece _ ".hidden", Piece _ "folder.png"] _ _ =
    return $ SendContent "image/png" $ L.fromChunks [$(embedFile "folder.png")]
checkPieces _ _ [Piece _ ".hidden", Piece _ "haskell.png"] _ _ =
    return $ SendContent "image/png" $ L.fromChunks [$(embedFile "haskell.png")]
checkPieces fileLookup indices pieces req ss
    | any (unsafe . piecePretty) pieces = return Forbidden
    | any (T.null . piecePretty) $ safeInit pieces =
        return $ Redirect $ filterButLast (not . T.null . piecePretty) pieces
    | otherwise = do
        let (isFile, isFolder) =
                case () of
                    ()
                        | null pieces -> (True, True)
                        | T.null $ piecePretty (last pieces) -> (False, True)
                        | otherwise -> (True, False)

        fl <- fileLookup pieces
        case (fl, isFile) of
            (FLDoesNotExist, _) -> return NotFound
            (FLFile file, True)  -> handleCache file
            (FLFile{}, False) -> return $ Redirect $ init pieces
            (FLFolder folder@(Folder _ contents), _) -> do
                case checkIndices $ map fileName $ rights contents of
                    Just index -> return $ Redirect $ setLast pieces $ Piece (T.unpack index) index
                    Nothing ->
                        if isFolder
                            then return $ DirectoryResponse folder
                            else return $ Redirect $ pieces ++ [Piece "" ""]
  where
    headers = W.requestHeaders req
    handleCache file =
        case (lookup "if-none-match" headers, fileGetHash file) of
            (Just lastHash, Just getHash) -> do
                hash <- getHash
                if hash == lastHash
                    then return NotModified
                    else return $ FileResponse file $ cc [("ETag", hash)]
            _ ->
                case (lookup "if-modified-since" headers >>= parseDate, fileGetModified file) of
                    (Just lastSent, Just modified) -> do
                        if lastSent >= modified
                            then return NotModified
                            else respond file
                    _ -> respond file

    respond file = do
        mhash <- maybe (return Nothing) (fmap Just) $ fileGetHash file
        let hash =
                case mhash of
                    Just h -> (:) ("ETag", h)
                    Nothing -> id
        return $ FileResponse file $ hash $ cc []

    ccInt =
        case ssMaxAge ss of
            NoMaxAge -> Nothing
            MaxAgeSeconds i -> Just i
            MaxAgeForever -> Just oneYear
    cc =
        case ccInt of
            Nothing -> id
            Just i -> (:) ("Cache-Control", S8.append "max-age" $ S8.pack $ show i)

    setLast [] x = [x]
    setLast [Piece "" ""] x = [x]
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

data StaticDirListing = StaticDirListing {
    ssListing :: Maybe Listing
  , ssIndices :: [T.Text]
}

defaultDirListing :: StaticDirListing
defaultDirListing = StaticDirListing (Just defaultListing) []

oneYear :: Int
oneYear = 60 * 60 * 24 * 365

data FileLookup = FLFolder Folder | FLFile File | FLDoesNotExist -- FIXME remove

data Folder = Folder
    { folderName :: T.Text
    , folderContents :: [Either Folder File]
    }

data FolderEntry = FolderEntry -- FIXME remove
    { feName :: T.Text
    , feIsFile :: Bool
    --, feGetMetaData :: IO (Maybe MetaData)
    }

data File = File
    { fileGetSize :: Int
    , fileToResponse :: H.Status -> H.ResponseHeaders -> W.Response
    , fileName :: T.Text
    , fileGetHash :: Maybe (IO ByteString)
    , fileGetModified :: Maybe EpochTime
    }

parseDate :: ByteString -> Maybe EpochTime
parseDate = error "parseDate"

data StaticSettings = StaticSettings
    { ssFolder :: Pieces -> IO FileLookup
    , ssMkRedirect :: Pieces -> ByteString -> ByteString
    , ssGetMimeType :: File -> IO MimeType
    , ssDirListing :: StaticDirListing
    , ssMaxAge :: MaxAge
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

defaultStaticSettings :: StaticSettings
defaultStaticSettings = StaticSettings
    { ssFolder = fileSystemLookup "static"
    , ssMkRedirect = defaultMkRedirect
    , ssGetMimeType = return . defaultMimeTypeByExt . T.unpack . fileName
    , ssDirListing = defaultDirListing
    , ssMaxAge = MaxAgeSeconds $ 60 * 60
    }

fileSystemLookup :: FilePath -> Pieces -> IO FileLookup
fileSystemLookup prefix pieces = do
    let fp = pathFromPieces prefix pieces
    fe <- doesFileExist fp
    if fe
        then do
            fs <- getFileStatus fp
            return $ FLFile File
                { fileGetSize = fromIntegral $ fileSize fs
                , fileToResponse = \s h -> W.ResponseFile s h fp Nothing
                , fileName = piecePretty $ last pieces
                , fileGetHash = Just $ do
                    -- FIXME replace lazy IO with enumerators
                    -- FIXME let's use a dictionary to cache these values?
                    l <- L.readFile fp
                    return $ runHashL l
                , fileGetModified = Just $ modificationTime fs
                }
        else do
            de <- doesDirectoryExist fp
            if de
                then do
                    let isVisible ('.':_) = return False
                        isVisible "" = return False
                        isVisible _ = return True
                    entries <- getDirectoryContents fp >>= filterM isVisible >>= (mapM $ \name -> do
                        let name' = T.pack $ fixPathName name
                        let fp' = fp ++ '/' : name
                        fe' <- doesFileExist fp'
                        if fe'
                            then do
                                fs <- getFileStatus fp'
                                return $ Right File
                                    { fileGetSize = fromIntegral $ fileSize fs
                                    , fileToResponse = \s h -> W.ResponseFile s h fp' Nothing
                                    , fileName = name'
                                    , fileGetHash = Just $ do
                                        -- FIXME replace lazy IO with enumerators
                                        -- FIXME let's use a dictionary to cache these values?
                                        l <- L.readFile fp'
                                        return $ runHashL l
                                    , fileGetModified = Just $ modificationTime fs
                                    }
                            else return $ Left $ Folder name' [])
                    return $ FLFolder $ Folder (error "413") entries
                else return FLDoesNotExist

type Embedded = Map.Map T.Text EmbeddedEntry

data EmbeddedEntry = EEFile S8.ByteString | EEFolder Embedded

embeddedLookup :: Embedded -> Pieces -> IO FileLookup
embeddedLookup root pieces =
    return $ elookup "<root>" (map piecePretty pieces) root
  where
    elookup  :: T.Text -> [T.Text] -> Embedded -> FileLookup
    elookup p [] x = FLFolder $ Folder p $ map toEntry $ Map.toList x
    elookup p [""] x = elookup p [] x
    elookup _ (p:ps) x =
        case Map.lookup p x of
            Nothing -> FLDoesNotExist
            Just (EEFile f) ->
                case ps of
                    [] -> FLFile $ bsToFile p f
                    _ -> FLDoesNotExist
            Just (EEFolder y) -> elookup p ps y

toEntry :: (T.Text, EmbeddedEntry) -> Either Folder File
toEntry (name, EEFolder e) = Left $ Folder name (error "toEntry")
toEntry (name, EEFile bs) = Right $ File
    { fileGetSize = S8.length bs
    , fileToResponse = \s h -> W.ResponseBuilder s h $ fromByteString bs
    , fileName = name
    , fileGetHash = Just $ return $ runHash bs
    , fileGetModified = Nothing
    }

toEmbedded :: [(FilePath, S8.ByteString)] -> Embedded
toEmbedded fps =
    go texts
  where
    texts = map (\(x, y) -> (filter (not . T.null) $ toPieces x, y)) fps
    toPiece fp = T.pack $ fixPathName fp
    toPieces "" = []
    toPieces x =
        let (y, z) = break (== '/') x
         in toPiece y : toPieces (drop 1 z)
    go :: [([T.Text], S8.ByteString)] -> Embedded
    go orig =
        Map.fromList $ map (second go') hoisted
      where
        next = map (\(x, y) -> (head x, (tail x, y))) orig
        grouped :: [[(T.Text, ([T.Text], S8.ByteString))]]
        grouped = groupBy ((==) `on` fst) $ sortBy (comparing fst) next
        hoisted :: [(T.Text, [([T.Text], S8.ByteString)])]
        hoisted = map (fst . head &&& map snd) grouped
    go' :: [([T.Text], S8.ByteString)] -> EmbeddedEntry
    go' [([], content)] = EEFile content
    go' x = EEFolder $ go $ filter (\y -> not $ null $ fst y) x

bsToFile :: T.Text -> S8.ByteString -> File
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
staticApp set req =
    staticAppPieces set (map toPiece $ W.pathInfo req) req
  where
    toPiece t = Piece
        { piecePretty = t
        , pieceRaw = unfixPathName $ T.unpack t
        }

status304, statusNotModified :: H.Status
status304 = H.Status 304 "Not Modified"
statusNotModified = status304

staticAppPieces :: StaticSettings -> Pieces -> W.Application
staticAppPieces _ _ req
    | W.requestMethod req /= "GET" = return $ W.responseLBS
        H.status405
        [("Content-Type", "text/plain")]
        "Only GET is supported"
staticAppPieces ss pieces req = liftIO $ do
    let indices = case ssDirListing ss of
                      StaticDirListing _ is -> is
    cp <- checkPieces (ssFolder ss) indices pieces req ss
    case cp of
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
        DirectoryResponse fp ->
            case ssDirListing ss of
                StaticDirListing (Just f) _ -> do
                    lbs <- f pieces fp
                    return $ W.responseLBS H.status200
                        [ ("Content-Type", "text/html; charset=utf-8")
                        ] lbs
                StaticDirListing Nothing _ -> return $ W.responseLBS H.status403
                        [ ("Content-Type", "text/plain")
                        ] "Directory listings disabled"
        SendContent mt lbs -> do
            -- TODO: set caching headers
            return $ W.responseLBS H.status200
                [ ("Content-Type", mt)
                  -- TODO: set Content-Length
                ] lbs
        Redirect pieces' -> do
            let loc = (ssMkRedirect ss) pieces' $ toByteString (H.encodePathSegments $ map piecePretty pieces')

            {- FIXME seems unnecessary
            let loc' =
                    -- relativeDirFromPieces pieces = T.concat $ map (const "../") (drop 1 pieces) -- last piece is not a dir
                    -- (ssMkRedirect ss) pieces' $ encodePathInfo pieces' [] 
                    toByteString $
                    foldr mappend (H.encodePathSegments $ map piecePretty pieces') -- FIXME is this correct?
                    $ map (const $ copyByteString "../") $ drop 1 pieces
            -}
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
        {-
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
        -}

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
defaultListing pieces (Folder _ contents) = do
    let isTop = null pieces || pieces == [Piece "" ""]
    let fps'' :: [Either Folder File]
        fps'' = (if isTop then id else (Left (Folder ".." []) :)) contents
    return $ HU.renderHtml
           $ H.html $ do
             H.head $ do
                 let title = T.unpack $ T.intercalate "/" $ map piecePretty pieces
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
                 H.h1 $ showFolder $ map (T.unpack . piecePretty) $ filter (not . T.null . piecePretty) pieces -- FIXME don't unpack
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
        " / "
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
                   H.td (H.a ! A.href (H.toValue $ name `T.append` if isFile then "" else "/") $ H.toHtml name)
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
