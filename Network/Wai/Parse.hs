{-# LANGUAGE CPP #-}
-- | Some helpers for parsing data out of a raw WAI 'Request'.

module Network.Wai.Parse
    ( parseQueryString
    , parseCookies
    , parseHttpAccept
    , parseRequestBody
    , Sink (..)
    , lbsSink
    , tempFileSink
    , FileInfo (..)
#if TEST
    , Bound (..)
    , findBound
    , sinkTillBound
#endif
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.Word (Word8)
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Function (on)
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO (hClose, openBinaryTempFile, Handle)
import Network.Wai

uncons :: S.ByteString -> Maybe (Word8, S.ByteString)
uncons s
    | S.null s = Nothing
    | otherwise = Just (S.head s, S.tail s)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

-- | Split out the query string into a list of keys and values. A few
-- importants points:
--
-- * There is no way to distinguish between a parameter with no value and a
-- parameter with an empty value. Eg, "foo=" and "foo" are the same.
--
-- * The result returned is still bytestrings, since we perform no character
-- decoding here. Most likely, you will want to use UTF-8 decoding, but this is
-- left to the user of the library.
--
-- * Percent decoding errors are ignored. In particular, "%Q" will be output as
-- "%Q".
parseQueryString :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseQueryString q | S.null q = []
parseQueryString q =
    let (x, xs) = breakDiscard 38 q -- ampersand
     in parsePair x : parseQueryString xs
  where
    parsePair x =
        let (k, v) = breakDiscard 61 x -- equal sign
         in (decode k, decode v)
    decode x = fst $ S.unfoldrN (S.length x) go x
    go bs =
        case uncons bs of
            Nothing -> Nothing
            Just (43, ws) -> Just (32, ws) -- plus to space
            Just (37, ws) -> Just $ fromMaybe (37, ws) $ do -- percent
                (x, xs) <- uncons ws
                x' <- hexVal x
                (y, ys) <- uncons xs
                y' <- hexVal y
                Just $ (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

-- | Decode the value of an HTTP_COOKIE header into key/value pairs.
parseCookies :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseCookies s
  | S.null s = []
  | otherwise =
    let (first, rest) = breakDiscard 59 s -- semicolon
     in parseCookie first : parseCookies rest

parseCookie :: S.ByteString -> (S.ByteString, S.ByteString)
parseCookie s =
    let (key, value) = breakDiscard 61 s -- equals sign
        key' = S.dropWhile (== 32) key -- space
     in (key', value)

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: S.ByteString -> [S.ByteString]
parseHttpAccept = map fst
                . sortBy (rcompare `on` snd)
                . map grabQ
                . S.split 44 -- comma
  where
    rcompare x y = compare y x
    grabQ s =
        let (s', q) = breakDiscard 59 s -- semicolon
            (_, q') = breakDiscard 61 q -- equals sign
         in (trimWhite s', readQ $ trimWhite q')
    readQ s = case reads $ S8.unpack s of
                (x, _):_ -> x
                _ -> 1.0
    trimWhite = S.dropWhile (== 32) -- space

-- | A destination for data, the opposite of a 'Source'.
data Sink x y = Sink
    { sinkInit :: IO x
    , sinkAppend :: x -> S.ByteString -> IO x
    , sinkClose :: x -> IO y
    , sinkFinalize :: y -> IO ()
    }

lbsSink :: Sink ([S.ByteString] -> [S.ByteString]) L.ByteString
lbsSink = Sink
    { sinkInit = return id
    , sinkAppend = \front bs -> return $ front . (:) bs
    , sinkClose = \front -> return $ L.fromChunks $ front []
    , sinkFinalize = \_ -> return ()
    }

tempFileSink :: Sink (FilePath, Handle) FilePath
tempFileSink = Sink
    { sinkInit = do
        tempDir <- getTemporaryDirectory
        openBinaryTempFile tempDir "webenc.buf"
    , sinkAppend = \(fp, h) bs -> S.hPut h bs >> return (fp, h)
    , sinkClose = \(fp, h) -> do
        hClose h
        return fp
    , sinkFinalize = \fp -> removeFile fp
    }

-- | Information on an uploaded file.
data FileInfo c = FileInfo
    { fileName :: S.ByteString
    , fileContentType :: S.ByteString
    , fileContent :: c
    }
    deriving (Eq, Show)

type Param = (S.ByteString, S.ByteString)
type File y = (S.ByteString, FileInfo y)

parseRequestBody :: Sink x y
                 -> Request
                 -> IO ([Param], [File y])
parseRequestBody sink req = do
    let ctype = do
          ctype' <- lookup ReqContentType $ requestHeaders req
          if urlenc `S.isPrefixOf` ctype'
              then Just Nothing
              else if formBound `S.isPrefixOf` ctype'
                      then Just $ Just $ S.drop (S.length formBound) ctype'
                      else Nothing
    case ctype of
        Nothing -> return ([], [])
        Just Nothing -> do -- url-encoded
            -- NOTE: in general, url-encoded data will be in a single chunk.
            -- Therefore, I'm optimizing for the usual case by sticking with
            -- strict byte strings here.
            bs <- sourceToBs $ requestBody req
            return (parseQueryString bs, [])
        Just (Just bound) -> -- multi-part
            parsePieces sink bound (S.empty, Just $ requestBody req) True
  where
    urlenc = S8.pack "application/x-www-form-urlencoded"
    formBound = S8.pack "multipart/form-data; boundary="

sourceToBs :: Source -> IO S.ByteString
sourceToBs = fmap S.concat . go id
  where
    go front (Source src) = do
        res <- src
        case res of
            Nothing -> return $ front []
            Just (bs, src') -> go (front . (:) bs) src'

type Source' = (S.ByteString, Maybe Source)

takeLine :: Source' -> IO (Maybe (S.ByteString, Source'))
takeLine (s, msrc)
    | S.null s = case msrc of
                    Nothing -> return Nothing
                    Just (Source src) -> do
                        res <- src
                        case res of
                            Nothing -> return Nothing
                            Just (x, y) -> takeLine (x, Just y)
takeLine (s, msrc) =
    let (x, y) = S.break (== 10) s -- newline
     in if S.null y
            then do
                case msrc of
                    Nothing -> return $ Just (x, (y, msrc))
                    Just (Source src) -> do
                        res <- src
                        case res of
                            Nothing -> return $ Just (x, (y, Nothing))
                            Just (s', src') ->
                                takeLine (s `S.append` s', Just src')
            else return $ Just (killCarriage x, (S.drop 1 y, msrc))
  where
    killCarriage bs
      | S.null bs = bs
      | S.last bs == 13 = S.init bs -- carriage return
      | otherwise = bs

takeLines :: Source' -> IO (Maybe ([S.ByteString], Source'))
takeLines src = do
    res <- takeLine src
    case res of
        Nothing -> return Nothing
        Just (l, src') ->
            if S.null l
                then return $ Just ([], src')
                else do
                    res' <- takeLines src'
                    case res' of
                        Nothing -> return $ Just ([l], src')
                        Just (ls, src'') -> return $ Just (l : ls, src'')

parsePieces :: Sink x y -> S.ByteString -> Source' -> Bool
            -> IO ([Param], [File y])
parsePieces sink bound src isFirst = do
    print ("bound", bound)
    src' <- if isFirst
                then do
                    res <- takeLine src -- first line should be bound
                    case res of
                        Nothing -> return (S.empty, Nothing)
                        Just (_, x) -> return x
                else return src
    res <- takeLines src'
    case res of
        Nothing -> return ([], [])
        Just (ls, src'') -> do
            let ls' = map parsePair ls
            case (lookup contDisp ls', lookup contType ls') of
                (Just cd, Just ct) -> do
                    return ([], []) -- FIXME
                _ -> do
                    (isEnd, src''') <- dropTillBound bound src''
                    if isEnd
                        then return ([], [])
                        else parsePieces sink bound src''' False
  where
    contDisp = S8.pack "Content-Disposition"
    contType = S8.pack "Content-Type"
    parsePair s =
        let (x, y) = breakDiscard 58 s -- colon
         in (x, S.dropWhile (== 32) y) -- space

dropTillBound bound (bs, msrc) = error "FIXME dropTillBound"

data Bound = FoundBound S.ByteString S.ByteString
           | NoBound
           | PartialBound
    deriving (Eq, Show)

findBound :: S.ByteString -> S.ByteString -> Bound
findBound b bs = go [0..S.length bs - 1]
  where
    go [] = NoBound
    go (i:is)
        | mismatch [0..S.length b - 1] [i..S.length bs - 1] = go is
        | otherwise =
            let endI = i + S.length b
             in if endI > S.length bs
                    then PartialBound
                    else FoundBound (S.take i bs) (S.drop endI bs)
    mismatch [] _ = False
    mismatch _ [] = False
    mismatch (x:xs) (y:ys)
        | S.index b x == S.index bs y = mismatch xs ys
        | otherwise = True

sinkTillBound :: S.ByteString
              -> Source'
              -> (x -> S.ByteString -> IO x)
              -> x
              -> IO (x, Bool, Source')
sinkTillBound bound (bs, msrc) iter seed = do
    case findBound bound bs of
        NoBound -> do
            seed' <- iter seed bs
            case msrc of
                Nothing -> return (seed', False, (S.empty, Nothing))
                Just (Source src) -> do
                    res <- src
                    case res of
                        Nothing -> return (seed', False, (S.empty, Nothing))
                        Just (bs', src') ->
                            sinkTillBound bound (bs', Just src') iter seed'
        FoundBound before after -> do
            seed' <- iter seed before
            return (seed', True, (after, msrc))
        PartialBound -> do
            -- not so efficient, but hopefully the unusual case
            case msrc of
                Nothing -> do
                    seed' <- iter seed bs
                    return (seed', False, (S.empty, Nothing))
                Just (Source src) -> do
                    res <- src
                    case res of
                        Nothing -> do
                            seed' <- iter seed bs
                            return (seed', False, (S.empty, Nothing))
                        Just (bs', src') -> do
                            let bs'' = bs `S.append` bs'
                            sinkTillBound bound (bs'', Just src') iter seed
