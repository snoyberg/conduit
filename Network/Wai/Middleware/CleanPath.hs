module Network.Wai.Middleware.CleanPath (cleanPath, splitPath) where

import Network.Wai
import Web.Encodings
import qualified Data.ByteString.Char8 as B

-- | Performs redirects as per 'splitPath'.
cleanPath :: ([B.ByteString] -> Request -> IO Response)
          -> Request
          -> IO Response
cleanPath app env =
    case splitPath $ pathInfo env of
        Left p -> do
            -- include the query string if present
            let suffix = case B.uncons $ queryString env of
                            Nothing -> B.empty
                            Just ('?', _) -> queryString env
                            _ -> B.cons '?' $ queryString env
            return $ Response Status303 [(Location, B.append p suffix)]
                   $ Right emptyEnum
        Right pieces -> app pieces env

emptyEnum :: Enumerator
emptyEnum = Enumerator $ \_ -> return . Right

-- | Given a certain requested path, return either a corrected path
-- to redirect to or the tokenized path.
--
-- This code corrects for the following issues:
--
-- * It is missing a trailing slash, and there is no period after the
-- last slash.
--
-- * There are any doubled slashes.
splitPath :: B.ByteString -> Either B.ByteString [B.ByteString]
splitPath s =
    let corrected = B.pack $ ats $ rds $ B.unpack s
     in if corrected == s
            then Right $ map decodeUrl
                       $ filter (not . B.null)
                       $ B.split '/' s
            else Left corrected

-- | Remove double slashes
rds :: String -> String
rds [] = []
rds [x] = [x]
rds (a:b:c)
    | a == '/' && b == '/' = rds (b:c)
    | otherwise = a : rds (b:c)

-- | Add a trailing slash if it is missing. Empty string is left alone.
ats :: String -> String
ats [] = []
ats s =
    if last s == '/' || dbs (reverse s)
        then s
        else s ++ "/"

-- | Is there a period before a slash here?
dbs :: String -> Bool
dbs ('/':_) = False
dbs ('.':_) = True
dbs (_:x) = dbs x
dbs [] = False
