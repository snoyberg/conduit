{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Lazy.Char8 ()
import Data.ByteString (ByteString)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

main :: IO ()
main = do
    a <- getArgs
    let prefix =
            case a of
                [] -> id
                [x] -> (filter (not . null) (split x) ++)
                _ -> error "Usage: warp [root dir]"
    run 3000 $ app prefix

app :: ([String] -> [String]) -> Application
app prefix Request { requestMethod = m, pathInfo = p }
    | m == "GET" =
        case checkUnsafe $ split $ unpack $ decodeUtf8With lenientDecode p of
            Nothing -> return $ responseLBS status403 [("Content-Type", "text/plain")] "Permission Denied"
            Just pieces -> do
                let file = join $ prefix pieces
                let ext = reverse $ takeWhile (/= '.') $ reverse file
                let ct = getCT ext
                e <- doesFileExist file
                if e
                    then return $ ResponseFile status200 [("Content-Type", ct)] file
                    else return $ responseLBS status404 [("Content-Type", "text/plain")] "File not found"
    | otherwise =
        return $ responseLBS status405 [("Content-Type", "text/plain")] "Bad method"

split :: String -> [String]
split "" = []
split s =
    let (x, y) = break (== '/') s
     in x : split (drop 1 y)

checkUnsafe :: [String] -> Maybe [String]
checkUnsafe [] = Nothing
checkUnsafe [""] = Just ["index.html"]
checkUnsafe [x] = Just [x]
checkUnsafe ("..":_) = Nothing
checkUnsafe (".":rest) = checkUnsafe rest
checkUnsafe ("":rest) = checkUnsafe rest
checkUnsafe (x:rest) = ((:) x) `fmap` checkUnsafe rest

join :: [String] -> String
join [] = []
join [x] = x
join (x:xs) = x ++ '/' : join xs

getCT :: String -> ByteString
getCT "jpg" = "image/jpeg"
getCT "jpeg" = "image/jpeg"
getCT "js" = "text/javascript"
getCT "css" = "text/css"
getCT "html" = "text/html"
getCT "png" = "image/png"
getCT "gif" = "image/gif"
getCT "txt" = "text/plain"
getCT "flv" = "video/x-flv"
getCT "ogv" = "video/ogg"
getCT _ = "application/octet-stream"
