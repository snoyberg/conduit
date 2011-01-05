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
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp.Util (mimeTypes, getFileSize)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as S8

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
                e <- liftIO $ doesFileExist file
                if e
                    then do
                        size <- liftIO $ getFileSize file
                        return $ ResponseFile status200
                            [ ("Content-Type", ct)
                            , ("Content-Length", S8.pack $ show size)
                            ] file
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
getCT s =
    case Map.lookup (S8.pack s) mimeTypes of
        Just ct -> ct
        Nothing -> "application/octet-stream"
