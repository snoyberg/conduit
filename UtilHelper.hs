module UtilHelper where

import Data.Maybe (mapMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

mimeTypes' :: IO [(String, String)]
mimeTypes' = do
    s <- readFile "mime.types"
    let go ('#':_) = Nothing
        go x =
            case words x of
                [a, b] -> Just (b, a)
                _ -> Nothing
    let pairs = mapMaybe go $ lines s
    return $ sortBy (comparing fst) pairs
