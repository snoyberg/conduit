{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Handler.DevelServer (runQuit)
import System.Console.CmdArgs
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Text.Lazy.IO as TIO
import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)

data Devel = Devel
    { port :: Int
    , moduleName :: String
    , function :: String
    , yesod :: Bool
    }
    deriving (Show, Data, Typeable)

main :: IO ()
main = do
    Devel p m f y <- cmdArgs Devel
        { port = 3000 &= argPos 0 &= typ "PORT"
        , moduleName = "" &= argPos 1 &= typ "MODULE"
        , function = "" &= argPos 2 &= typ "FUNCTION"
        , yesod = False
        } &= summary "WAI development web server"
    runQuit p m f $ if y then determineHamletDeps else (const $ return [])

data TempType = Hamlet | Cassius | Lucius | Julius | Widget
    deriving Show

determineHamletDeps :: FilePath -> IO [FilePath]
determineHamletDeps x = do
    y <- TIO.readFile x
    let z = A.parse (A.many $ (parser <|> (A.anyChar >> return Nothing))) y
    case z of
        A.Fail{} -> return []
        A.Done _ r -> return $ mapMaybe go r
  where
    go (Just (Hamlet, f)) = Just $ "hamlet/" ++ f ++ ".hamlet"
    go (Just (Widget, f)) = Just $ "hamlet/" ++ f ++ ".hamlet"
    go _ = Nothing
    parser = do
        ty <- (A.string "$(hamletFile " >> return Hamlet)
           <|> (A.string "$(cassiusFile " >> return Cassius)
           <|> (A.string "$(luciusFile " >> return Lucius)
           <|> (A.string "$(juliusFile " >> return Julius)
           <|> (A.string "$(widgetFile " >> return Widget)
           <|> (A.string "$(Settings.hamletFile " >> return Hamlet)
           <|> (A.string "$(Settings.cassiusFile " >> return Cassius)
           <|> (A.string "$(Settings.luciusFile " >> return Lucius)
           <|> (A.string "$(Settings.juliusFile " >> return Julius)
           <|> (A.string "$(Settings.widgetFile " >> return Widget)
        A.skipWhile isSpace
        _ <- A.char '"'
        y <- A.many1 $ A.satisfy (/= '"')
        _ <- A.char '"'
        A.skipWhile isSpace
        _ <- A.char ')'
        return $ Just (ty, y)
