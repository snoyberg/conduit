{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module FullApp (fullApp) where

import Network.Wai
import Network.HTTP.Types
import Data.ByteString.Lazy.Char8 (pack)
import Database.Persist.TH
import Database.Persist.Sqlite
import System.Directory
import Control.Monad (when)
import Helper
import Text.Hamlet
import Text.Blaze.Renderer.Text (renderHtml)
import Data.Text.Lazy.Encoding (encodeUtf16LE)

mkPersist sqlSettings [persist|
Dummy
    dummy String
|]

testApp handler = do
    putStrLn "testApp called, this should happen only once per reload"
    -- Swap between the following two lines as necessary to generate errors
    exi <- doesFileExist "db"
    --let exi = True
    when exi $ removeFile "db"
    withSqlitePool "db" 10 $ \pool -> do
        flip runSqlPool pool $ runMigration $ migrate $ Dummy ""
        handler $ \req -> do
            if pathInfo req == "/favicon.ico"
                then return $ responseLBS status301 [("Location", "http://docs.yesodweb.com/favicon.ico")]
                            $ pack ""
                else do
                    print $ pathInfo req
                    x <- flip runSqlPool pool $ do
                        insert $ Dummy ""
                        count ([] :: [Filter Dummy])
                    return $ responseLBS status200
                        [("Content-Type", "text/html; charset=utf-8")] $
                        encodeUtf16LE $ renderHtml
                        $(htmlFile "hamlet/testapp.hamlet")
        putStrLn "handler completed, this should only happen at the beginning of a reload"
