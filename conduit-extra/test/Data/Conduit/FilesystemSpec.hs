module Data.Conduit.FilesystemSpec (spec) where

import Test.Hspec
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem
import Data.List (sort, isSuffixOf)
import Control.Monad.Trans.Resource (runResourceT)

spec :: Spec
spec = describe "Data.Conduit.Filesystem" $ do
    it "sourceDirectory" $ do
        res <- runResourceT
             $ sourceDirectory "test/filesystem"
             $$ CL.filter (not . (".swp" `isSuffixOf`))
             =$ CL.consume
        sort res `shouldBe`
            [ "test/filesystem/bar.txt"
            , "test/filesystem/baz.txt"
            , "test/filesystem/bin"
            , "test/filesystem/foo.txt"
            ]
    it "sourceDirectoryDeep" $ do
        res1 <- runResourceT
              $ sourceDirectoryDeep False "test/filesystem"
              $$ CL.filter (not . (".swp" `isSuffixOf`))
              =$ CL.consume
        res2 <- runResourceT
              $ sourceDirectoryDeep True "test/filesystem"
              $$ CL.filter (not . (".swp" `isSuffixOf`))
              =$ CL.consume
        sort res1 `shouldBe`
            [ "test/filesystem/bar.txt"
            , "test/filesystem/baz.txt"
            , "test/filesystem/bin/bin.txt"
            , "test/filesystem/foo.txt"
            ]
        sort res1 `shouldBe` sort res2
