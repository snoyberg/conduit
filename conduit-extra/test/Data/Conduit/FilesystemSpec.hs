module Data.Conduit.FilesystemSpec (spec) where

import Test.Hspec
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem
import Data.List (sort, isSuffixOf)
import System.FilePath ((</>))

spec :: Spec
spec = describe "Data.Conduit.Filesystem" $ do
    it "sourceDirectory" $ do
        res <- runConduitRes
             $ sourceDirectory ("test" </> "filesystem")
            .| CL.filter (not . (".swp" `isSuffixOf`))
            .| CL.consume
        sort res `shouldBe`
            [ "test" </> "filesystem" </> "bar.txt"
            , "test" </> "filesystem" </> "baz.txt"
            , "test" </> "filesystem" </> "bin"
            , "test" </> "filesystem" </> "foo.txt"
            ]
    it "sourceDirectoryDeep" $ do
        res1 <- runConduitRes
              $ sourceDirectoryDeep False ("test" </> "filesystem")
             .| CL.filter (not . (".swp" `isSuffixOf`))
             .| CL.consume
        res2 <- runConduitRes
              $ sourceDirectoryDeep True ("test" </> "filesystem")
             .| CL.filter (not . (".swp" `isSuffixOf`))
             .| CL.consume
        sort res1 `shouldBe`
            [ "test" </> "filesystem" </> "bar.txt"
            , "test" </> "filesystem" </> "baz.txt"
            , "test" </> "filesystem" </> "bin" </> "bin.txt"
            , "test" </> "filesystem" </> "foo.txt"
            ]
        sort res1 `shouldBe` sort res2
