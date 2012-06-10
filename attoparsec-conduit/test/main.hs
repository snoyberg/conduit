{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit
import Control.Exception (fromException)

import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.ByteString.Char8
import Control.Applicative ((<|>))
import Control.Monad.Trans.Resource

main :: IO ()
main = hspecX $ do
    describe "error position" $ do
        it "works for text" $ do
            let input = ["aaa\na", "aaa\n\n", "aaa", "aab\n\naaaa"]
                badLine = 4
                badCol = 6
                parser = Data.Attoparsec.Text.endOfInput <|> (Data.Attoparsec.Text.notChar 'b' >> parser)
                sink = sinkParser parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe @?= Position badLine badCol
        it "works for bytestring" $ do
            let input = ["aaa\na", "aaa\n\n", "aaa", "aab\n\naaaa"]
                badLine = 4
                badCol = 6
                parser = Data.Attoparsec.ByteString.Char8.endOfInput <|> (Data.Attoparsec.ByteString.Char8.notChar 'b' >> parser)
                sink = sinkParser parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe @?= Position badLine badCol
        it "works in last chunk" $ do
            let input = ["aaa\na", "aaa\n\n", "aaa", "aab\n\naaaa"]
                badLine = 6
                badCol = 5
                parser = Data.Attoparsec.Text.char 'c' <|> (Data.Attoparsec.Text.anyChar >> parser)
                sink = sinkParser parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe @?= Position badLine badCol
        it "works in last chunk" $ do
            let input = ["aaa\na", "aaa\n\n", "aaa", "aa\n\naaaab"]
                badLine = 6
                badCol = 6
                parser = Data.Attoparsec.Text.string "bc" <|> (Data.Attoparsec.Text.anyChar >> parser)
                sink = sinkParser parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe @?= Position badLine badCol
