{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
import           Control.Exception                (fromException)
import           Test.Hspec

import           Control.Applicative              ((<*), (<|>))
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.List                as CL

main :: IO ()
main = hspec $ do
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
                            errorPosition pe `shouldBe` Position badLine badCol
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
                            errorPosition pe `shouldBe` Position badLine badCol
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
                            errorPosition pe `shouldBe` Position badLine badCol
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
                            errorPosition pe `shouldBe` Position badLine badCol
        it "works after new line in text" $ do
            let input = ["aaa\n", "aaa\n\n", "aaa", "aa\nb\naaaa"]
                badLine = 5
                badCol = 1
                parser = Data.Attoparsec.Text.endOfInput <|> (Data.Attoparsec.Text.notChar 'b' >> parser)
                sink = sinkParser parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Position badLine badCol
        it "works after new line in bytestring" $ do
            let input = ["aaa\n", "aaa\n\n", "aaa", "aa\nb\naaaa"]
                badLine = 5
                badCol = 1
                parser = Data.Attoparsec.ByteString.Char8.endOfInput <|> (Data.Attoparsec.ByteString.Char8.notChar 'b' >> parser)
                sink = sinkParser parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Position badLine badCol

    describe "conduitParser" $ do
        it "parses a repeated stream" $ do
            let input = ["aaa\n", "aaa\naaa\n", "aaa\n"]
                parser = Data.Attoparsec.Text.string "aaa" <* Data.Attoparsec.Text.endOfLine
                sink = conduitParserEither parser =$= CL.consume
            (Right ea) <- runExceptionT $ CL.sourceList input $$ sink
            let chk a = case a of
                          Left{} -> False
                          Right (_, xs) -> xs == "aaa"
                chkp 1 = (PositionRange (Position 1 0) (Position 2 1))
                chkp l = (PositionRange (Position l 1) (Position (l+1) 1))
            forM_ ea $ \ a -> a `shouldSatisfy` chk :: Expectation
            forM_ (zip ea [1..]) $ \ (Right (pos, _), l) -> pos `shouldBe` chkp l
            length ea `shouldBe` 4


