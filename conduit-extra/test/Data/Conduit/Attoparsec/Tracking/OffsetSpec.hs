{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Data.Conduit.Attoparsec.Tracking.OffsetSpec (spec) where
import           Control.Exception                (fromException)
import           Test.Hspec

import           Control.Applicative              ((<|>))
import           Control.Monad
import           Control.Monad.Trans.Resource (runExceptionT)
import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text
import           Data.Conduit
import           Data.Conduit.Attoparsec.Tracking
import           Data.Conduit.Attoparsec.Tracking.Offset
import qualified Data.Conduit.List                as CL

spec :: Spec
spec = describe "Data.Conduit.Attoparsec.Tracking.OffsetSpec" $ do
    describe "error position" $ do
        it "works for text" $ do
            let input = ["aaa\na", "aaa\n\n", "aaa", "aab\n\naaaa"]
                badOffset = 15
                parser = Data.Attoparsec.Text.endOfInput <|> (Data.Attoparsec.Text.notChar 'b' >> parser)
                sink = sinkParser (Offset 0) parser
                sink' = sinkParserEither (Offset 0) parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Offset badOffset
            ea' <- CL.sourceList input $$ sink'
            case ea' of
                Left pe ->
                    errorPosition pe `shouldBe` Offset badOffset
        it "works for bytestring" $ do
            let input = ["aaa\na", "aaa\n\n", "aaa", "aab\n\naaaa"]
                badOffset = 15
                parser = Data.Attoparsec.ByteString.Char8.endOfInput <|> (Data.Attoparsec.ByteString.Char8.notChar 'b' >> parser)
                sink = sinkParser (Offset 0) parser
                sink' = sinkParserEither (Offset 0) parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Offset badOffset
            ea' <- CL.sourceList input $$ sink'
            case ea' of
                Left pe ->
                    errorPosition pe `shouldBe` Offset badOffset
        it "works in last chunk" $ do
            let input = ["aaa\na", "aaa\n\n", "aaa", "aab\n\naaaa"]
                badOffset = 22
                parser = Data.Attoparsec.Text.char 'c' <|> (Data.Attoparsec.Text.anyChar >> parser)
                sink = sinkParser (Offset 0) parser
                sink' = sinkParserEither (Offset 0) parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Offset badOffset
            ea' <- CL.sourceList input $$ sink'
            case ea' of
                Left pe ->
                    errorPosition pe `shouldBe` Offset badOffset
        it "works in last chunk" $ do
            let input = ["aaa\na", "aaa\n\n", "aaa", "aa\n\naaaab"]
                badOffset = 22
                parser = Data.Attoparsec.Text.string "bc" <|> (Data.Attoparsec.Text.anyChar >> parser)
                sink = sinkParser (Offset 0) parser
                sink' = sinkParserEither (Offset 0) parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Offset badOffset
            ea' <- CL.sourceList input $$ sink'
            case ea' of
                Left pe ->
                    errorPosition pe `shouldBe` Offset badOffset
        it "works after new line in text" $ do
            let input = ["aaa\n", "aaa\n\n", "aaa", "aa\nb\naaaa"]
                badOffset = 15
                parser = Data.Attoparsec.Text.endOfInput <|> (Data.Attoparsec.Text.notChar 'b' >> parser)
                sink = sinkParser (Offset 0) parser
                sink' = sinkParserEither (Offset 0) parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Offset badOffset
            ea' <- CL.sourceList input $$ sink'
            case ea' of
                Left pe ->
                    errorPosition pe `shouldBe` Offset badOffset
        it "works after new line in bytestring" $ do
            let input = ["aaa\n", "aaa\n\n", "aaa", "aa\nb\naaaa"]
                badOffset = 15
                parser = Data.Attoparsec.ByteString.Char8.endOfInput <|> (Data.Attoparsec.ByteString.Char8.notChar 'b' >> parser)
                sink = sinkParser (Offset 0) parser
                sink' = sinkParserEither (Offset 0) parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Offset badOffset
            ea' <- CL.sourceList input $$ sink'
            case ea' of
                Left pe ->
                    errorPosition pe `shouldBe` Offset badOffset
        it "works for first line" $ do
            let input = ["aab\na", "aaa\n\n", "aaa", "aab\n\naaaa"]
                badOffset = 2
                parser = Data.Attoparsec.Text.endOfInput <|> (Data.Attoparsec.Text.notChar 'b' >> parser)
                sink = sinkParser (Offset 0) parser
                sink' = sinkParserEither (Offset 0) parser
            ea <- runExceptionT $ CL.sourceList input $$ sink
            case ea of
                Left e ->
                    case fromException e of
                        Just pe -> do
                            errorPosition pe `shouldBe` Offset badOffset
            ea' <- CL.sourceList input $$ sink'
            case ea' of
                Left pe ->
                    errorPosition pe `shouldBe` Offset badOffset

    describe "conduitParser" $ do
        it "parses a repeated stream" $ do
            let input = ["aaa\n", "aaa\naaa\n", "aaa\n"]
                parser = Data.Attoparsec.Text.string "aaa" <* Data.Attoparsec.Text.endOfLine
                sink = conduitParserEither (Offset 0) parser =$= CL.consume
            (Right ea) <- runExceptionT $ CL.sourceList input $$ sink
            let chk a = case a of
                          Left{} -> False
                          Right (_, xs) -> xs == "aaa"
                chkp l = (ParseDelta (Offset l) (Offset (l + 4)))
            forM_ ea $ \ a -> a `shouldSatisfy` chk :: Expectation
            forM_ (zip ea [0,4..]) $ \ (Right (pos2, _), l) -> pos2 `shouldBe` chkp l
            length ea `shouldBe` 4

        it "positions on first line" $ do
            results <- yield "hihihi\nhihi"
                $$ conduitParser (Offset 0) (Data.Attoparsec.Text.string "\n" <|> Data.Attoparsec.Text.string "hi")
                =$ CL.consume
            let f (b, d, e) = (ParseDelta (Offset b) (Offset d), e)
            results `shouldBe` map f
                [ (0,  2, "hi")
                , (2,  4, "hi")
                , (4,  6, "hi")

                , (6,  7, "\n")

                , (7,  9, "hi")
                , (9, 11, "hi")
                ]
