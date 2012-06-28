{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright: 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Consume attoparsec parsers via conduit.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module Data.Conduit.Attoparsec
    ( -- * Sink
      sinkParser
      -- * Conduit
    , conduitParser
      -- * Types
    , ParseError (..)
    , Position (..)
    , PositionRange (..)
      -- * Classes
    , AttoparsecInput
    ) where

import           Prelude hiding (lines)
import           Control.Exception (Exception)
import           Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import           Control.Monad.Trans.Class (lift)
import           Control.Monad (unless)

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types as A
import           Data.Conduit hiding (Pipe, Sink, Conduit, Source)

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
    { errorContexts :: [String]
    , errorMessage  :: String
    , errorPosition :: Position
    } | DivergentParser
    deriving (Show, Typeable)

instance Exception ParseError

data Position = Position
    { posLine :: Int
    , posCol  :: Int
    }
    deriving (Eq, Ord)
instance Show Position where
    show (Position l c) = show l ++ ':' : show c

data PositionRange = PositionRange
    { posRangeStart :: Position
    , posRangeEnd   :: Position
    }
    deriving (Eq, Ord)
instance Show PositionRange where
    show (PositionRange s e) = show s ++ '-' : show e

-- | A class of types which may be consumed by an Attoparsec parser.
class AttoparsecInput a where
    parseA :: A.Parser a b -> a -> A.IResult a b
    feedA :: A.IResult a b -> a -> A.IResult a b
    empty :: a
    isNull :: a -> Bool
    notEmpty :: [a] -> [a]
    getLinesCols :: a -> (Int, Int)
    take' :: Int -> a -> a
    length' :: a -> Int

instance AttoparsecInput B.ByteString where
    parseA = Data.Attoparsec.ByteString.parse
    feedA = Data.Attoparsec.ByteString.feed
    empty = B.empty
    isNull = B.null
    notEmpty = filter (not . B.null)
    getLinesCols b =
        (lines, cols)
      where
        lines = B.count 10 b
        cols =
            case B8.lines b of
                [] -> 0
                ls -> B.length $ last ls
    take' = B.take
    length' = B.length

instance AttoparsecInput T.Text where
    parseA = Data.Attoparsec.Text.parse
    feedA = Data.Attoparsec.Text.feed
    empty = T.empty
    isNull = T.null
    notEmpty = filter (not . T.null)
    getLinesCols t =
        (lines, cols)
      where
        lines = T.count (T.pack "\n") t
        cols =
            case T.lines t of
                [] -> 0
                ls -> T.length $ last ls
    take' = T.take
    length' = T.length

-- | Convert an Attoparsec 'A.Parser' into a 'Sink'. The parser will
-- be streamed bytes until it returns 'A.Done' or 'A.Fail'.
--
-- If parsing fails, a 'ParseError' will be thrown with 'monadThrow'.
--
-- Since 0.5.0
sinkParser :: (AttoparsecInput a, MonadThrow m) => A.Parser a b -> GLSink a m b
sinkParser = fmap snd . sinkParserPos (Position 1 1)

-- | Consume a stream of parsed tokens, returning both the token and the
-- position it appears at.
--
-- Since 0.5.0
conduitParser :: (AttoparsecInput a, MonadThrow m) => A.Parser a b -> GLInfConduit a m (PositionRange, b)
conduitParser parser =
    conduit $ Position 1 0
  where
    conduit pos =
        awaitE >>= either return go
      where
        go x = do
            leftover x
            (pos', res) <- sinkParserPos pos parser
            yield (PositionRange pos pos', res)
            conduit pos'

sinkParserPos :: (AttoparsecInput a, MonadThrow m) => Position -> A.Parser a b -> GLSink a m (Position, b)
sinkParserPos pos0 =
    sink empty pos0 . parseA
  where
    sink prev pos parser = do
        await >>= maybe close push
      where

        push c
            | isNull c  = sink prev pos parser
            | otherwise = go False c $ parser c

        close = go True prev (feedA (parser empty) empty)

        go end c (A.Done lo x) = do
            let pos'
                    | end       = pos
                    | otherwise = addLinesCols prev pos
                y = take' (length' c - length' lo) c
                pos'' = addLinesCols y pos'
            unless (isNull lo) $ leftover lo
            return (pos'', x)
        go end c (A.Fail rest contexts msg) =
            let x = take' (length' c - length' rest) c
                pos'
                    | end       = pos
                    | otherwise = addLinesCols prev pos
                pos'' = addLinesCols x pos'
             in lift $ monadThrow $ ParseError contexts msg pos''
        go end c (A.Partial parser')
            | end       = lift $ monadThrow DivergentParser
            | otherwise =
                sink c pos' parser'
              where
                pos' = addLinesCols prev pos

    addLinesCols :: AttoparsecInput a => a -> Position -> Position
    addLinesCols x (Position lines cols) =
        lines' `seq` cols' `seq` Position lines' cols'
      where
        (dlines, dcols) = getLinesCols x
        lines' = lines + dlines
        cols' = (if dlines > 0 then 1 else cols) + dcols
