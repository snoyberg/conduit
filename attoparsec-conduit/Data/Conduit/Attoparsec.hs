{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright: 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Turn an Attoparsec parser into a 'C.Sink'.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module Data.Conduit.Attoparsec
    ( ParseError (..)
    , AttoparsecInput
    , sinkParser
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
import           Data.Conduit
import           Data.Maybe (fromMaybe)

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
    { errorContexts :: [String]
    , errorMessage  :: String
    , errorLine     :: Int
    , errorColumn   :: Int
    } | DivergentParser
    deriving (Show, Typeable)

instance Exception ParseError

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
sinkParser :: (AttoparsecInput a, MonadThrow m) => A.Parser a b -> Pipe a a o u m b
sinkParser =
    sink 1 0 . parseA
  where
    sink lines cols parser = do
        await >>= maybe close push
      where

        push c
            | isNull c  = sink lines cols parser
            | otherwise = go (Just c) $ parser c

        close = go (Nothing :: Maybe T.Text) (feedA (parser empty) empty)

        go _ (A.Done lo x) = do
            unless (isNull lo) $ leftover lo
            return x
        go mc (A.Fail rest contexts msg) =
            let c = fromMaybe empty mc
                x = take' (length' c - length' rest) c
                (lines', cols') = addLinesCols x
              in lift $ monadThrow $ ParseError contexts msg lines' (cols' + 1)
        go mc (A.Partial parser') =
            case mc of
                Nothing -> lift $ monadThrow DivergentParser
                Just c ->
                    let (lines', cols') = addLinesCols c
                     in sink lines' cols' parser'

        addLinesCols :: AttoparsecInput a => a -> (Int, Int)
        addLinesCols x =
            lines' `seq` cols' `seq` (lines', cols')
          where
            (dlines, dcols) = getLinesCols x
            lines' = lines + dlines
            cols' = (if dlines > 0 then 0 else cols) + dcols
