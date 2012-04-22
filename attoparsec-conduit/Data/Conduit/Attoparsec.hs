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

import           Control.Exception (Exception)
import           Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Monad.Trans.Class (lift)

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types as A
import qualified Data.Conduit as C

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
    { errorContexts :: [String]
    , errorMessage :: String
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

instance AttoparsecInput B.ByteString where
    parseA = Data.Attoparsec.ByteString.parse
    feedA = Data.Attoparsec.ByteString.feed
    empty = B.empty
    isNull = B.null
    notEmpty = filter (not . B.null)

instance AttoparsecInput T.Text where
    parseA = Data.Attoparsec.Text.parse
    feedA = Data.Attoparsec.Text.feed
    empty = T.empty
    isNull = T.null
    notEmpty = filter (not . T.null)

-- | Convert an Attoparsec 'A.Parser' into a 'C.Sink'. The parser will
-- be streamed bytes until it returns 'A.Done' or 'A.Fail'.
--
-- If parsing fails, a 'ParseError' will be thrown with 'C.monadThrow'.
sinkParser :: (AttoparsecInput a, C.MonadThrow m) => A.Parser a b -> C.Sink a m b
sinkParser =
    sink . parseA
  where
    sink parser = C.NeedInput (push parser) (close parser)

    push parser c | isNull c = sink parser
    push parser c = go (parser c) sink

    close parser = go
        (feedA (parser empty) empty)
        (const $ C.PipeM exc $ lift exc)
      where
        exc = C.monadThrow DivergentParser

    go (A.Done leftover x) _ =
        C.Done lo x
      where
        lo
            | isNull leftover = Nothing
            | otherwise = Just leftover
    go (A.Fail _ contexts msg) _ =
        C.PipeM exc $ lift exc
      where
        exc = C.monadThrow $ ParseError contexts msg

    go (A.Partial parser') onPartial = onPartial parser'
