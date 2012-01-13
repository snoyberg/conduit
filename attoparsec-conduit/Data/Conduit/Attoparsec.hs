{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright: 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Turn an Attoparsec parser into a 'C.Sink' or 'C.Conduit'.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module Data.Conduit.Attoparsec
    ( ParseError (..)
    , AttoparsecInput
    , ConduitInput
    , sinkParser
    , conduitParser
    ) where

import           Control.Exception (Exception)
import           Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.Text as T

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types as A
import qualified Data.Conduit as C
import Control.Monad.Trans.Class (lift)

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

-- | Extra functions needed for conduitParser
class (AttoparsecInput a) => ConduitInput a where
    append :: a -> a -> a

instance AttoparsecInput B.ByteString where
    parseA = Data.Attoparsec.ByteString.parse
    feedA = Data.Attoparsec.ByteString.feed
    empty = B.empty
    isNull = B.null
    notEmpty = filter (not . B.null)

instance ConduitInput B.ByteString where
    append = B.append

instance AttoparsecInput T.Text where
    parseA = Data.Attoparsec.Text.parse
    feedA = Data.Attoparsec.Text.feed
    empty = T.empty
    isNull = T.null
    notEmpty = filter (not . T.null)

-- | Convert an Attoparsec 'A.Parser' into a 'C.Sink'. The parser will
-- be streamed bytes until it returns 'A.Done' or 'A.Fail'.
--
-- If parsing fails, a 'ParseError' will be thrown with 'C.resourceThrow'.
sinkParser :: (AttoparsecInput a, C.ResourceThrow m) => A.Parser a b -> C.Sink a m b
sinkParser p0 = C.sinkState
    (parseA p0)
    push
    close
  where
    push parser c | isNull c = return (C.StateProcessing parser)
    push parser c =
        case parser c of
            A.Done leftover x ->
                let lo = if isNull leftover then Nothing else Just leftover
                 in return (C.StateDone lo x)
            A.Fail _ contexts msg -> lift $ C.resourceThrow $ ParseError contexts msg
            A.Partial p -> return (C.StateProcessing p)
    close parser = do
        case feedA (parser empty) empty of
            A.Done _leftover y -> return y
            A.Fail _ contexts msg -> lift $ C.resourceThrow $ ParseError contexts msg
            A.Partial _ -> lift $ C.resourceThrow DivergentParser

-- | Convert an Attoparsec 'A.Parser' into a 'C.Conduit'. The parser will
-- be streamed bytes until the source is exhausted. When done is returned a new
-- parser is created and fed with anything leftover in the stream before resuming. 
--
-- If parsing fails, a 'ParseError' will be thrown with 'C.resourceThrow'.
conduitParser :: (ConduitInput a, C.ResourceThrow m) =>
                        A.Parser a b
                     -> C.Conduit a m b
conduitParser p0 = C.conduitState
    (parseA p0)
    push
    close
  where
    push parser c | isNull c = return (parser, C.Producing [])
    push parser c =
        case parser c of
            A.Done leftover x
                | isNull leftover ->    
                    return (parseA p0, C.Producing  [x])
                | otherwise ->
                    let newP = parseA p0 
                    in
                     return ((\inp -> newP $ append leftover inp),
                             C.Producing [x])
            A.Fail _ contexts msg -> lift $ C.resourceThrow $ ParseError contexts msg
            A.Partial p -> return (p, C.Producing [])
    close parser = do
        case feedA (parser empty) empty of
            A.Done _leftover y -> return [y]
            A.Fail _ contexts msg -> lift $ C.resourceThrow $ ParseError contexts msg
            A.Partial _ -> lift $ C.resourceThrow DivergentParser
