{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Data.Attoparsec.Enumerator
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
module Data.Conduit.Attoparsec
    ( ParseError (..)
    , AttoparsecInput
    , sinkParser
    ) where

import           Control.Exception (Exception)
import           Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.Text as T

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types as A
import qualified Data.Conduit as C

import Control.Monad.Base (MonadBase (liftBase))
import Control.Exception (throwIO)

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
    { errorContexts :: [String]
    , errorMessage :: String
    } | DivergentParser
    deriving (Show, Typeable)

instance Exception ParseError

-- | A class of types which may be consumed by an Attoparsec parser.
--
-- Since: 0.3
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

-- | Convert an Attoparsec 'A.Parser' into an 'E.Iteratee'. The parser will
-- be streamed bytes until it returns 'A.Done' or 'A.Fail'.
--
-- If parsing fails, a 'ParseError' will be thrown with 'E.throwError'. Use
-- 'E.catchError' to catch it.
sinkParser :: (AttoparsecInput a, MonadBase IO m) => A.Parser a b -> C.SinkM a m b
sinkParser p0 = C.sinkMState
    (parseA p0)
    push
    close
  where
    push parser [] = return (parser, C.SinkResult [] Nothing)
    push parser (c:cs) =
        case parser c of
            A.Done leftover x ->
                let lo = if null cs && isNull leftover then [] else leftover:cs
                 in return (parser, C.SinkResult lo (Just x))
            A.Fail _ contexts msg -> liftBase $ throwIO $ ParseError contexts msg
            A.Partial p -> push p cs
    close parser x = do
        (parser', sres) <- push parser x
        case sres of
            C.SinkResult leftover (Just res) -> return $ C.SinkResult leftover res
            C.SinkResult _leftover_must_be_empty Nothing ->
                -- by definition leftover from before is null
                case feedA (parser' empty) empty of
                    A.Done leftover y -> return $ C.SinkResult (toList leftover) y
                    A.Fail _ contexts msg -> liftBase $ throwIO $ ParseError contexts msg
                    A.Partial _ -> liftBase $ throwIO DivergentParser
    toList x
        | isNull x = []
        | otherwise = [x]
