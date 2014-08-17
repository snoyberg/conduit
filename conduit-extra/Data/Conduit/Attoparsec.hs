{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}

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
    , conduitParserEither

      -- * Types
    , ParseError (..)
    , Position (..)
    , PositionRange (..)
      -- * Classes
    , AttoparsecInput
    ) where

import           Control.Exception          (Exception)
import           Control.Monad              (unless)
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           Prelude                    hiding (lines)

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types      as A
import           Data.Conduit
import Control.Monad.Trans.Resource (MonadThrow, monadThrow)

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
    { errorContexts :: [String]
    , errorMessage  :: String
    , errorPosition :: Position
    } | DivergentParser
    deriving (Show, Typeable)

instance Exception ParseError

data Position = Position
    { posLine :: {-# UNPACK #-} !Int
    , posCol  :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord)

instance Show Position where
    show (Position l c) = show l ++ ':' : show c

data PositionRange = PositionRange
    { posRangeStart :: {-# UNPACK #-} !Position
    , posRangeEnd   :: {-# UNPACK #-} !Position
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
    getLinesCols :: a -> Position

    -- | Return the beginning of the first input with the length of
    -- the second input removed. Assumes the second string is shorter
    -- than the first.
    stripFromEnd :: a -> a -> a

instance AttoparsecInput B.ByteString where
    parseA = Data.Attoparsec.ByteString.parse
    feedA = Data.Attoparsec.ByteString.feed
    empty = B.empty
    isNull = B.null
    notEmpty = filter (not . B.null)
    getLinesCols = B.foldl' f (Position 0 0)
      where
        f (Position l c) ch | ch == 10 = Position (l + 1) 0
                            | otherwise = Position l (c + 1)
    stripFromEnd b1 b2 = B.take (B.length b1 - B.length b2) b1

instance AttoparsecInput T.Text where
    parseA = Data.Attoparsec.Text.parse
    feedA = Data.Attoparsec.Text.feed
    empty = T.empty
    isNull = T.null
    notEmpty = filter (not . T.null)
    getLinesCols = T.foldl' f (Position 0 0)
      where
        f (Position l c) ch | ch == '\n' = Position (l + 1) 0
                            | otherwise = Position l (c + 1)
    stripFromEnd t1 t2 = T.dropEnd (T.length t2) t1

-- | Convert an Attoparsec 'A.Parser' into a 'Sink'. The parser will
-- be streamed bytes until it returns 'A.Done' or 'A.Fail'.
--
-- If parsing fails, a 'ParseError' will be thrown with 'monadThrow'.
--
-- Since 0.5.0
sinkParser :: (AttoparsecInput a, MonadThrow m) => A.Parser a b -> Consumer a m b
sinkParser = fmap snd . sinkParserPosErr (Position 1 1)



-- | Consume a stream of parsed tokens, returning both the token and
-- the position it appears at. This function will raise a 'ParseError'
-- on bad input.
--
-- Since 0.5.0
conduitParser :: (AttoparsecInput a, MonadThrow m) => A.Parser a b -> Conduit a m (PositionRange, b)
conduitParser parser =
    conduit $ Position 1 0
       where
         conduit !pos = await >>= maybe (return ()) go
             where
               go x = do
                   leftover x
                   (!pos', !res) <- sinkParserPosErr pos parser
                   yield (PositionRange pos pos', res)
                   conduit pos'
{-# SPECIALIZE conduitParser
                   :: MonadThrow m
                   => A.Parser T.Text b
                   -> Conduit T.Text m (PositionRange, b) #-}
{-# SPECIALIZE conduitParser
                   :: MonadThrow m
                   => A.Parser B.ByteString b
                   -> Conduit B.ByteString m (PositionRange, b) #-}



-- | Same as 'conduitParser', but we return an 'Either' type instead
-- of raising an exception.
conduitParserEither
    :: (Monad m, AttoparsecInput a)
    => A.Parser a b
    -> Conduit a m (Either ParseError (PositionRange, b))
conduitParserEither parser =
    conduit $ Position 1 0
  where
    conduit !pos = await >>= maybe (return ()) go
      where
        go x = do
          leftover x
          eres <- sinkParserPos pos parser
          case eres of
            Left e -> yield $ Left e
            Right (!pos', !res) -> do
              yield $! Right (PositionRange pos pos', res)
              conduit pos'
{-# SPECIALIZE conduitParserEither
                   :: Monad m
                   => A.Parser T.Text b
                   -> Conduit T.Text m (Either ParseError (PositionRange, b)) #-}
{-# SPECIALIZE conduitParserEither
                   :: Monad m
                   => A.Parser B.ByteString b
                   -> Conduit B.ByteString m (Either ParseError (PositionRange, b)) #-}




sinkParserPosErr
    :: (AttoparsecInput a, MonadThrow m)
    => Position
    -> A.Parser a b
    -> Consumer a m (Position, b)
sinkParserPosErr pos0 p = sinkParserPos pos0 p >>= f
    where
      f (Left e) = monadThrow e
      f (Right a) = return a
{-# INLINE sinkParserPosErr #-}


sinkParserPos
    :: (AttoparsecInput a, Monad m)
    => Position
    -> A.Parser a b
    -> Consumer a m (Either ParseError (Position, b))
sinkParserPos pos0 p = sink empty pos0 (parseA p)
  where
    sink prev pos parser = await >>= maybe close push
      where
        push c
            | isNull c  = sink prev pos parser
            | otherwise = go False c $ parser c

        close = go True prev (feedA (parser empty) empty)

        go end c (A.Done lo x) = do
            let pos'
                    | end       = pos
                    | otherwise = addLinesCols prev pos
                y = stripFromEnd c lo
                pos'' = addLinesCols y pos'
            unless (isNull lo) $ leftover lo
            pos'' `seq` return $! Right (pos'', x)
        go end c (A.Fail rest contexts msg) =
            let x = stripFromEnd c rest
                pos'
                    | end       = pos
                    | otherwise = addLinesCols prev pos
                pos'' = addLinesCols x pos'
             in pos'' `seq` return $! Left (ParseError contexts msg pos'')
        go end c (A.Partial parser')
            | end       = return $! Left DivergentParser
            | otherwise =
                pos' `seq` sink c pos' parser'
              where
                pos' = addLinesCols prev pos

    addLinesCols :: AttoparsecInput a => a -> Position -> Position
    addLinesCols x (Position lines cols) =
        lines' `seq` cols' `seq` Position lines' cols'
      where
        Position dlines dcols = getLinesCols x
        lines' = lines + dlines
        cols' = (if dlines > 0 then 1 else cols) + dcols
{-# INLINE sinkParserPos #-}
