{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

-- |
-- Copyright: 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Consume attoparsec parsers via conduit.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module Data.Conduit.Attoparsec.Tracking
    ( -- * Sink
      sinkParser
    , sinkParserEither
      -- * Conduit
    , conduitParser
    , conduitParserEither

      -- * Types
    , ParseError (..)
    , ParseDelta (..)
      -- * Classes
    , AttoparsecInput(..)
    , AttoparsecState(..)
    ) where

import           Control.Exception            (Exception)
import           Control.Monad                (unless)
import           Control.Monad.Trans.Resource (MonadThrow, monadThrow)
import qualified Data.Attoparsec.Types        as A
import           Data.Conduit
import           Data.Typeable                (Typeable)
import           Prelude                      hiding (lines)

-- | The context and message from a 'A.Fail' value.
data ParseError s = ParseError
    { errorContexts :: [String]
    , errorMessage  :: String
    , errorPosition :: s
    } | DivergentParser
    deriving (Show, Typeable)

data ParseDelta s = ParseDelta
    { before  :: {-# UNPACK #-} !s
    , after   :: {-# UNPACK #-} !s
    }
    deriving (Eq, Ord)

-- | A class of types which may be consumed by an Attoparsec parser.
class AttoparsecInput a where
    parseA :: A.Parser a b -> a -> A.IResult a b
    feedA :: A.IResult a b -> a -> A.IResult a b
    empty :: a
    isNull :: a -> Bool
    notEmpty :: [a] -> [a]

    -- | Return the beginning of the first input with the length of
    -- the second input removed. Assumes the second string is shorter
    -- than the first.
    stripFromEnd :: a -> a -> a

class AttoparsecState a s where
    getLinesCols :: a -> s
    addLinesCols :: AttoparsecInput a => a -> s -> s

-- | Convert an Attoparsec 'A.Parser' into a 'Sink'. The parser will
-- be streamed bytes until it returns 'A.Done' or 'A.Fail'.
--
-- If parsing fails, a 'ParseError' will be thrown with 'monadThrow'.
--
-- Since 0.5.0
sinkParser :: (AttoparsecInput a, AttoparsecState a s, MonadThrow m, Exception (ParseError s)) => s -> A.Parser a b -> Consumer a m b
sinkParser s = fmap snd . sinkParserPosErr s

-- | Same as 'sinkParser', but we return an 'Either' type instead
-- of raising an exception.
--
-- Since 1.1.5
sinkParserEither :: (AttoparsecInput a, AttoparsecState a s, Monad m) => s -> A.Parser a b -> Consumer a m (Either (ParseError s) b)
sinkParserEither s = (fmap.fmap) snd . sinkParserPos s

-- | Consume a stream of parsed tokens, returning both the token and
-- the position it appears at. This function will raise a 'ParseError'
-- on bad input.
--
-- Since 0.5.0
conduitParser :: (AttoparsecInput a, AttoparsecState a s, MonadThrow m, Exception (ParseError s)) => s -> A.Parser a b -> Conduit a m (ParseDelta s, b)
conduitParser s parser =
    conduit $ s
       where
         conduit !pos = await >>= maybe (return ()) go
             where
               go x = do
                   leftover x
                   (!pos', !res) <- sinkParserPosErr pos parser
                   yield (ParseDelta pos pos', res)
                   conduit pos'


-- | Same as 'conduitParser', but we return an 'Either' type instead
-- of raising an exception.
conduitParserEither
    :: (Monad m, AttoparsecInput a, AttoparsecState a s)
    => s
    -> A.Parser a b
    -> Conduit a m (Either (ParseError s) (ParseDelta s, b))
conduitParserEither s parser =
    conduit $ s
  where
    conduit !pos = await >>= maybe (return ()) go
      where
        go x = do
          leftover x
          eres <- sinkParserPos pos parser
          case eres of
            Left e -> yield $ Left e
            Right (!pos', !res) -> do
              yield $! Right (ParseDelta pos pos', res)
              conduit pos'

sinkParserPosErr
    :: (AttoparsecInput a, AttoparsecState a s, MonadThrow m, Exception (ParseError s))
    => s
    -> A.Parser a b
    -> Consumer a m (s, b)
sinkParserPosErr s p = sinkParserPos s p >>= f
    where
      f (Left e) = monadThrow e
      f (Right a) = return a
{-# INLINE sinkParserPosErr #-}

sinkParserPos
    :: (AttoparsecInput a, AttoparsecState a s, Monad m)
    => s
    -> A.Parser a b
    -> Consumer a m (Either (ParseError s) (s, b))
sinkParserPos s p = sink empty s (parseA p)
  where
    -- sink :: a -> s -> (a -> A.IResult a b) -> Consumer a m (Either (ParseError s) (s, b))
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

{-# INLINE sinkParserPos #-}
