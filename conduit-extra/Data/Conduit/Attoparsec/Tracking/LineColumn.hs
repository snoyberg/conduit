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
module Data.Conduit.Attoparsec.Tracking.LineColumn
    ( Position (..)
    ) where

import           Control.Exception                (Exception)
import           Control.Monad                    (unless)
import qualified Data.ByteString                  as B
import           Data.Conduit.Attoparsec.Tracking
import qualified Data.Text                        as T
import qualified Data.Text.Internal               as TI
import           Data.Typeable                    (Typeable)
import           Prelude                          hiding (lines)

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types      as A
import           Data.Conduit
import Control.Monad.Trans.Resource (MonadThrow, monadThrow)

data Position = Position
    { posLine :: {-# UNPACK #-} !Int
    , posCol  :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord)

instance Show Position where
    show (Position l c) = show l ++ ':' : show c

instance Exception (ParseError Position)

instance Show (ParseDelta Position) where
    show (ParseDelta s e) = show s ++ '-' : show e

instance AttoparsecInput B.ByteString where
    parseA = Data.Attoparsec.ByteString.parse
    feedA = Data.Attoparsec.ByteString.feed
    empty = B.empty
    isNull = B.null
    notEmpty = filter (not . B.null)
    stripFromEnd b1 b2 = B.take (B.length b1 - B.length b2) b1

instance AttoparsecState B.ByteString Position where
    getLinesCols = B.foldl' f (Position 0 0)
      where
        f (Position l c) ch | ch == 10 = Position (l + 1) 0
                            | otherwise = Position l (c + 1)
    addLinesCols x (Position lines cols) =
        lines' `seq` cols' `seq` Position lines' cols'
      where
        Position dlines dcols = getLinesCols x
        lines' = lines + dlines
        cols' = (if dlines > 0 then 1 else cols) + dcols

instance AttoparsecInput T.Text where
    parseA = Data.Attoparsec.Text.parse
    feedA = Data.Attoparsec.Text.feed
    empty = T.empty
    isNull = T.null
    notEmpty = filter (not . T.null)
    stripFromEnd (TI.Text arr1 off1 len1) (TI.Text _ _ len2) =
        TI.textP arr1 off1 (len1 - len2)

instance AttoparsecState T.Text Position where
    getLinesCols = T.foldl' f (Position 0 0)
      where
        f (Position l c) ch | ch == '\n' = Position (l + 1) 0
                            | otherwise = Position l (c + 1)
    addLinesCols x (Position lines cols) =
        lines' `seq` cols' `seq` Position lines' cols'
      where
        Position dlines dcols = getLinesCols x
        lines' = lines + dlines
        cols' = (if dlines > 0 then 1 else cols) + dcols

{-# SPECIALIZE conduitParser
                  :: MonadThrow m
                  => Position
                  -> A.Parser T.Text b
                  -> Conduit T.Text m (ParseDelta Position, b) #-}

{-# SPECIALIZE conduitParser
                  :: MonadThrow m
                  => Position
                  -> A.Parser B.ByteString b
                  -> Conduit B.ByteString m (ParseDelta Position, b) #-}

{-# SPECIALIZE conduitParserEither
                  :: Monad m
                  => Position
                  -> A.Parser T.Text b
                  -> Conduit T.Text m (Either (ParseError Position) (ParseDelta Position, b)) #-}

{-# SPECIALIZE conduitParserEither
                  :: Monad m
                  => Position
                  -> A.Parser B.ByteString b
                  -> Conduit B.ByteString m (Either (ParseError Position) (ParseDelta Position, b)) #-}
