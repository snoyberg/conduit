{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

-- |
-- Copyright: 2016 John Ky, 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Customisation for tracking line and column numbers.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module Data.Conduit.Attoparsec.Tracking.LineColumn
    ( Position (..)
    ) where

import           Control.Exception                          (Exception)
import           Control.Monad.Trans.Resource               (MonadThrow)
import qualified Data.Attoparsec.Types                      as A
import qualified Data.ByteString                            as B
import           Data.Conduit
import           Data.Conduit.Attoparsec.Tracking.Internal
import qualified Data.Text                                  as T
import           Prelude                                    hiding (lines)

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
