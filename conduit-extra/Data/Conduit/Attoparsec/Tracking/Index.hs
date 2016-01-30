{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

-- |
-- Copyright: 2016 John Ky, 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Consume attoparsec parsers via conduit.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module Data.Conduit.Attoparsec.Tracking.Index
    ( Index (..)
    ) where

import           Control.Exception                (Exception)
import           Control.Monad.Trans.Resource     (MonadThrow)
import qualified Data.Attoparsec.Types            as A
import qualified Data.ByteString                  as B
import           Data.Conduit
import           Data.Conduit.Attoparsec.Tracking
import qualified Data.Text                        as T
import           Prelude                          hiding (lines)

data Index = Index
    { posLine :: {-# UNPACK #-} !Int
    , posCol  :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord)

instance Show Index where
    show (Index l c) = show l ++ ':' : show c

instance Exception (ParseError Index)

instance Show (ParseDelta Index) where
    show (ParseDelta s e) = show s ++ '-' : show e

instance AttoparsecState B.ByteString Index where
    getLinesCols = B.foldl' f (Index 0 0)
      where
        f (Index l c) ch | ch == 10 = Index (l + 1) 0
                            | otherwise = Index l (c + 1)
    addLinesCols x (Index lines cols) =
        lines' `seq` cols' `seq` Index lines' cols'
      where
        Index dlines dcols = getLinesCols x
        lines' = lines + dlines
        cols' = (if dlines > 0 then 1 else cols) + dcols

instance AttoparsecState T.Text Index where
    getLinesCols = T.foldl' f (Index 0 0)
      where
        f (Index l c) ch | ch == '\n' = Index (l + 1) 0
                            | otherwise = Index l (c + 1)
    addLinesCols x (Index lines cols) =
        lines' `seq` cols' `seq` Index lines' cols'
      where
        Index dlines dcols = getLinesCols x
        lines' = lines + dlines
        cols' = (if dlines > 0 then 1 else cols) + dcols

{-# SPECIALIZE conduitParser
                  :: MonadThrow m
                  => Index
                  -> A.Parser T.Text b
                  -> Conduit T.Text m (ParseDelta Index, b) #-}

{-# SPECIALIZE conduitParser
                  :: MonadThrow m
                  => Index
                  -> A.Parser B.ByteString b
                  -> Conduit B.ByteString m (ParseDelta Index, b) #-}

{-# SPECIALIZE conduitParserEither
                  :: Monad m
                  => Index
                  -> A.Parser T.Text b
                  -> Conduit T.Text m (Either (ParseError Index) (ParseDelta Index, b)) #-}

{-# SPECIALIZE conduitParserEither
                  :: Monad m
                  => Index
                  -> A.Parser B.ByteString b
                  -> Conduit B.ByteString m (Either (ParseError Index) (ParseDelta Index, b)) #-}
