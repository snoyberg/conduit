{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

-- |
-- Copyright: 2016 John Ky, 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Customisation for tracking offset from beginning of stream.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module Data.Conduit.Attoparsec.Tracking.Offset
    ( Index (..)
    ) where

import           Control.Exception                          (Exception)
import           Control.Monad.Trans.Resource               (MonadThrow)
import qualified Data.Attoparsec.Types                      as A
import qualified Data.ByteString                            as B
import           Data.Conduit
import           Data.Conduit.Attoparsec.Tracking.Internal
import qualified Data.Text                                  as T
import           Prelude                                    hiding (lines)

data Index = Index
    { pos :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord)

instance Show Index where
    show (Index c) = show c

instance Exception (ParseError Index)

instance Show (ParseDelta Index) where
    show (ParseDelta s e) = show s ++ '-' : show e

instance AttoparsecState B.ByteString Index where
    getState = B.foldl' f (Index 0)
      where
        f (Index c) _ = Index (c + 1)
    modState x (Index cols) = cols' `seq` Index cols'
      where
        Index dcols = getState x
        cols' = cols + dcols

instance AttoparsecState T.Text Index where
    getState = T.foldl' f (Index 0)
      where
        f (Index c) _ = Index (c + 1)
    modState x (Index cols) =
        cols' `seq` Index cols'
      where
        Index dcols = getState x
        cols' = cols + dcols

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
