{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Your intended one-stop-shop for conduit functionality.
-- This re-exports functions from many commonly used modules.
-- When there is a conflict with standard functions, functions
-- in this module are disambiguated by adding a trailing C
-- (or for chunked functions, replacing a trailing E with CE).
-- This means that the Conduit module can be imported unqualified
-- without causing naming conflicts.
--
-- For more information on the naming scheme and intended usages of the
-- combinators, please see the "Data.Conduit.Combinators" documentation.
module Conduit
    ( -- * Core conduit library
      module Data.Conduit
    , module Data.Conduit.Lift
      -- * Commonly used combinators
    , module Data.Conduit.Combinators.Unqualified
      -- * Monadic lifting
    , MonadIO (..)
    , MonadTrans (..)
    , MonadThrow (..)
    , MonadCatch (..)
    , MonadUnliftIO (..)
    , PrimMonad (..)
      -- * ResourceT
    , MonadResource
    , ResourceT
    , runResourceT
      -- * Acquire
    , module Data.Acquire
      -- * Pure pipelines
    , Identity (..)
    ) where

import Data.Conduit
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Primitive (PrimMonad (..), PrimState)
import Data.Conduit.Lift
import Data.Conduit.Combinators.Unqualified
import Data.Functor.Identity (Identity (..))
import Control.Monad.Trans.Resource (MonadResource, MonadThrow (..), MonadCatch (..), runResourceT, ResourceT)
import Data.Acquire hiding (with)
