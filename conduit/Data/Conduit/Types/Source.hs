{-# LANGUAGE FlexibleContexts #-}
-- | Defines the types for a source, which is a producer of data.
module Data.Conduit.Types.Source
    ( SourceResult (..)
    , Source (..)
    ) where

import Control.Monad.Trans.Resource
import Data.Monoid (Monoid (..))
import Control.Monad (liftM)

-- | Result of pulling from a source. Either a new piece of data (@Open@), or
-- indicates that the source is now @Closed@.
--
-- The @Open@ constructor returns both a new value, as well as a new @Source@,
-- which should be used in place of the previous @Source@.
--
-- Since 0.2.0
data SourceResult m a = Open (Source m a) a | Closed

instance Monad m => Functor (SourceResult m) where
    fmap f (Open p a) = Open (fmap f p) (f a)
    fmap _ Closed = Closed

-- | A @Source@ has two operations on it: pull some data, and close the
-- @Source@. Since @Source@ is built on top of 'ResourceT', all acquired
-- resources should be automatically released anyway. Closing a @Source@ early
-- is merely an optimization to free scarce resources as soon as possible.
--
-- A @Source@ is should free any resources it allocated when either
-- @sourceClose@ is called or a @Closed@ is returned. However, based on the
-- usage of @ResourceT@, this is simply an optimization.
--
-- Since 0.2.0
data Source m a = Source
    { sourcePull :: ResourceT m (SourceResult m a)
    , sourceClose :: ResourceT m ()
    }

instance Monad m => Functor (Source m) where
    fmap f src = src
        { sourcePull = liftM (fmap f) (sourcePull src)
        }

instance Resource m => Monoid (Source m a) where
    mempty = Source
        { sourcePull = return Closed
        , sourceClose = return ()
        }
    mappend a b = mconcat [a, b]
    mconcat [] = mempty
    mconcat (next0:rest0) =
        src next0 rest0
      where
        src next rest = Source (pull next rest) (close next rest)

        pull current rest = do
            res <- sourcePull current
            case res of
                -- end of the current Source
                Closed -> do
                    case rest of
                        -- ... and open the next one
                        a:as -> pull a as
                        -- no more source, return an EOF
                        [] -> return Closed
                Open current' val -> return (Open (src current' rest) val)
        close current _rest = do
            -- we only need to close the current Source, since they are opened
            -- one at a time
            sourceClose current
