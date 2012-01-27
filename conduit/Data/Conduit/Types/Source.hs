{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Defines the types for a source, which is a producer of data.
module Data.Conduit.Types.Source
    ( SourceResult (..)
    , PreparedSource (..)
    , Source (..)
    , SourceInvariantException (..)
    ) where

import Control.Monad.Trans.Resource
import Data.Monoid (Monoid (..))
import Control.Monad (liftM)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

-- | Result of pulling from a source. Either a new piece of data (@Open@), or
-- indicates that the source is now @Closed@.
--
-- Since 0.0.0
data SourceResult m a = Open (PreparedSource m a) a | Closed

instance Monad m => Functor (SourceResult m) where
    fmap f (Open p a) = Open (fmap f p) (f a)
    fmap _ Closed = Closed

-- | A 'PreparedSource' has two operations on it: pull some data, and close the
-- 'PreparedSource'. Since 'PreparedSource' is built on top of 'ResourceT', all
-- acquired resources should be automatically released anyway. Closing a
-- 'PreparedSource' early
-- is merely an optimization to free scarce resources as soon as possible.
--
-- A 'PreparedSource' has three invariants:
--
-- * It is illegal to call 'sourcePull' after a previous call returns 'Closed', or after a call to 'sourceClose'.
--
-- * It is illegal to call 'sourceClose' multiple times, or after a previous
-- 'sourcePull' returns a 'Closed'.
--
-- * A 'PreparedSource' is responsible to free any resources when either 'sourceClose'
-- is called or a 'Closed' is returned. However, based on the usage of
-- 'ResourceT', this is simply an optimization.
--
-- Since 0.0.0
data PreparedSource m a = PreparedSource
    { sourcePull :: ResourceT m (SourceResult m a)
    , sourceClose :: ResourceT m ()
    }

instance Monad m => Functor (PreparedSource m) where
    fmap f src = src
        { sourcePull = liftM (fmap f) (sourcePull src)
        }

-- | All but the simplest of 'PreparedSource's (e.g., @repeat@) require some
-- type of state to track their current status. This may be in the form of a
-- mutable variable (e.g., @IORef@), or via opening a resource like a @Handle@.
-- While a 'PreparedSource' is given no opportunity to acquire such resources,
-- this type is.
--
-- A 'Source' is simply a monadic action that returns a 'PreparedSource'. One
-- nice consequence of this is the possibility of creating an efficient
-- 'Monoid' instance, which will only acquire one resource at a time, instead
-- of bulk acquiring all resources at the beginning of running the 'Source'.
--
-- Note that each time you \"call\" a @Source@, it is started from scratch. If
-- you want a resumable source (e.g., one which can be passed to multiple
-- @Sink@s), you likely want to use a 'BufferedSource'.
--
-- Since 0.0.0
newtype Source m a = Source { prepareSource :: ResourceT m (PreparedSource m a) }

instance Monad m => Functor (Source m) where
    fmap f (Source msrc) = Source (liftM (fmap f) msrc)

instance Resource m => Monoid (Source m a) where
    mempty = Source (return PreparedSource
        { sourcePull = return Closed
        , sourceClose = return ()
        })
    mappend a b = mconcat [a, b]
    mconcat [] = mempty
    mconcat (Source mnext:rest0) = Source $ do
        -- open up the first Source...
        next0 <- mnext

        return $ src next0 rest0
      where
        src next rest = PreparedSource (pull next rest) (close next rest)

        pull current rest = do
            res <- sourcePull current
            case res of
                -- end of the current Source
                Closed -> do
                    case rest of
                        -- ... and open the next one
                        Source ma:as -> do
                            a <- ma
                            pull a as
                        -- no more source, return an EOF
                        [] -> return Closed
                Open current' val -> return (Open (src current' rest) val)
        close current _rest = do
            -- we only need to close the current Source, since they are opened
            -- one at a time
            sourceClose current

-- |
-- Since 0.0.0
data SourceInvariantException = PullAfterEOF String
    deriving (Show, Typeable)
instance Exception SourceInvariantException
