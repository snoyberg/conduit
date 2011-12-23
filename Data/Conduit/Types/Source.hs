{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Defines the types for a source, which is a producer of data.
module Data.Conduit.Types.Source
    ( SourceResult (..)
    , PreparedSource (..)
    , Source (..)
    , BufferedSource (..)
    , SourceInvariantException (..)
    , BufferSource (..)
    ) where

import Control.Monad.Trans.Resource
import Data.Monoid (Monoid (..))
import Control.Monad (liftM)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throw)

-- | When pulling data from a source, it returns back a list of values pulled
-- from the stream. It also indicates whether or not the stream has been
-- closed.
data SourceResult a = Open [a] | Closed
    deriving (Show, Eq, Ord)

instance Functor SourceResult where
    fmap f (Open a) = Open (fmap f a)
    fmap f Closed = Closed

-- | A 'Source' has two operations on it: pull some data, and close the
-- 'Source'. Since 'Source' is built on top of 'ResourceT', all acquired
-- resources should be automatically released anyway. Closing a 'Source' early
-- is merely an optimization to free scarce resources as soon as possible.
--
-- A 'Source' has three invariants:
--
-- * It is illegal to call 'sourcePull' after a previous call returns 'Closed', or after a call to 'sourceClose'.
--
-- * It is illegal to call 'sourceClose' multiple times, or after a previous
-- 'sourcePull' returns a 'Closed'.
--
-- * A 'Source' is responsible to free any resources when either 'sourceClose'
-- is called or a 'Closed' is returned. However, based on the usage of
-- 'ResourceT', this is simply an optimization.
data PreparedSource m a = PreparedSource
    { sourcePull :: ResourceT m (SourceResult a)
    , sourceClose :: ResourceT m ()
    }

instance Monad m => Functor (PreparedSource m) where
    fmap f src = src
        { sourcePull = liftM (fmap f) (sourcePull src)
        }

-- | All but the simplest of 'Source's (e.g., @repeat@ and @cycle@) require
-- some type of state to track their current status. This may be in the form of
-- a mutable variable (e.g., @IORef@), or via opening a resource like a
-- @Handle@. While a 'Source' is given no opportunity to acquire such
-- resources, this type is.
--
-- A 'Source' is simply a monadic action that returns a 'Source'. One nice
-- consequence of this is the possibility of creating an efficient 'Monoid'
-- instance, which will only acquire one resource at a time, instead of bulk
-- acquiring all resources at the beginning of running the 'Source'.
--
-- Note that each time you \"call\" a @Source@, it is started from scratch. If
-- you want a resumable source (e.g., one which can be passed to multiple
-- @Sink@s), you likely want to use a 'BufferedSource'.
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
        -- and place it in a mutable reference along with all of the upcoming
        -- Sources
        istate <- newRef (next0, rest0)
        return PreparedSource
            { sourcePull = pull istate
            , sourceClose = close istate
            }
      where
        pull istate =
            readRef istate >>= pull'
          where
            pull' (current, rest) = do
                res <- sourcePull current
                case res of
                    -- end of the current Source
                    Closed -> do
                        case rest of
                            -- ... and open the next one
                            Source ma:as -> do
                                a <- ma
                                writeRef istate (a, as)
                                -- continue pulling base on this new state
                                pull istate
                            -- no more source, return an EOF
                            [] -> do
                                -- give an error message if the first Source
                                -- invariant is violated (read data after EOF)
                                writeRef istate $
                                    throw $ PullAfterEOF "Source:mconcat"
                                return Closed
                    Open _ -> return res
        close istate = do
            -- we only need to close the current Source, since they are opened
            -- one at a time
            (current, _) <- readRef istate
            sourceClose current

-- | When actually interacting with 'Source's, we usually want to be able to
-- buffer the output, in case any intermediate steps return leftover data. A
-- 'BufferedSource' allows for such buffering, via the 'bsourceUnpull' function.
--
-- A 'BufferedSource', unlike a 'Source', is resumable, meaning it can be passed to
-- multiple 'Sink's without restarting.
--
-- Finally, a 'BufferedSource' relaxes one of the invariants of a 'Source': calling
-- 'bsourcePull' after an 'EOF' will simply return another 'EOF'.
data BufferedSource m a = BufferedSource
    { bsourcePull :: ResourceT m (SourceResult a)
    , bsourceUnpull :: [a] -> ResourceT m () -- ^ It is the responsibility of the 'BufferedSource' to check if the argument is null.
    , bsourceClose :: ResourceT m ()
    }

data SourceInvariantException = PullAfterEOF String
    deriving (Show, Typeable)
instance Exception SourceInvariantException

-- | This typeclass allows us to unify operators on 'Source' and 'BufferedSource'.
class BufferSource s where
    bufferSource :: Resource m => s m a -> ResourceT m (BufferedSource m a)

-- | Note that this instance hides the 'bsourceClose' record, so that a
-- @BufferedSource@ remains resumable.
instance BufferSource BufferedSource where
    bufferSource bsrc = return bsrc
        { bsourceClose = return ()
        }

-- | State of a 'BufferedSource'
data BState a = EmptyOpen -- ^ nothing in buffer, EOF not received yet
              | EmptyClosed -- ^ nothing in buffer, EOF has been received
              | BOpen [a] -- ^ something in buffer, EOF not received yet
              | BClosed [a] -- ^ something in buffer, EOF has been received
    deriving Show

instance BufferSource PreparedSource where
    bufferSource src = do
        istate <- newRef EmptyOpen
        return BufferedSource
            { bsourcePull = do
                mresult <- modifyRef istate $ \state ->
                    case state of
                        BOpen buffer -> (EmptyOpen, Just $ Open buffer)
                        BClosed buffer -> (EmptyClosed, Just $ Open buffer)
                        EmptyOpen -> (EmptyOpen, Nothing)
                        EmptyClosed -> (EmptyClosed, Just Closed)
                case mresult of
                    Nothing -> do
                        result <- sourcePull src
                        case result of
                            Closed -> writeRef istate EmptyClosed
                            Open _ -> return ()
                        return result
                    Just result -> return result
            , bsourceUnpull =
                \x ->
                    if null x
                        then return ()
                        else modifyRef istate $ \state ->
                            case state of
                                BOpen buffer -> (BOpen (x ++ buffer), ())
                                BClosed buffer -> (BClosed (x ++ buffer), ())
                                EmptyOpen -> (BOpen x, ())
                                EmptyClosed -> (BClosed x, ())
            , bsourceClose = do
                action <- modifyRef istate $ \state ->
                    case state of
                        BOpen x -> (BClosed x, sourceClose src)
                        BClosed _ -> (state, return ())
                        EmptyOpen -> (EmptyClosed, sourceClose src)
                        EmptyClosed -> (state, return ())
                action
            }

instance BufferSource Source where
    bufferSource (Source msrc) = msrc >>= bufferSource
