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
    , unbufferSource
    ) where

import Control.Monad.Trans.Resource
import Data.Monoid (Monoid (..))
import Control.Monad (liftM)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throw)

-- | Result of pulling from a source. Either a new piece of data (@Open@), or
-- indicates that the source is now @Closed@.
--
-- Since 0.0.0
data SourceResult a = Open a | Closed
    deriving (Show, Eq, Ord)

instance Functor SourceResult where
    fmap f (Open a) = Open (f a)
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
    { sourcePull :: ResourceT m (SourceResult a)
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
--
-- A @BufferedSource@ is also known as a /resumable source/, in that it can be
-- called multiple times, and each time will provide new data. One caveat:
-- while the types will allow you to use the buffered source in multiple
-- threads, there is no guarantee that all @BufferedSource@s will handle this
-- correctly.
--
-- Since 0.0.0
data BufferedSource m a = BufferedSource
    { bsourcePull :: ResourceT m (SourceResult a)
    , bsourceUnpull :: a -> ResourceT m ()
    , bsourceClose :: ResourceT m ()
    }

-- |
-- Since 0.0.0
data SourceInvariantException = PullAfterEOF String
    deriving (Show, Typeable)
instance Exception SourceInvariantException

-- | This typeclass allows us to unify operators on 'Source' and 'BufferedSource'.
--
-- Since 0.0.0
class BufferSource s where
    bufferSource :: Resource m => s m a -> ResourceT m (BufferedSource m a)

    -- | Same as 'bufferSource', but an implementation is guaranteed that the
    -- resulting 'BufferedSource' will be used only once. As such, an
    -- implementation may implement fake buffering, such as coding
    -- 'bsourceUnpull' as a no-op.
    unsafeBufferSource :: Resource m => s m a -> ResourceT m (BufferedSource m a)
    unsafeBufferSource = bufferSource

-- | Note that this instance hides the 'bsourceClose' record, so that a
-- @BufferedSource@ remains resumable. The correct way to handle closing of a
-- resumable source would be to call @bsourceClose@ on the originally
-- @BufferedSource@, e.g.:
--
-- > bsrc <- bufferSource $ sourceFile "myfile.txt"
-- > bsrc $$ drop 5
-- > rest <- bsrc $$ consume
-- > bsourceClose bsrc
--
-- Note that the call to the @$$@ operator allocates a /new/ 'BufferedSource'
-- internally, so that when @$$@ calls @bsourceClose@ the first time, it does
-- not close the actual file, thereby allowing us to pass the same @bsrc@ to
-- the @consume@ function. Afterwards, we should call @bsourceClose@ manually
-- (though @runResourceT@ will handle it for us eventually).
instance BufferSource BufferedSource where
    bufferSource bsrc = return bsrc
        { bsourceClose = return ()
        }

-- | State of a 'BufferedSource'
data BState a = BOpen [a]
              | BClosed [a]
    deriving Show

instance BufferSource PreparedSource where
    bufferSource src = do
        istate <- newRef $ BOpen []
        return BufferedSource
            { bsourcePull = do
                mresult <- modifyRef istate $ \state ->
                    case state of
                        BOpen [] -> (state, Nothing)
                        BClosed [] -> (state, Just Closed)
                        BOpen (x:xs) -> (BOpen xs, Just $ Open x)
                        BClosed (x:xs) -> (BClosed xs, Just $ Open x)
                case mresult of
                    Nothing -> do
                        result <- sourcePull src
                        case result of
                            Closed -> writeRef istate $ BClosed []
                            Open _ -> return ()
                        return result
                    Just result -> return result
            , bsourceUnpull = \x ->
                modifyRef istate $ \state ->
                    case state of
                        BOpen buffer -> (BOpen (x : buffer), ())
                        BClosed buffer -> (BClosed (x : buffer), ())
            , bsourceClose = do
                action <- modifyRef istate $ \state ->
                    case state of
                        BOpen x -> (BClosed x, sourceClose src)
                        BClosed _ -> (state, return ())
                action
            }
    unsafeBufferSource src = return BufferedSource
        { bsourcePull = sourcePull src
        , bsourceClose = sourceClose src
        , bsourceUnpull = const $ return ()
        }

instance BufferSource Source where
    bufferSource (Source msrc) = msrc >>= bufferSource
    unsafeBufferSource (Source msrc) = msrc >>= unsafeBufferSource

-- | Turn a 'BufferedSource' into a 'Source'. Note that in general this will
-- mean your original 'BufferedSource' will be closed. Additionally, all
-- leftover data from usage of the returned @Source@ will be discarded. In
-- other words: this is a no-going-back move.
--
-- Note: @bufferSource@ . @unbufferSource@ is /not/ the identity function.
--
-- Since 0.0.1
unbufferSource :: Monad m
               => BufferedSource m a
               -> Source m a
unbufferSource (BufferedSource pull _unpull close) =
    Source $ return $ PreparedSource pull close
