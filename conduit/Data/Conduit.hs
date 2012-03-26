{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | The main module, exporting types, utility functions, and fuse and connect
-- operators.
module Data.Conduit
    ( -- * Types
      -- | The three core types to this package are 'Source' (the data
      -- producer), 'Sink' (the data consumer), and 'Conduit' (the data
      -- transformer). For all three types, a result will provide the next
      -- value to be used. For example, the @Open@ constructor includes a new
      -- @Source@ in it. This leads to the main invariant for all conduit code:
      -- these three types may /never/ be reused.  While some specific values
      -- may work fine with reuse, the result is generally unpredictable and
      -- should no be relied upon.
      --
      -- The user-facing API provided by the connect and fuse operators
      -- automatically addresses the low level details of pulling, pushing, and
      -- closing, and there should rarely be need to perform these actions in
      -- user code.

      module Data.Conduit.Types
      -- ** Buffering
    , BufferedSource
    , bufferSource
    , unbufferSource
    , bsourceClose
      -- ** Unifying
    , IsSource
    , -- * Connect/fuse operators
      ($$)
    , ($=)
    , (=$)
    , (=$=)
      -- * Utility functions
      -- ** Source
    , module Data.Conduit.Util.Source
      -- ** Sink
    , module Data.Conduit.Util.Sink
      -- ** Conduit
    , module Data.Conduit.Util.Conduit
      -- * Flushing
    , Flush (..)
      -- * Convenience re-exports
    , ResourceT
    , MonadResource
    , MonadThrow (..)
    , MonadUnsafeIO (..)
    , runResourceT
    ) where

import Control.Monad (liftM)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.IORef as I
import Data.Conduit.Types
import Data.Conduit.Util.Source
import Data.Conduit.Util.Sink
import Data.Conduit.Util.Conduit
import Data.Void (absurd)

-- $typeOverview

infixr 0 $$

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- There are two ways this process can terminate:
--
-- 1. If the @Sink@ is a @Done@ constructor, the @Source@ is closed.
--
-- 2. If the @Source@ is a @Closed@ constructor, the @Sink@ is closed.
--
-- This function will automatically close any @Source@s, but will not close any
-- @BufferedSource@s, allowing them to be reused. Also, leftover data will be
-- discarded when connecting a @Source@, but will be buffered when using a
-- @BufferedSource@.
--
-- Since 0.3.0
($$) :: IsSource src m a => src -> Sink a m b -> m b
($$) = connect
{-# INLINE ($$) #-}

-- | A typeclass allowing us to unify operators for 'Source' and
-- 'BufferedSource'.
--
-- Since 0.3.0
class IsSource src m a | src -> m, src -> a where
    connect :: src -> Sink a m b -> m b
    fuseLeft :: src -> Conduit a m b -> Source m b

instance Monad m => IsSource (Pipe () o m ()) m o where
    connect src sink = runPipe $ pipe src sink
    {-# INLINE connect #-}
    fuseLeft src conduit = pipe src conduit
    {-# INLINE fuseLeft #-}

instance MonadIO m => IsSource (BufferedSource m a) m a where
    connect = bufferedConnect
    {-# INLINE connect #-}
    fuseLeft = bufferedFuseLeft
    {-# INLINE fuseLeft #-}

data FuseLeftState srcState input m output =
    FLClosed
  | FLOpen srcState (input -> Pipe input output m ()) (Source m output)
  | FLHaveOutput srcState (Conduit input m output) (m ())

infixl 1 $=

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Any @Source@ passed in will be automatically closed, while a
-- @BufferedSource@ will be left open. Leftover input will be discarded for a
-- @Source@, and buffered for a @BufferedSource@.
--
-- Since 0.3.0
($=) :: IsSource src m a
     => src
     -> Conduit a m b
     -> Source m b
($=) = fuseLeft
{-# INLINE ($=) #-}

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Any leftover data returns from the @Sink@ will be discarded.
--
-- Since 0.3.0
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
(=$) = pipe

infixr 0 =$=

-- | Middle fuse, combining two conduits together into a new conduit.
--
-- Any leftovers provided by the inner @Conduit@ will be discarded.
--
-- Since 0.3.0
(=$=) :: Monad m => Conduit a m b -> Conduit b m c -> Conduit a m c
(=$=) = pipe

-- | When actually interacting with @Source@s, we sometimes want to be able to
-- buffer the output, in case any intermediate steps return leftover data. A
-- @BufferedSource@ allows for such buffering.
--
-- A @BufferedSource@, unlike a @Source@, is resumable, meaning it can be
-- passed to multiple @Sink@s without restarting. Therefore, a @BufferedSource@
-- relaxes the main invariant of this package: the same value may be used
-- multiple times.
--
-- The intention of a @BufferedSource@ is to be used internally by an
-- application or library, not to be part of its user-facing API. For example,
-- the Warp webserver uses a @BufferedSource@ internally for parsing the
-- request headers, but then passes a normal @Source@ to the web application
-- for reading the request body.
--
-- One caveat: while the types will allow you to use the buffered source in
-- multiple threads, there is no guarantee that all @BufferedSource@s will
-- handle this correctly.
--
-- Since 0.3.0
data BufferedSource m a = BufferedSource (I.IORef (BSState m a))

data BSState m a =
    ClosedEmpty
  | OpenEmpty (Source m a)
  | ClosedFull a
  | OpenFull (Source m a) a

-- | Places the given @Source@ and a buffer into a mutable variable. Note that
-- you should manually call 'bsourceClose' when the 'BufferedSource' is no
-- longer in use.
--
-- Since 0.3.0
bufferSource :: MonadIO m => Source m a -> m (BufferedSource m a)
bufferSource src = liftM BufferedSource $ liftIO $ I.newIORef $ OpenEmpty src

-- | Turn a 'BufferedSource' into a 'Source'. Note that in general this will
-- mean your original 'BufferedSource' will be closed. Additionally, all
-- leftover data from usage of the returned @Source@ will be discarded. In
-- other words: this is a no-going-back move.
--
-- Note: @bufferSource@ . @unbufferSource@ is /not/ the identity function.
--
-- Since 0.3.0
unbufferSource :: MonadIO m
               => BufferedSource m a
               -> Source m a
unbufferSource (BufferedSource bs) =
    PipeM msrc (msrc >>= pipeClose)
  where
    msrc = do
        buf <- liftIO $ I.readIORef bs
        case buf of
            OpenEmpty src -> return src
            OpenFull src a -> return $ HaveOutput src (pipeClose src) a
            ClosedEmpty -> return $ Done Nothing ()
            ClosedFull a -> return $ HaveOutput (Done Nothing ()) (return ()) a

bufferedConnect :: MonadIO m => BufferedSource m a -> Sink a m b -> m b
bufferedConnect _ (Done Nothing output) = return output
bufferedConnect _ (Done Just{} _) = error "Invariant violated: sink returned leftover without input"
bufferedConnect bsrc (PipeM msink _) = msink >>= bufferedConnect bsrc
bufferedConnect _ (HaveOutput _ _ o) = absurd o
bufferedConnect (BufferedSource bs) (NeedInput push0 close0) = do
    bsState <- liftIO $ I.readIORef bs
    case bsState of
        ClosedEmpty -> pipeClose close0
        OpenEmpty src -> connect' src push0 close0
        ClosedFull a -> onRes Nothing $ push0 a
        OpenFull src a -> onRes (Just src) $ push0 a
  where
    connect' (Done _ ()) _ close = do
        liftIO $ I.writeIORef bs ClosedEmpty
        pipeClose close
    connect' (HaveOutput src _ x) push _ = onRes (Just src) (push x)
    connect' (PipeM msrc _) push close = msrc >>= \src -> connect' src push close
    connect' (NeedInput p _) push close = connect' (p ()) push close

    onRes msrc (Done mleftover res) = do
        let state =
                case (msrc, mleftover) of
                    (Nothing, Nothing) -> ClosedEmpty
                    (Just src, Nothing) -> OpenEmpty src
                    (Nothing, Just leftover) -> ClosedFull leftover
                    (Just src, Just leftover) -> OpenFull src leftover
        liftIO $ I.writeIORef bs state
        return res
    onRes Nothing (NeedInput _ close) = do
        liftIO $ I.writeIORef bs ClosedEmpty
        pipeClose close
    onRes (Just src) (NeedInput push close) = connect' src push close
    onRes msrc (PipeM msink _) = msink >>= onRes msrc
    onRes _ (HaveOutput _ _ o) = absurd o

bufferedFuseLeft
    :: MonadIO m
    => BufferedSource m a
    -> Conduit a m b
    -> Source m b
bufferedFuseLeft bsrc (PipeM mcon close) = PipeM
    (liftM (bufferedFuseLeft bsrc) mcon)
    close
bufferedFuseLeft _ (Done _ ()) = Done Nothing ()
bufferedFuseLeft bsrc (HaveOutput next close x) = HaveOutput
    (bufferedFuseLeft bsrc next)
    close
    x
bufferedFuseLeft bsrc (NeedInput push0 close0) = PipeM
    (pullF $ FLOpen () push0 close0)
    (pipeClose close0)
  where
    mkSrc state = PipeM
        (pullF state)
        (closeF state)

    pullF state' =
        case state' of
            FLClosed -> return  $ Done Nothing ()
            FLHaveOutput () pull _ -> goRes pull
            FLOpen () push close -> do
                mres <- bsourcePull bsrc
                case mres of
                    Nothing -> return close
                    Just input -> goRes $ push input

    goRes (Done leftover ()) = do
        bsourceUnpull bsrc leftover
        return $ Done Nothing ()
    goRes (HaveOutput pull close' x) =
        let state = FLHaveOutput () pull close'
         in return $ HaveOutput (mkSrc state) (closeF state) x
    goRes (NeedInput pushI closeI) = pullF (FLOpen () pushI closeI)
    goRes (PipeM mcon _) = mcon >>= goRes

    closeF state = do
        -- Normally we don't have to worry about double closing, as the
        -- invariant of a source is that close is never called twice. However,
        -- here, if the Conduit returned Finished with some data, the overall
        -- Source will return an Open while the Conduit will be Closed.
        -- Therefore, we have to do a check.
        case state of
            FLClosed -> return ()
            FLOpen () _ close -> do
                () <- pipeClose close
                return ()
            FLHaveOutput () _ close -> close

bsourcePull :: MonadIO m => BufferedSource m a -> m (Maybe a)
bsourcePull (BufferedSource bs) =
    liftIO (I.readIORef bs) >>= goBuf
  where
    goBuf (OpenEmpty (NeedInput p _)) = goBuf (OpenEmpty (p ()))
    goBuf (OpenEmpty (Done _ ())) = liftIO $ I.writeIORef bs ClosedEmpty >> return Nothing
    goBuf (OpenEmpty (HaveOutput src _ a)) = do
        liftIO $ I.writeIORef bs $ OpenEmpty src
        return $ Just a
    goBuf (OpenEmpty (PipeM msrc _)) = msrc >>= goBuf . OpenEmpty
    goBuf ClosedEmpty = return Nothing
    goBuf (OpenFull src a) = do
        liftIO $ I.writeIORef bs (OpenEmpty src)
        return $ Just a
    goBuf (ClosedFull a) = do
        liftIO $ I.writeIORef bs ClosedEmpty
        return $ Just a

bsourceUnpull :: MonadIO m => BufferedSource m a -> Maybe a -> m ()
bsourceUnpull _ Nothing = return ()
bsourceUnpull (BufferedSource ref) (Just a) = do
    buf <- liftIO $ I.readIORef ref
    case buf of
        OpenEmpty src -> liftIO $ I.writeIORef ref (OpenFull src a)
        ClosedEmpty -> liftIO $ I.writeIORef ref (ClosedFull a)
        _ -> error $ "Invariant violated: bsourceUnpull called on full data"

-- | Close the underlying 'Source' for the given 'BufferedSource'. Note
-- that this function can safely be called multiple times, as it will first
-- check if the 'Source' was previously closed.
--
-- Since 0.3.0
bsourceClose :: MonadIO m => BufferedSource m a -> m ()
bsourceClose (BufferedSource ref) = do
    buf <- liftIO $ I.readIORef ref
    case buf of
        OpenEmpty src -> pipeClose src
        OpenFull src _ -> pipeClose src
        ClosedEmpty -> return ()
        ClosedFull _ -> return ()

-- | Provide for a stream of data that can be flushed.
--
-- A number of @Conduit@s (e.g., zlib compression) need the ability to flush
-- the stream at some point. This provides a single wrapper datatype to be used
-- in all such circumstances.
--
-- Since 0.3.0
data Flush a = Chunk a | Flush
    deriving (Show, Eq, Ord)
instance Functor Flush where
    fmap _ Flush = Flush
    fmap f (Chunk a) = Chunk (f a)
