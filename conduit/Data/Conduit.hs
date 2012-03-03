{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

      -- ** Source
      module Data.Conduit.Types.Source
      -- *** Buffering
    , BufferedSource
    , bufferSource
    , unbufferSource
    , bsourceClose
      -- *** Unifying
    , IsSource
      -- ** Sink
    , module Data.Conduit.Types.Sink
      -- ** Conduit
    , module Data.Conduit.Types.Conduit
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
import Data.Conduit.Types.Source
import Data.Conduit.Util.Source
import Data.Conduit.Types.Sink
import Data.Conduit.Util.Sink
import Data.Conduit.Types.Conduit
import Data.Conduit.Util.Conduit

-- $typeOverview

infixr 0 $$

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- There are three ways this process can terminate:
--
-- 1. In the case of a @SinkNoData@ constructor, the source is not opened at
-- all, and the output value is returned immediately.
--
-- 2. The sink returns @Done@. If the input was a @BufferedSource@, any
-- leftover input is put in the buffer. For a normal @Source@, the leftover
-- value is discarded, and the source is closed.
--
-- 3. The source return @Closed@, in which case the sink is closed.
--
-- Note that this function will automatically close any @Source@s, but will not
-- close any @BufferedSource@s, allowing them to be reused.
--
-- Since 0.2.0
($$) :: IsSource src m => src m a -> Sink a m b -> m b
($$) = connect
{-# INLINE ($$) #-}

-- | A typeclass allowing us to unify operators for 'Source' and
-- 'BufferedSource'.
--
-- Since 0.2.0
class IsSource src m where
    connect :: src m a -> Sink a m b -> m b
    fuseLeft :: src m a -> Conduit a m b -> Source m b

instance Monad m => IsSource Source m where
    connect = normalConnect
    {-# INLINE connect #-}
    fuseLeft = normalFuseLeft
    {-# INLINE fuseLeft #-}

instance MonadIO m => IsSource BufferedSource m where
    connect = bufferedConnect
    {-# INLINE connect #-}
    fuseLeft = bufferedFuseLeft
    {-# INLINE fuseLeft #-}

normalConnect :: Monad m => Source m a -> Sink a m b -> m b
normalConnect src (Done _leftover output) = sourceClose src >> return output
normalConnect src (SinkM msink) = msink >>= normalConnect src
normalConnect (SourceM msrc _) sink = msrc >>= \src -> normalConnect src sink
normalConnect Closed (Processing _ close) = close
normalConnect (Open src _ a) (Processing push _) = normalConnect src $ push a

data FuseLeftState srcState input m output =
    FLClosed
  | FLOpen srcState (ConduitPush input m output) (ConduitClose m output)
  | FLHaveMore srcState (ConduitPull input m output) (m ())

infixl 1 $=

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Note that any @Source@ passed in will be automatically closed, while a
-- @BufferedSource@ will be left open.
--
-- Since 0.2.0
($=) :: IsSource src m
     => src m a
     -> Conduit a m b
     -> Source m b
($=) = fuseLeft
{-# INLINE ($=) #-}

normalFuseLeft :: Monad m => Source m a -> Conduit a m b -> Source m b
normalFuseLeft Closed (Conduit _ close) = close
normalFuseLeft (SourceM msrc closeS) conduit@(Conduit _ closeC) =
    SourceM (liftM (flip normalFuseLeft conduit) msrc) $ do
        closeS
        sourceClose closeC
normalFuseLeft (Open src closeS a) (Conduit push closeC0) =
    SourceM (liftM goRes $ push a) (closeS >> sourceClose closeC0)
  where
    goRes (Running push' close') = normalFuseLeft src (Conduit push' close')
    goRes (Finished _leftover) = SourceM (closeS >> return Closed) closeS
    goRes (HaveMore pullC closeC output) = Open
        (pull pullC closeC)
        (closeS >> closeC)
        output

    pull pullC closeC = SourceM (liftM goRes pullC) (closeS >> closeC)

infixr 0 =$

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Since 0.2.0
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
Conduit _ close =$ Done _mleftover res = SinkM $ do
    () <- sourceClose close
    return $ Done Nothing res
conduit =$ SinkM msink = SinkM (liftM (conduit =$) msink)
Conduit initPushO initCloseO =$ Processing initPushI initCloseI = Processing
    (SinkM . push initPushI initCloseI initPushO)
    (close initPushI initCloseI initCloseO)
  where
    push :: Monad m
         => SinkPush b m c
         -> SinkClose m c
         -> ConduitPush a m b
         -> a
         -> m (Sink a m c)
    push pushI closeI pushO inputO = pushO inputO >>= goRes pushI closeI

    close :: Monad m
          => SinkPush b m c
          -> SinkClose m c
          -> ConduitClose m b
          -> m c
    close pushI closeI closeO = closeO $$ Processing pushI closeI

    goRes :: Monad m
          => SinkPush b m c
          -> SinkClose m c
          -> ConduitResult a m b
          -> m (Sink a m c)
    goRes pushI closeI (Running pushO closeO) = return $ Processing
        (SinkM . push pushI closeI pushO)
        (close pushI closeI closeO)
    goRes _ closeI (Finished leftoverO) = do
        outputI <- closeI
        return $ Done leftoverO outputI
    goRes pushI _ (HaveMore pullO closeO inputI) = goResInner pullO closeO $ pushI inputI

    goResInner :: Monad m
               => ConduitPull a m b
               -> m ()
               -> Sink b m c
               -> m (Sink a m c)
    goResInner pullO _ (Processing pushI closeI) = pullO >>= goRes pushI closeI
    goResInner _ closeO (Done _leftoverI outputI) = do
        () <- closeO
        return $ Done Nothing outputI
    goResInner pullO closeO (SinkM msink) = msink >>= goResInner pullO closeO

infixr 0 =$=

-- | Middle fuse, combining two conduits together into a new conduit.
--
-- Since 0.2.0
(=$=) :: Monad m => Conduit a m b -> Conduit b m c -> Conduit a m c
Conduit initPushO initCloseO =$= Conduit initPushI initCloseI = Conduit
    (push initPushO initPushI initCloseI)
    (close initCloseO initPushI initCloseI)
  where
    push :: Monad m
         => ConduitPush a m b
         -> ConduitPush b m c
         -> ConduitClose  m c
         -> ConduitPush a m c
    push pushO pushI closeI inputO = pushO inputO >>= goResOuter pushI closeI

    close :: Monad m
          => ConduitClose  m b
          -> ConduitPush b m c
          -> ConduitClose  m c
          -> ConduitClose  m c
    close closeO pushI closeI = closeO $= Conduit pushI closeI

    goResOuter :: Monad m
               => ConduitPush b m c
               -> ConduitClose  m c
               -> ConduitResult a m b
               -> m (ConduitResult a m c)
    goResOuter pushI closeI (Running pushO closeO) = return $ Running
        (push pushO pushI closeI)
        (close closeO pushI closeI)
    goResOuter _ closeI (Finished leftoverO) = do
        () <- sourceClose closeI
        return $ Finished leftoverO
    goResOuter pushI _ (HaveMore pullO closeO inputI) = pushI inputI >>= goResInner pullO closeO

    goResInner :: Monad m
               => ConduitPull a m b
               -> m ()
               -> ConduitResult b m c
               -> m (ConduitResult a m c)
    goResInner pullO _ (Running pushI closeI) = pullO >>= goResOuter pushI closeI
    goResInner _ closeO (Finished _leftoverI) = do
        () <- closeO
        return $ Finished Nothing
    goResInner pullO closeO (HaveMore pullI closeI outputI) = return $ HaveMore
        (pullI >>= goResInner pullO closeO)
        (do
            () <- closeO
            () <- closeI
            return ())
        outputI

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
-- Since 0.2.0
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
-- Since 0.2.0
bufferSource :: MonadIO m => Source m a -> m (BufferedSource m a)
bufferSource src = liftM BufferedSource $ liftIO $ I.newIORef $ OpenEmpty src

-- | Turn a 'BufferedSource' into a 'Source'. Note that in general this will
-- mean your original 'BufferedSource' will be closed. Additionally, all
-- leftover data from usage of the returned @Source@ will be discarded. In
-- other words: this is a no-going-back move.
--
-- Note: @bufferSource@ . @unbufferSource@ is /not/ the identity function.
--
-- Since 0.2.0
unbufferSource :: MonadIO m
               => BufferedSource m a
               -> Source m a
unbufferSource (BufferedSource bs) =
    SourceM msrc (msrc >>= sourceClose)
  where
    msrc = do
        buf <- liftIO $ I.readIORef bs
        case buf of
            OpenEmpty src -> return src
            OpenFull src a -> return $ Open src (sourceClose src) a
            ClosedEmpty -> return Closed
            ClosedFull a -> return $ Open Closed (return ()) a

bufferedConnect :: MonadIO m => BufferedSource m a -> Sink a m b -> m b
bufferedConnect _ (Done Nothing output) = return output
bufferedConnect _ (Done Just{} _) = error "Invariant violated: sink returned leftover without input"
bufferedConnect bsrc (SinkM msink) = msink >>= bufferedConnect bsrc
bufferedConnect (BufferedSource bs) (Processing push0 close0) = do
    bsState <- liftIO $ I.readIORef bs
    case bsState of
        ClosedEmpty -> close0
        OpenEmpty src -> connect' src push0 close0
        ClosedFull a -> onRes Nothing $ push0 a
        OpenFull src a -> onRes (Just src) $ push0 a
  where
    connect' Closed _ close = do
        liftIO $ I.writeIORef bs ClosedEmpty
        close
    connect' (Open src _ x) push _ = onRes (Just src) $ push x
    connect' (SourceM msrc _) push close = msrc >>= \src -> connect' src push close

    onRes msrc (Done mleftover res) = do
        let state =
                case (msrc, mleftover) of
                    (Nothing, Nothing) -> ClosedEmpty
                    (Just src, Nothing) -> OpenEmpty src
                    (Nothing, Just leftover) -> ClosedFull leftover
                    (Just src, Just leftover) -> OpenFull src leftover
        liftIO $ I.writeIORef bs state
        return res
    onRes Nothing (Processing _ close) = do
        liftIO $ I.writeIORef bs ClosedEmpty
        close
    onRes (Just src) (Processing push close) = connect' src push close
    onRes msrc (SinkM msink) = msink >>= onRes msrc

bufferedFuseLeft
    :: MonadIO m
    => BufferedSource m a
    -> Conduit a m b
    -> Source m b
bufferedFuseLeft bsrc (Conduit push0 close0) = SourceM
    (pullF $ FLOpen () push0 close0)
    (sourceClose close0)
  where
    mkSrc state = SourceM
        (pullF state)
        (closeF state)

    pullF state' =
        case state' of
            FLClosed -> return Closed
            FLHaveMore () pull _ -> pull >>= goRes
            FLOpen () push close -> do
                mres <- bsourcePull bsrc
                case mres of
                    Nothing -> return close
                    Just input -> push input >>= goRes

    goRes (Finished leftover) = do
        bsourceUnpull bsrc leftover
        return Closed
    goRes (HaveMore pull close' x) =
        let state = FLHaveMore () pull close'
         in return $ Open (mkSrc state) (closeF state) x
    goRes (Running pushI closeI) = pullF (FLOpen () pushI closeI)

    closeF state = do
        -- Normally we don't have to worry about double closing, as the
        -- invariant of a source is that close is never called twice. However,
        -- here, if the Conduit returned Finished with some data, the overall
        -- Source will return an Open while the Conduit will be Closed.
        -- Therefore, we have to do a check.
        case state of
            FLClosed -> return ()
            FLOpen () _ close -> do
                () <- sourceClose close
                return ()
            FLHaveMore () _ close -> close

bsourcePull :: MonadIO m => BufferedSource m a -> m (Maybe a)
bsourcePull (BufferedSource bs) =
    liftIO (I.readIORef bs) >>= goBuf
  where
    goBuf (OpenEmpty Closed) = liftIO $ I.writeIORef bs ClosedEmpty >> return Nothing
    goBuf (OpenEmpty (Open src _ a)) = do
        liftIO $ I.writeIORef bs $ OpenEmpty src
        return $ Just a
    goBuf (OpenEmpty (SourceM msrc _)) = msrc >>= goBuf . OpenEmpty
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
-- Since 0.2.0
bsourceClose :: MonadIO m => BufferedSource m a -> m ()
bsourceClose (BufferedSource ref) = do
    buf <- liftIO $ I.readIORef ref
    case buf of
        OpenEmpty src -> sourceClose src
        OpenFull src _ -> sourceClose src
        ClosedEmpty -> return ()
        ClosedFull _ -> return ()
-- | Provide for a stream of data that can be flushed.
--
-- A number of @Conduit@s (e.g., zlib compression) need the ability to flush
-- the stream at some point. This provides a single wrapper datatype to be used
-- in all such circumstances.
--
-- Since 0.2.0
data Flush a = Chunk a | Flush
    deriving (Show, Eq, Ord)
instance Functor Flush where
    fmap _ Flush = Flush
    fmap f (Chunk a) = Chunk (f a)
