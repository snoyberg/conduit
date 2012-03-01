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
normalConnect _ (SinkNoData output) = return output
normalConnect src0 (SinkLift msink) = msink >>= normalConnect src0
normalConnect src0 (SinkData push0 close0) =
    connect' src0 push0 close0
  where
    connect' src push close = do
        res <- sourcePull src
        case res of
            Closed -> do
                res' <- close
                return res'
            Open src' a -> do
                mres <- push a
                case mres of
                    Done _leftover res' -> do
                        sourceClose src'
                        return res'
                    Processing push' close' -> connect' src' push' close'

data FuseLeftState src push pull close close' output =
    FLClosed [output]
  | FLOpen src push close [output]
  | FLHaveMore src pull close' [output]

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
normalFuseLeft src0 (Conduit initPush initClose) = Source
    { sourcePull = pull $ FLOpen src0 initPush initClose []
    , sourceClose = return ()
    }
  where
    mkSrc state = Source (pull state) (close state)
    pull state' =
        case state' of
            FLClosed [] -> return Closed
            FLClosed (x:xs) -> return $ Open
                (mkSrc (FLClosed xs))
                x
            FLHaveMore src pull' close' (x:xs) -> return $ Open
                (mkSrc (FLHaveMore src pull' close' xs))
                x
            FLHaveMore src pull' _ [] -> pull' >>= goRes src
            FLOpen src push close' (x:xs) -> return $ Open
                (mkSrc (FLOpen src push close' xs))
                x
            FLOpen src push close0 [] -> do
                mres <- sourcePull src
                case mres of
                    Closed -> do
                        res <- close0
                        case res of
                            [] -> return Closed
                            x:xs -> return $ Open
                                (mkSrc (FLClosed xs))
                                x
                    Open src'' input -> push input >>= goRes src''

    goRes src (Producing push' close' []) =
        pull $ FLOpen src push' close' []
    goRes src (Producing push' close' (x:xs)) = return $ Open
        (mkSrc (FLOpen src push' close' xs))
        x
    goRes src (Finished _leftover output) = do
        sourceClose src
        case output of
            [] -> return Closed
            x:xs -> return $ Open
                (mkSrc (FLClosed xs))
                x
    goRes src (HaveMore pull' close' []) =
        pull $ FLHaveMore src pull' close' []
    goRes src (HaveMore pull' close' (x:xs)) = return $ Open
        (mkSrc (FLHaveMore src pull' close' xs))
        x

    close state = do
        -- See comment on bufferedFuseLeft for why we need to have the
        -- following check
        case state of
            FLClosed _ -> return ()
            FLOpen src' _ closeC _ -> do
                _ignored <- closeC
                sourceClose src'
            FLHaveMore src' _ closeC _ -> do
                () <- closeC
                sourceClose src'

infixr 0 =$

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Since 0.2.0
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
_ =$ SinkNoData res = SinkNoData res
conduit =$ SinkLift msink = SinkLift (liftM (conduit =$) msink)
Conduit initPushC initCloseC =$ SinkData pushI0 closeI0 = SinkData
    { sinkPush = push pushI0 closeI0 initPushC
    , sinkClose = close pushI0 closeI0 initCloseC
    }
  where
    push pushI closeI pushC0 cinput =
        pushC0 cinput >>= goRes pushI closeI

    goRes pushI closeI (Producing pushC closeC sinput) = do
        let loop p c [] = return (Processing (push p c pushC) (close p c closeC))
            loop p _ (i:is) = do
                mres <- p i
                case mres of
                    Processing p' c' -> loop p' c' is
                    Done _sleftover res' -> do
                        _ <- closeC
                        return $ Done Nothing res'
        loop pushI closeI sinput
    goRes pushI closeI (Finished cleftover sinput) = do
        let loop _ c [] = c
            loop p _ (i:is) = do
                mres <- p i
                case mres of
                    Processing p' c' -> loop p' c' is
                    Done _sleftover res' -> return res'
        res' <- loop pushI closeI sinput
        return $ Done cleftover res'
    goRes pushI closeI (HaveMore pullC closeC sinput) = do
        let loop p c [] = pullC >>= goRes p c
            loop p _ (i:is) = do
                mres <- p i
                case mres of
                    Processing p' c' -> loop p' c' is
                    Done _sleftover res' -> do
                        () <- closeC
                        return $ Done Nothing res'
        loop pushI closeI sinput

    close pushI closeI closeC0 = do
        sinput <- closeC0
        let loop _ c [] = c
            loop p _ (i:is) = do
                mres <- p i
                case mres of
                    Processing p' c' -> loop p' c' is
                    Done _sleftover res' -> return res'
        loop pushI closeI sinput

infixr 0 =$=

data FuseMiddleOuterState outerPush outerClose outerPull outerClose' inputO inputI =
    FMOOpen outerPush outerClose
  | FMOClosed (Maybe inputO)
  | FMOHaveMore outerPull outerClose'

-- | Middle fuse, combining two conduits together into a new conduit.
--
-- Since 0.2.0
(=$=) :: Monad m => Conduit a m b -> Conduit b m c -> Conduit a m c
Conduit initOuterPush initOuterClose =$= (Conduit initInnerPush initInnerClose) = Conduit
    (pushF initOuterPush initInnerPush initInnerClose)
    (closeF initOuterClose initInnerClose)
  where
    pushF outerPush innerPush innerClose inputO = outerPush inputO >>= goResOuter innerPush innerClose

    goResOuter innerPush innerClose resOuter =
        case resOuter of
            Producing outerPush' outerClose' inputIs -> pushInnerF (FMOOpen outerPush' outerClose') innerPush innerClose inputIs
            Finished leftoverO inputIs -> pushInnerF (FMOClosed leftoverO) innerPush innerClose inputIs
            HaveMore outerPull' outerClose' inputIs -> pushInnerF (FMOHaveMore outerPull' outerClose') innerPush innerClose inputIs

    pushInnerF (FMOOpen outerPush outerClose) innerPush innerClose [] = return $ Producing
        (pushF outerPush innerPush innerClose)
        (closeF outerClose innerClose)
        []
    pushInnerF (FMOClosed leftoverO) _ innerClose [] = do
        outputIs <- innerClose
        return $ Finished leftoverO outputIs
    pushInnerF (FMOHaveMore outerPull _) innerPush innerClose [] = outerPull >>= goResOuter innerPush innerClose
    pushInnerF fmo innerPush _ (inputI:inputIs) =
        innerPush inputI >>= goResInner fmo inputIs

    goResInner fmo inputIs resInner =
        case resInner of
            Finished _leftoverI outputIs -> do
                leftoverO <- closeOuterLeftover fmo
                return $ Finished leftoverO outputIs
            Producing innerPush' innerClose' outputIs -> return $ HaveMore
                (pushInnerF fmo innerPush' innerClose' inputIs)
                (closeUnit fmo innerClose')
                outputIs
            HaveMore innerPull' innerClose' outputIs -> return $ HaveMore
                (pullInnerF fmo inputIs innerPull')
                (closeUnit fmo innerClose')
                outputIs

    closeOuterLeftover (FMOOpen _ outerClose) = outerClose >> return Nothing
    closeOuterLeftover (FMOClosed leftoverO) = return leftoverO
    closeOuterLeftover (FMOHaveMore _ outerClose) = outerClose >> return Nothing

    pullInnerF fmo inputIs innerPull = innerPull >>= goResInner fmo inputIs

    closeUnit fmo innerClose = closeOuterLeftover fmo >> innerClose >> return ()

    closeF outerClose innerClose = outerClose >> innerClose

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
unbufferSource (BufferedSource bs) = Source
    { sourcePull = msrc >>= sourcePull
    , sourceClose = msrc >>= sourceClose
    }
  where
    msrc = do
        buf <- liftIO $ I.readIORef bs
        case buf of
            OpenEmpty src -> return src
            OpenFull src a -> return Source
                { sourcePull = return $ Open src a
                , sourceClose = sourceClose src
                }
            ClosedEmpty -> return Source
                -- Note: we could put some invariant checking in here if we wanted
                { sourcePull = return Closed
                , sourceClose = return ()
                }
            ClosedFull a -> return Source
                { sourcePull = return $ Open
                    (Source (return Closed) (return ()))
                    a
                , sourceClose = return ()
                }

bufferedConnect :: MonadIO m => BufferedSource m a -> Sink a m b -> m b
bufferedConnect _ (SinkNoData output) = return output
bufferedConnect bsrc (SinkLift msink) = msink >>= bufferedConnect bsrc
bufferedConnect (BufferedSource bs) (SinkData push0 close0) = do
    bsState <- liftIO $ I.readIORef bs
    case bsState of
        ClosedEmpty -> close0
        OpenEmpty src -> connect' src push0 close0
        ClosedFull a -> do
            res <- push0 a
            case res of
                Done mleftover res' -> do
                    liftIO $ I.writeIORef bs $ maybe ClosedEmpty ClosedFull mleftover
                    return res'
                Processing _ close' -> do
                    liftIO $ I.writeIORef bs ClosedEmpty
                    close'
        OpenFull src a -> push0 a >>= onRes src
  where
    connect' src push close = do
        res <- sourcePull src
        case res of
            Closed -> do
                liftIO $ I.writeIORef bs ClosedEmpty
                res' <- close
                return res'
            Open src' a -> push a >>= onRes src'
    onRes src (Done mleftover res) = do
        liftIO $ I.writeIORef bs $ maybe (OpenEmpty src) (OpenFull src) mleftover
        return res
    onRes src (Processing push close) = connect' src push close

bufferedFuseLeft
    :: MonadIO m
    => BufferedSource m a
    -> Conduit a m b
    -> Source m b
bufferedFuseLeft bsrc (Conduit push0 close0) = Source
    { sourcePull = pullF $ FLOpen () push0 close0 [] -- still open, no buffer
    , sourceClose = return ()
    }
  where
    mkSrc state = Source
        (pullF state)
        (closeF state)

    pullF state' =
        case state' of
            FLClosed [] -> return Closed
            FLClosed (x:xs) -> return $ Open
                (mkSrc (FLClosed xs))
                x
            FLHaveMore () pull close (x:xs) -> return $ Open
                (mkSrc (FLHaveMore () pull close xs))
                x
            FLHaveMore () pull _ [] -> pull >>= goRes
            FLOpen () push close (x:xs) -> return $ Open
                (mkSrc (FLOpen () push close xs))
                x
            FLOpen () push close [] -> do
                mres <- bsourcePull bsrc
                case mres of
                    Nothing -> do
                        res <- close
                        case res of
                            [] -> return Closed
                            x:xs -> return $ Open
                                (mkSrc (FLClosed xs))
                                x
                    Just input -> push input >>= goRes

    goRes (Producing push' close' []) =
        pullF (FLOpen () push' close' [])
    goRes (Producing push' close' (x:xs)) = return $ Open
        (mkSrc (FLOpen () push' close' xs))
        x
    goRes (Finished leftover output) = do
        bsourceUnpull bsrc leftover
        case output of
            [] -> return Closed
            x:xs -> return $ Open
                (mkSrc (FLClosed xs))
                x
    goRes (HaveMore pull close' []) = pullF (FLHaveMore () pull close' [])
    goRes (HaveMore pull close' (x:xs)) = return $ Open
        (mkSrc (FLHaveMore () pull close' xs))
        x

    closeF state = do
        -- Normally we don't have to worry about double closing, as the
        -- invariant of a source is that close is never called twice. However,
        -- here, if the Conduit returned Finished with some data, the overall
        -- Source will return an Open while the Conduit will be Closed.
        -- Therefore, we have to do a check.
        case state of
            FLClosed _ -> return ()
            FLOpen () _ close _ -> do
                _ignored <- close
                return ()
            FLHaveMore () _ close _ -> close

bsourcePull :: MonadIO m => BufferedSource m a -> m (Maybe a)
bsourcePull (BufferedSource bs) = do
    buf <- liftIO $ I.readIORef bs
    case buf of
        OpenEmpty src -> do
            res <- sourcePull src
            case res of
                Open src' a -> do
                    liftIO $ I.writeIORef bs $ OpenEmpty src'
                    return $ Just a
                Closed -> liftIO $ I.writeIORef bs ClosedEmpty >> return Nothing
        ClosedEmpty -> return Nothing
        OpenFull src a -> do
            liftIO $ I.writeIORef bs (OpenEmpty src)
            return $ Just a
        ClosedFull a -> do
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
