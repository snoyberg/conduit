{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | The main module, exporting types, utility functions, and fuse and connect
-- operators.
module Data.Conduit
    ( -- * Types
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
    , Resource (..)
    , ResourceIO
    , ResourceUnsafeIO
    , runResourceT
    , ResourceThrow (..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource
import Data.Conduit.Types.Source
import Data.Conduit.Util.Source
import Data.Conduit.Types.Sink
import Data.Conduit.Util.Sink
import Data.Conduit.Types.Conduit
import Data.Conduit.Util.Conduit

infixr 0 $$

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- There are three ways this process can terminate:
--
-- 1. In the case of a @SinkNoData@ constructor, the source is not opened at
-- all, and the output value is returned immediately.
--
-- 2. The sink returns @Done@, in which case any leftover input is returned via
-- @bsourceUnpull@ the source is closed.
--
-- 3. The source return @Closed@, in which case the sink is closed.
--
-- Note that this function will automatically close any 'Source's, but will not
-- close any 'BufferedSource's, allowing them to be reused.
--
-- Since 0.0.0
($$) :: (IsSource src, Resource m) => src m a -> Sink a m b -> ResourceT m b
($$) = connect
{-# INLINE ($$) #-}

-- | A typeclass allowing us to unify operators for 'Source' and
-- 'BufferedSource'.
class IsSource src where
    connect :: Resource m => src m a -> Sink a m b -> ResourceT m b
    fuseLeft :: Resource m => src m a -> Conduit a m b -> Source m b

instance IsSource Source where
    connect = normalConnect
    {-# INLINE connect #-}
    fuseLeft = normalFuseLeft
    {-# INLINE fuseLeft #-}

instance IsSource BufferedSource where
    connect = bufferedConnect
    {-# INLINE connect #-}
    fuseLeft = bufferedFuseLeft
    {-# INLINE fuseLeft #-}

normalConnect :: Resource m => Source m a -> Sink a m b -> ResourceT m b
normalConnect _ (SinkNoData output) = return output
normalConnect src0 (SinkMonad msink) = lift msink >>= normalConnect src0
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

data FuseLeftState src conduit output =
    FLClosed [output]
  | FLOpen src conduit [output]

infixl 1 $=

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Since 0.0.0
($=) :: (IsSource src, Resource m)
     => src m a
     -> Conduit a m b
     -> Source m b
($=) = fuseLeft
{-# INLINE ($=) #-}

normalFuseLeft :: Resource m => Source m a -> Conduit a m b -> Source m b
normalFuseLeft src0 conduit0 = Source
    { sourcePull = pull $ FLOpen src0 conduit0 []
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
            FLOpen src conduit (x:xs) -> return $ Open
                (mkSrc (FLOpen src conduit xs))
                x
            FLOpen src conduit [] -> do
                mres <- sourcePull src
                case mres of
                    Closed -> do
                        res <- conduitClose conduit
                        case res of
                            [] -> return Closed
                            x:xs -> return $ Open
                                (mkSrc (FLClosed xs))
                                x
                    Open src'' input -> do
                        res' <- conduitPush conduit input
                        case res' of
                            Producing conduit' [] ->
                                pull $ FLOpen src'' conduit' []
                            Producing conduit' (x:xs) -> return $ Open
                                (mkSrc (FLOpen src'' conduit' xs))
                                x
                            Finished _leftover output -> do
                                sourceClose src''
                                case output of
                                    [] -> return Closed
                                    x:xs -> return $ Open
                                        (mkSrc (FLClosed xs))
                                        x
    close state = do
        -- See comment on bufferedFuseLeft for why we need to have the
        -- following check
        case state of
            FLClosed _ -> return ()
            FLOpen src' (Conduit _ closeC) _ -> do
                _ignored <- closeC
                sourceClose src'

infixr 0 =$

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Since 0.0.0
(=$) :: Resource m => Conduit a m b -> Sink b m c -> Sink a m c
_ =$ SinkNoData res = SinkNoData res
conduit =$ SinkMonad msink = SinkMonad (liftM (conduit =$) msink)
conduitOrig =$ SinkData pushI0 closeI0 = SinkData
    { sinkPush = push pushI0 closeI0 conduitOrig
    , sinkClose = close pushI0 closeI0 conduitOrig
    }
  where
    push pushI closeI conduit0 cinput = do
        res <- conduitPush conduit0 cinput
        case res of
            Producing conduit' sinput -> do
                let loop p c [] = return (Processing (push p c conduit') (close p c conduit'))
                    loop p _ (i:is) = do
                        mres <- p i
                        case mres of
                            Processing p' c' -> loop p' c' is
                            Done _sleftover res' -> do
                                _ <- conduitClose conduit'
                                return $ Done Nothing res'
                loop pushI closeI sinput
            Finished cleftover sinput -> do
                let loop _ c [] = c
                    loop p _ (i:is) = do
                        mres <- p i
                        case mres of
                            Processing p' c' -> loop p' c' is
                            Done _sleftover res' -> return res'
                res' <- loop pushI closeI sinput
                return $ Done cleftover res'
    close pushI closeI conduit = do
        sinput <- conduitClose conduit
        let loop _ c [] = c
            loop p _ (i:is) = do
                mres <- p i
                case mres of
                    Processing p' c' -> loop p' c' is
                    Done _sleftover res' -> return res'
        loop pushI closeI sinput

infixr 0 =$=

-- | Middle fuse, combining two conduits together into a new conduit.
--
-- Since 0.0.0
(=$=) :: Resource m => Conduit a m b -> Conduit b m c -> Conduit a m c
outerOrig =$= innerOrig = Conduit
    (pushF outerOrig innerOrig)
    (closeF outerOrig innerOrig)
  where
    pushF outer0 inner0 inputO = do
        res <- conduitPush outer0 inputO
        case res of
            Producing outer inputI -> do
                let loop inner [] front = return $ Producing
                        (Conduit (pushF outer inner) (closeF outer inner))
                        (front [])
                    loop inner (i:is) front = do
                        resI <- conduitPush inner i
                        case resI of
                            Producing conduit c -> loop
                                conduit
                                is
                                (front . (c ++))
                            Finished _leftover c -> do
                                _ <- conduitClose outer
                                return $ Finished Nothing $ front c
                loop inner0 inputI id
            Finished leftoverO inputI -> do
                c <- conduitPushClose inner0 inputI
                return $ Finished leftoverO c
    closeF outer inner = do
        b <- conduitClose outer
        c <- conduitPushClose inner b
        return c

-- | Push some data to a conduit, then close it if necessary.
conduitPushClose :: Monad m => Conduit a m b -> [a] -> ResourceT m [b]
conduitPushClose c [] = conduitClose c
conduitPushClose c (input:rest) = do
    res <- conduitPush c input
    case res of
        Finished _ b -> return b
        Producing conduit b -> do
            b' <- conduitPushClose conduit rest
            return $ b ++ b'

-- | When actually interacting with 'Source's, we usually want to be able to
-- buffer the output, in case any intermediate steps return leftover data. A
-- 'BufferedSource' allows for such buffering.
--
-- A 'BufferedSource', unlike a 'Source', is resumable, meaning it can be passed to
-- multiple 'Sink's without restarting.
--
-- Finally, a 'BufferedSource' relaxes one of the invariants of a 'Source':
-- pulling after an the source is closed is allowed.
--
-- A @BufferedSource@ is also known as a /resumable source/, in that it can be
-- called multiple times, and each time will provide new data. One caveat:
-- while the types will allow you to use the buffered source in multiple
-- threads, there is no guarantee that all @BufferedSource@s will handle this
-- correctly.
--
-- Since 0.0.0
data BufferedSource m a = BufferedSource (Ref (Base m) (BSState m a))

data BSState m a =
    ClosedEmpty
  | OpenEmpty (Source m a)
  | ClosedFull a
  | OpenFull (Source m a) a

-- | Prepare a 'Source' and initialize a buffer. Note that you should manually
-- call 'bsourceClose' when the 'BufferedSource' is no longer in use.
--
-- Since 0.0.0
bufferSource :: Resource m => Source m a -> ResourceT m (BufferedSource m a)
bufferSource src = BufferedSource <$> newRef (OpenEmpty src)

-- | Turn a 'BufferedSource' into a 'Source'. Note that in general this will
-- mean your original 'BufferedSource' will be closed. Additionally, all
-- leftover data from usage of the returned @Source@ will be discarded. In
-- other words: this is a no-going-back move.
--
-- Note: @bufferSource@ . @unbufferSource@ is /not/ the identity function.
--
-- Since 0.0.1
unbufferSource :: Resource m
               => BufferedSource m a
               -> ResourceT m (Source m a)
unbufferSource (BufferedSource bs) = do
    buf <- readRef bs
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

bufferedConnect :: Resource m => BufferedSource m a -> Sink a m b -> ResourceT m b
bufferedConnect _ (SinkNoData output) = return output
bufferedConnect bsrc (SinkMonad msink) = lift msink >>= bufferedConnect bsrc
bufferedConnect (BufferedSource bs) (SinkData push0 close0) = do
    bsState <- readRef bs
    case bsState of
        ClosedEmpty -> close0
        OpenEmpty src -> connect' src push0 close0
        ClosedFull a -> do
            res <- push0 a
            case res of
                Done mleftover res' -> do
                    writeRef bs $ maybe ClosedEmpty ClosedFull mleftover
                    return res'
                Processing _ close' -> do
                    writeRef bs ClosedEmpty
                    close'
        OpenFull src a -> push0 a >>= onRes src
  where
    connect' src push close = do
        res <- sourcePull src
        case res of
            Closed -> do
                writeRef bs ClosedEmpty
                res' <- close
                return res'
            Open src' a -> push a >>= onRes src'
    onRes src (Done mleftover res) = do
        writeRef bs $ maybe (OpenEmpty src) (OpenFull src) mleftover
        return res
    onRes src (Processing push close) = connect' src push close

bufferedFuseLeft
    :: Resource m
    => BufferedSource m a
    -> Conduit a m b
    -> Source m b
bufferedFuseLeft bsrc conduit0 = Source
    { sourcePull = pullF $ FLOpen () conduit0 [] -- still open, no buffer
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
            FLOpen () conduit (x:xs) -> return $ Open
                (mkSrc (FLOpen () conduit xs))
                x
            FLOpen () conduit [] -> do
                mres <- bsourcePull bsrc
                case mres of
                    Nothing -> do
                        res <- conduitClose conduit
                        case res of
                            [] -> return Closed
                            x:xs -> return $ Open
                                (mkSrc (FLClosed xs))
                                x
                    Just input -> do
                        res' <- conduitPush conduit input
                        case res' of
                            Producing conduit' [] ->
                                pullF (FLOpen () conduit' [])
                            Producing conduit' (x:xs) -> return $ Open
                                (mkSrc (FLOpen () conduit' xs))
                                x
                            Finished leftover output -> do
                                bsourceUnpull bsrc leftover
                                case output of
                                    [] -> return Closed
                                    x:xs -> return $ Open
                                        (mkSrc (FLClosed xs))
                                        x
    closeF state = do
        -- Normally we don't have to worry about double closing, as the
        -- invariant of a source is that close is never called twice. However,
        -- here, if the Conduit returned Finished with some data, the overall
        -- Source will return an Open while the Conduit will be Closed.
        -- Therefore, we have to do a check.
        case state of
            FLClosed _ -> return ()
            FLOpen () (Conduit _ close) _ -> do
                _ignored <- close
                return ()

bsourcePull :: Resource m => BufferedSource m a -> ResourceT m (Maybe a)
bsourcePull (BufferedSource bs) = do
    buf <- readRef bs
    case buf of
        OpenEmpty src -> do
            res <- sourcePull src
            case res of
                Open src' a -> do
                    writeRef bs $ OpenEmpty src'
                    return $ Just a
                Closed -> writeRef bs ClosedEmpty >> return Nothing
        ClosedEmpty -> return Nothing
        OpenFull src a -> do
            writeRef bs (OpenEmpty src)
            return $ Just a
        ClosedFull a -> do
            writeRef bs ClosedEmpty
            return $ Just a

bsourceUnpull :: Resource m => BufferedSource m a -> Maybe a -> ResourceT m ()
bsourceUnpull _ Nothing = return ()
bsourceUnpull (BufferedSource ref) (Just a) = do
    buf <- readRef ref
    case buf of
        OpenEmpty src -> writeRef ref (OpenFull src a)
        ClosedEmpty -> writeRef ref (ClosedFull a)
        _ -> error $ "Invariant violated: bsourceUnpull called on full data"

-- | Close the underlying 'Source' for the given 'BufferedSource'. Note
-- that this function can safely be called multiple times, as it will first
-- check if the 'Source' was previously closed.
--
-- Since 0.0.0
bsourceClose :: Resource m => BufferedSource m a -> ResourceT m ()
bsourceClose (BufferedSource ref) = do
    buf <- readRef ref
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
-- Since 0.1.2
data Flush a = Chunk a | Flush
    deriving (Show, Eq, Ord)
instance Functor Flush where
    fmap _ Flush = Flush
    fmap f (Chunk a) = Chunk (f a)
