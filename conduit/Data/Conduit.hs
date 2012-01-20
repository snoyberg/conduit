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
      -- * Convenience re-exports
    , ResourceT
    , Resource (..)
    , ResourceIO
    , ResourceUnsafeIO
    , runResourceT
    , ResourceThrow (..)
    ) where

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
normalConnect (Source msrc) (Sink msink) = do
    sinkI <- msink
    case sinkI of
        SinkNoData output -> return output
        SinkData push close -> do
            src <- msrc
            connect' src push close
  where
    connect' src push close =
        loop
      where
        loop = do
            res <- sourcePull src
            case res of
                Closed -> do
                    res' <- close
                    return res'
                Open a -> do
                    mres <- push a
                    case mres of
                        Done _leftover res' -> do
                            sourceClose src
                            return res'
                        Processing -> loop

data FuseLeftState a = FLClosed [a] | FLOpen [a]

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
normalFuseLeft (Source msrc) (Conduit mc) = Source $ do
    istate <- newRef $ FLOpen [] -- still open, no buffer
    src <- msrc
    c <- mc
    return $ PreparedSource
        (pull istate src c)
        (close istate src c)
  where
    pull istate src c = do
        state' <- readRef istate
        case state' of
            FLClosed [] -> return Closed
            FLClosed (x:xs) -> do
                writeRef istate $ FLClosed xs
                return $ Open x
            FLOpen (x:xs) -> do
                writeRef istate $ FLOpen xs
                return $ Open x
            FLOpen [] -> do
                mres <- sourcePull src
                case mres of
                    Closed -> do
                        res <- conduitClose c
                        case res of
                            [] -> do
                                writeRef istate $ FLClosed []
                                return Closed
                            x:xs -> do
                                writeRef istate $ FLClosed xs
                                return $ Open x
                    Open input -> do
                        res' <- conduitPush c input
                        case res' of
                            Producing [] -> pull istate src c
                            Producing (x:xs) -> do
                                writeRef istate $ FLOpen xs
                                return $ Open x
                            Finished _leftover output -> do
                                sourceClose src
                                case output of
                                    [] -> do
                                        writeRef istate $ FLClosed []
                                        return Closed
                                    x:xs -> do
                                        writeRef istate $ FLClosed xs
                                        return $ Open x
    close istate src c = do
        -- See comment on bufferedFuseLeft for why we need to have the
        -- following check
        state <- readRef istate
        case state of
            FLClosed _ -> return ()
            FLOpen _ -> do
                _ignored <- conduitClose c
                sourceClose src

infixr 0 =$

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Since 0.0.0
(=$) :: Resource m => Conduit a m b -> Sink b m c -> Sink a m c
Conduit mc =$ Sink ms = Sink $ do
    s <- ms
    case s of
        SinkData pushI closeI -> mc >>= go pushI closeI
        SinkNoData mres -> return $ SinkNoData mres
  where
    go pushI closeI c = do
        return SinkData
            { sinkPush = \cinput -> do
                res <- conduitPush c cinput
                case res of
                    Producing sinput -> do
                        let push [] = return Processing
                            push (i:is) = do
                                mres <- pushI i
                                case mres of
                                    Processing -> push is
                                    Done _sleftover res' -> do
                                        _ <- conduitClose c
                                        return $ Done Nothing res'
                        push sinput
                    Finished cleftover sinput -> do
                        let push [] = closeI
                            push (i:is) = do
                                mres <- pushI i
                                case mres of
                                    Processing -> push is
                                    Done _sleftover res' -> return res'
                        res' <- push sinput
                        return $ Done cleftover res'
            , sinkClose = do
                sinput <- conduitClose c
                let push [] = closeI
                    push (i:is) = do
                        mres <- pushI i
                        case mres of
                            Processing -> push is
                            Done _sleftover res' -> return res'
                push sinput
            }

infixr 0 =$=

-- | Middle fuse, combining two conduits together into a new conduit.
--
-- Since 0.0.0
(=$=) :: Resource m => Conduit a m b -> Conduit b m c -> Conduit a m c
Conduit outerM =$= Conduit innerM = Conduit $ do
    outer <- outerM
    inner <- innerM
    return PreparedConduit
        { conduitPush = \inputO -> do
            res <- conduitPush outer inputO
            case res of
                Producing inputI -> do
                    let push [] front = return $ Producing $ front []
                        push (i:is) front = do
                            resI <- conduitPush inner i
                            case resI of
                                Producing c -> push is (front . (c ++))
                                Finished _leftover c -> do
                                    _ <- conduitClose outer
                                    return $ Finished Nothing $ front c
                    push inputI id
                Finished leftoverO inputI -> do
                    c <- conduitPushClose inner inputI
                    return $ Finished leftoverO c
        , conduitClose = do
            b <- conduitClose outer
            c <- conduitPushClose inner b
            return c
        }

-- | Push some data to a conduit, then close it if necessary.
conduitPushClose :: Monad m => PreparedConduit a m b -> [a] -> ResourceT m [b]
conduitPushClose c [] = conduitClose c
conduitPushClose c (input:rest) = do
    res <- conduitPush c input
    case res of
        Finished _ b -> return b
        Producing b -> do
            b' <- conduitPushClose c rest
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
data BufferedSource m a = BufferedSource
    { bsSource :: PreparedSource m a
    , bsBuffer :: Ref (Base m) (BSState a)
    }

data BSState a = ClosedEmpty | OpenEmpty | ClosedFull a | OpenFull a

-- | Prepare a 'Source' and initialize a buffer. Note that you should manually
-- call 'bsourceClose' when the 'BufferedSource' is no longer in use.
--
-- Since 0.0.0
bufferSource :: Resource m => Source m a -> ResourceT m (BufferedSource m a)
bufferSource (Source msrc) = do
    src <- msrc
    buf <- newRef OpenEmpty
    return $ BufferedSource src buf

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
               -> Source m a
unbufferSource (BufferedSource src bufRef) = Source $ do
    buf <- readRef bufRef
    case buf of
        OpenEmpty -> return src
        OpenFull a -> do
            isUsedRef <- newRef False
            return PreparedSource
                { sourcePull = do
                    isUsed <- readRef isUsedRef
                    if isUsed
                        then sourcePull src
                        else do
                            writeRef isUsedRef True
                            return $ Open a
                , sourceClose = sourceClose src
                }
        ClosedEmpty -> return PreparedSource
            -- Note: we could put some invariant checking in here if we wanted
            { sourcePull = return Closed
            , sourceClose = return ()
            }
        ClosedFull a -> do
            isUsedRef <- newRef False
            return PreparedSource
                { sourcePull = do
                    isUsed <- readRef isUsedRef
                    if isUsed
                        then return Closed
                        else do
                            writeRef isUsedRef True
                            return $ Open a
                , sourceClose = sourceClose src
                }

bufferedConnect :: Resource m => BufferedSource m a -> Sink a m b -> ResourceT m b
bufferedConnect bs (Sink msink) = do
    sinkI <- msink
    case sinkI of
        SinkNoData output -> return output
        SinkData push close -> do
            bsState <- readRef $ bsBuffer bs
            case bsState of
                ClosedEmpty -> close
                OpenEmpty -> connect' push close
                ClosedFull a -> do
                    res <- push a
                    case res of
                        Done mleftover res' -> do
                            writeRef (bsBuffer bs) $ maybe ClosedEmpty ClosedFull mleftover
                            return res'
                        Processing -> do
                            writeRef (bsBuffer bs) ClosedEmpty
                            close
                OpenFull a -> push a >>= onRes (connect' push close)
  where
    connect' push close =
        loop
      where
        loop = do
            res <- sourcePull $ bsSource bs
            case res of
                Closed -> do
                    writeRef (bsBuffer bs) ClosedEmpty
                    res' <- close
                    return res'
                Open a -> push a >>= onRes loop
    onRes _ (Done mleftover res) = do
        writeRef (bsBuffer bs) (maybe OpenEmpty OpenFull mleftover)
        return res
    onRes loop Processing = loop

bufferedFuseLeft
    :: Resource m
    => BufferedSource m a
    -> Conduit a m b
    -> Source m b
bufferedFuseLeft bsrc (Conduit mc) = Source $ do
    istate <- newRef $ FLOpen [] -- still open, no buffer
    c <- mc
    return $ PreparedSource
        (pull istate c)
        (close istate c)
  where
    pull istate c = do
        state' <- readRef istate
        case state' of
            FLClosed [] -> return Closed
            FLClosed (x:xs) -> do
                writeRef istate $ FLClosed xs
                return $ Open x
            FLOpen (x:xs) -> do
                writeRef istate $ FLOpen xs
                return $ Open x
            FLOpen [] -> do
                mres <- bsourcePull bsrc
                case mres of
                    Closed -> do
                        res <- conduitClose c
                        case res of
                            [] -> do
                                writeRef istate $ FLClosed []
                                return Closed
                            x:xs -> do
                                writeRef istate $ FLClosed xs
                                return $ Open x
                    Open input -> do
                        res' <- conduitPush c input
                        case res' of
                            Producing [] -> pull istate c
                            Producing (x:xs) -> do
                                writeRef istate $ FLOpen xs
                                return $ Open x
                            Finished leftover output -> do
                                bsourceUnpull bsrc leftover
                                case output of
                                    [] -> do
                                        writeRef istate $ FLClosed []
                                        return Closed
                                    x:xs -> do
                                        writeRef istate $ FLClosed xs
                                        return $ Open x
    close istate c = do
        -- Normally we don't have to worry about double closing, as the
        -- invariant of a source is that close is never called twice. However,
        -- here, if the Conduit returned Finished with some data, the overall
        -- Source will return an Open while the Conduit will be Closed.
        -- Therefore, we have to do a check.
        state <- readRef istate
        case state of
            FLClosed _ -> return ()
            FLOpen _ -> do
                _ignored <- conduitClose c
                return ()

bsourcePull :: Resource m => BufferedSource m a -> ResourceT m (SourceResult a)
bsourcePull (BufferedSource src bufRef) = do
    buf <- readRef bufRef
    case buf of
        OpenEmpty -> do
            res <- sourcePull src
            case res of
                Open _ -> return res
                Closed -> writeRef bufRef ClosedEmpty >> return Closed
        ClosedEmpty -> return Closed
        OpenFull a -> do
            writeRef bufRef OpenEmpty
            return $ Open a
        ClosedFull a -> do
            writeRef bufRef ClosedEmpty
            return $ Open a

bsourceUnpull :: Resource m => BufferedSource m a -> Maybe a -> ResourceT m ()
bsourceUnpull _ Nothing = return ()
bsourceUnpull (BufferedSource _ bufRef) (Just a) = do
    buf <- readRef bufRef
    case buf of
        OpenEmpty -> writeRef bufRef $ OpenFull a
        ClosedEmpty -> writeRef bufRef $ ClosedFull a
        _ -> error $ "Invariant violated: bsourceUnpull called on full data"

-- | Close the underlying 'PreparedSource' for the given 'BufferedSource'. Note
-- that this function can safely be called multiple times, as it will first
-- check if the 'PreparedSource' was previously closed.
--
-- Since 0.0.0
bsourceClose :: Resource m => BufferedSource m a -> ResourceT m ()
bsourceClose (BufferedSource src bufRef) = do
    buf <- readRef bufRef
    case buf of
        OpenEmpty -> sourceClose src
        OpenFull _ -> sourceClose src
        ClosedEmpty -> return ()
        ClosedFull _ -> return ()
