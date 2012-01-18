{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Functions for interacting with bytes.
module Data.Conduit.Binary
    ( sourceFile
    , sourceHandle
    , sourceIOHandle
    , sourceFileRange
    , sinkFile
    , sinkHandle
    , sinkIOHandle
    , conduitFile
    , isolate
    , openFile
    , head
    , takeWhile
    , dropWhile
    , take
    ) where

import Prelude hiding (head, take, takeWhile, dropWhile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Exception (assert)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import qualified System.IO as IO
import Control.Monad.Trans.Resource
    ( withIO, release, newRef, readRef, writeRef
    )
import Data.Word (Word8)
#if CABAL_OS_WINDOWS
import qualified System.Win32File as F
#elif NO_HANDLES
import qualified System.PosixFile as F
#endif

-- | Open a file 'IO.Handle' safely by automatically registering a release
-- action.
--
-- While you are not required to call @hClose@ on the resulting handle, you
-- should do so as early as possible to free scarce resources.
--
-- Since 0.0.2
openFile :: ResourceIO m
         => FilePath
         -> IO.IOMode
         -> ResourceT m IO.Handle
openFile fp mode = fmap snd $ withIO (IO.openBinaryFile fp mode) IO.hClose

-- | Stream the contents of a file as binary data.
--
-- Since 0.0.0
sourceFile :: ResourceIO m
           => FilePath
           -> Source m S.ByteString
sourceFile fp =
#if CABAL_OS_WINDOWS || NO_HANDLES
    sourceIO (F.openRead fp)
             F.close
             (liftIO . F.read)
#else
    sourceIOHandle (IO.openBinaryFile fp IO.ReadMode)
#endif

-- | Stream the contents of a 'IO.Handle' as binary data. Note that this
-- function will /not/ automatically close the @Handle@ when processing
-- completes, since it did not acquire the @Handle@ in the first place.
--
-- Since 0.0.2.
sourceHandle :: ResourceIO m
             => IO.Handle
             -> Source m S.ByteString
sourceHandle h = Source $ return $ PreparedSource
    { sourcePull = do
        bs <- liftIO (S.hGetSome h 4096)
        if S.null bs
            then return Closed
            else return (Open bs)
    , sourceClose = return ()
    }

-- | An alternative to 'sourceHandle'.
-- Instead of taking a pre-opened 'IO.Handle', it takes an action that opens
-- a 'IO.Handle' (in read mode), so that it can open it only when needed
-- and close it as soon as possible.
sourceIOHandle :: ResourceIO m
               => IO IO.Handle
               -> Source m S.ByteString
sourceIOHandle alloc = sourceIO alloc IO.hClose
    (\handle -> do
        bs <- liftIO (S.hGetSome handle 4096)
        if S.null bs
            then return Closed
            else return $ Open bs)

-- | Stream all incoming data to the given 'IO.Handle'. Note that this function
-- will /not/ automatically close the @Handle@ when processing completes.
--
-- Since 0.0.2.
sinkHandle :: ResourceIO m
           => IO.Handle
           -> Sink S.ByteString m ()
sinkHandle h = Sink $ return $ SinkData
    { sinkPush = \input -> liftIO (S.hPut h input) >> return Processing
    , sinkClose = return ()
    }

-- | An alternative to 'sinkHandle'.
-- Instead of taking a pre-opened 'IO.Handle', it takes an action that opens
-- a 'IO.Handle' (in write mode), so that it can open it only when needed
-- and close it as soon as possible.
sinkIOHandle :: ResourceIO m
             => IO IO.Handle
             -> Sink S.ByteString m ()
sinkIOHandle alloc = sinkIO alloc IO.hClose
    (\handle bs -> liftIO (S.hPut handle bs) >> return Processing)
    (const $ return ())

-- | Stream the contents of a file as binary data, starting from a certain
-- offset and only consuming up to a certain number of bytes.
--
-- Since 0.0.0
sourceFileRange :: ResourceIO m
                => FilePath
                -> Maybe Integer -- ^ Offset
                -> Maybe Integer -- ^ Maximum count
                -> Source m S.ByteString
sourceFileRange fp offset count = Source $ do
    (key, handle) <- withIO (IO.openBinaryFile fp IO.ReadMode) IO.hClose
    case offset of
        Nothing -> return ()
        Just off -> liftIO $ IO.hSeek handle IO.AbsoluteSeek off
    pull <-
        case count of
            Nothing -> return $ pullUnlimited handle key
            Just c -> do
                ic <- newRef c
                return $ pullLimited ic handle key
    return PreparedSource
        { sourcePull = pull
        , sourceClose = release key
        }
  where
    pullUnlimited handle key = do
        bs <- liftIO $ S.hGetSome handle 4096
        if S.null bs
            then do
                release key
                return Closed
            else return $ Open bs
    pullLimited ic handle key = do
        c <- fmap fromInteger $ readRef ic
        bs <- liftIO $ S.hGetSome handle (min c 4096)
        let c' = c - S.length bs
        assert (c' >= 0) $
            if S.null bs
                then do
                    release key
                    return Closed
                else do
                    writeRef ic $ toInteger c'
                    return $ Open bs

-- | Stream all incoming data to the given file.
--
-- Since 0.0.0
sinkFile :: ResourceIO m
         => FilePath
         -> Sink S.ByteString m ()
sinkFile fp = sinkIOHandle (IO.openBinaryFile fp IO.WriteMode)

-- | Stream the contents of the input to a file, and also send it along the
-- pipeline. Similar in concept to the Unix command @tee@.
--
-- Since 0.0.0
conduitFile :: ResourceIO m
            => FilePath
            -> Conduit S.ByteString m S.ByteString
conduitFile fp = conduitIO
    (IO.openBinaryFile fp IO.WriteMode)
    IO.hClose
    (\handle bs -> do
        liftIO $ S.hPut handle bs
        return $ Producing [bs])
    (const $ return [])

-- | Ensure that only up to the given number of bytes are consume by the inner
-- sink. Note that this does /not/ ensure that all of those bytes are in fact
-- consumed.
--
-- Since 0.0.0
isolate :: Resource m
        => Int
        -> Conduit S.ByteString m S.ByteString
isolate count0 = conduitState
    count0
    push
    close
  where
    push 0 bs = return (0, Finished (Just bs) [])
    push count bs = do
        let (a, b) = S.splitAt count bs
        let count' = count - S.length a
        return (count',
            if count' == 0
                then Finished (if S.null b then Nothing else Just b) (if S.null a then [] else [a])
                else assert (S.null b) $ Producing [a])
    close _ = return []

-- | Return the next byte from the stream, if available.
--
-- Since 0.0.2
head :: Resource m => Sink S.ByteString m (Maybe Word8)
head = Sink $ return $ SinkData
    { sinkPush = \bs ->
        case S.uncons bs of
            Nothing -> return Processing
            Just (w, bs') -> do
                let lo = if S.null bs' then Nothing else Just bs'
                return $ Done lo (Just w)
    , sinkClose = return Nothing
    }

-- | Return all bytes while the predicate returns @True@.
--
-- Since 0.0.2
takeWhile :: Resource m => (Word8 -> Bool) -> Conduit S.ByteString m S.ByteString
takeWhile p = Conduit $ return $ PreparedConduit
    { conduitPush = \bs -> do
        let (x, y) = S.span p bs
        return $
            if S.null y
                then Producing [x]
                else Finished (Just y) (if S.null x then [] else [x])
    , conduitClose = return []
    }

-- | Ignore all bytes while the predicate returns @True@.
--
-- Since 0.0.2
dropWhile :: Resource m => (Word8 -> Bool) -> Sink S.ByteString m ()
dropWhile p = Sink $ return $ SinkData
    { sinkPush = \bs -> do
        let bs' = S.dropWhile p bs
        return $
            if S.null bs'
                then Processing
                else Done (Just bs') ()
    , sinkClose = return ()
    }

-- | Take the given number of bytes, if available.
--
-- Since 0.0.3
take :: Resource m => Int -> Sink S.ByteString m L.ByteString
take n = L.fromChunks `liftM` (isolate n =$ CL.consume)
