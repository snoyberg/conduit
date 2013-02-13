{-# LANGUAGE CPP, RankNTypes #-}
-- | Functions for interacting with bytes.
module Data.Conduit.Binary
    ( -- * Files and @Handle@s

      -- | Note that most of these functions live in the @MonadResource@ monad
      -- to ensure resource finalization even in the presence of exceptions. In
      -- order to run such code, you will need to use @runResourceT@.

      -- ** Sources
      sourceFile
    , sourceHandle
    , sourceIOHandle
    , sourceFileRange
      -- ** Sinks
    , sinkFile
    , sinkHandle
    , sinkIOHandle
      -- ** Conduits
    , conduitFile
      -- * Utilities
      -- ** Sources
    , sourceLbs
      -- ** Sinks
    , head
    , dropWhile
    , take
    , drop
      -- ** Conduits
    , isolate
    , takeWhile
    , Data.Conduit.Binary.lines
    ) where

import Prelude hiding (head, take, drop, takeWhile, dropWhile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Conduit.List (sourceList)
import Control.Exception (assert)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified System.IO as IO
import Data.Word (Word8)
import Control.Applicative ((<$>))
#if CABAL_OS_WINDOWS
import qualified System.Win32File as F
#elif NO_HANDLES
import qualified System.PosixFile as F
#endif

-- | Stream the contents of a file as binary data.
--
-- Since 0.3.0
sourceFile :: MonadResource m
           => FilePath
           -> Producer m S.ByteString
sourceFile fp =
#if CABAL_OS_WINDOWS || NO_HANDLES
    bracketP
        (F.openRead fp)
         F.close
         loop
  where
    loop h = liftIO (F.read h) >>= maybe (return ()) (\bs -> yield bs >> loop h)
#else
    sourceIOHandle (IO.openBinaryFile fp IO.ReadMode)
#endif

-- | Stream the contents of a 'IO.Handle' as binary data. Note that this
-- function will /not/ automatically close the @Handle@ when processing
-- completes, since it did not acquire the @Handle@ in the first place.
--
-- Since 0.3.0
sourceHandle :: MonadIO m
             => IO.Handle
             -> Producer m S.ByteString
sourceHandle h =
    loop
  where
    loop = do
        bs <- liftIO (S.hGetSome h 4096)
        if S.null bs
            then return ()
            else yield bs >> loop

-- | An alternative to 'sourceHandle'.
-- Instead of taking a pre-opened 'IO.Handle', it takes an action that opens
-- a 'IO.Handle' (in read mode), so that it can open it only when needed
-- and closed it as soon as possible.
--
-- Since 0.3.0
sourceIOHandle :: MonadResource m
               => IO IO.Handle
               -> Producer m S.ByteString
sourceIOHandle alloc = bracketP alloc IO.hClose sourceHandle

-- | Stream all incoming data to the given 'IO.Handle'. Note that this function
-- will /not/ automatically close the @Handle@ when processing completes.
--
-- Since 0.3.0
sinkHandle :: MonadIO m
           => IO.Handle
           -> Consumer S.ByteString m ()
sinkHandle h = awaitForever $ liftIO . S.hPut h

-- | An alternative to 'sinkHandle'.
-- Instead of taking a pre-opened 'IO.Handle', it takes an action that opens
-- a 'IO.Handle' (in write mode), so that it can open it only when needed
-- and close it as soon as possible.
--
-- Since 0.3.0
sinkIOHandle :: MonadResource m
             => IO IO.Handle
             -> Consumer S.ByteString m ()
sinkIOHandle alloc = bracketP alloc IO.hClose sinkHandle

-- | Stream the contents of a file as binary data, starting from a certain
-- offset and only consuming up to a certain number of bytes.
--
-- Since 0.3.0
sourceFileRange :: MonadResource m
                => FilePath
                -> Maybe Integer -- ^ Offset
                -> Maybe Integer -- ^ Maximum count
                -> Producer m S.ByteString
sourceFileRange fp offset count = bracketP
    (IO.openBinaryFile fp IO.ReadMode)
    IO.hClose
    start
  where
    start handle = do
        case offset of
            Nothing -> return ()
            Just off -> liftIO $ IO.hSeek handle IO.AbsoluteSeek off
        case count of
            Nothing -> pullUnlimited handle
            Just c -> pullLimited (fromInteger c) handle

    pullUnlimited handle = do
        bs <- liftIO $ S.hGetSome handle 4096
        if S.null bs
            then return ()
            else do
                yield bs
                pullUnlimited handle

    pullLimited c handle = do
        bs <- liftIO $ S.hGetSome handle (min c 4096)
        let c' = c - S.length bs
        assert (c' >= 0) $
            if S.null bs
                then return ()
                else do
                    yield bs
                    pullLimited c' handle

-- | Stream all incoming data to the given file.
--
-- Since 0.3.0
sinkFile :: MonadResource m
         => FilePath
         -> Consumer S.ByteString m ()
sinkFile fp = sinkIOHandle (IO.openBinaryFile fp IO.WriteMode)

-- | Stream the contents of the input to a file, and also send it along the
-- pipeline. Similar in concept to the Unix command @tee@.
--
-- Since 0.3.0
conduitFile :: MonadResource m
            => FilePath
            -> Conduit S.ByteString m S.ByteString
conduitFile fp = bracketP
    (IO.openBinaryFile fp IO.WriteMode)
    IO.hClose
    go
  where
    go h = awaitForever $ \bs -> liftIO (S.hPut h bs) >> yield bs

-- | Ensure that only up to the given number of bytes are consume by the inner
-- sink. Note that this does /not/ ensure that all of those bytes are in fact
-- consumed.
--
-- Since 0.3.0
isolate :: Monad m
        => Int
        -> Conduit S.ByteString m S.ByteString
isolate =
    loop
  where
    loop 0 = return ()
    loop count = do
        mbs <- await
        case mbs of
            Nothing -> return ()
            Just bs -> do
                let (a, b) = S.splitAt count bs
                case count - S.length a of
                    0 -> do
                        unless (S.null b) $ leftover b
                        yield a
                    count' -> assert (S.null b) $ yield a >> loop count'

-- | Return the next byte from the stream, if available.
--
-- Since 0.3.0
head :: Monad m => Consumer S.ByteString m (Maybe Word8)
head = do
    mbs <- await
    case mbs of
        Nothing -> return Nothing
        Just bs ->
            case S.uncons bs of
                Nothing -> head
                Just (w, bs') -> leftover bs' >> return (Just w)

-- | Return all bytes while the predicate returns @True@.
--
-- Since 0.3.0
takeWhile :: Monad m => (Word8 -> Bool) -> Conduit S.ByteString m S.ByteString
takeWhile p =
    loop
  where
    loop = await >>= maybe (return ()) go

    go bs
        | S.null x = next
        | otherwise = yield x >> next
      where
        next = if S.null y then loop else leftover y
        (x, y) = S.span p bs

-- | Ignore all bytes while the predicate returns @True@.
--
-- Since 0.3.0
dropWhile :: Monad m => (Word8 -> Bool) -> Consumer S.ByteString m ()
dropWhile p =
    loop
  where
    loop = do
        mbs <- await
        case S.dropWhile p <$> mbs of
            Nothing -> return ()
            Just bs
                | S.null bs -> loop
                | otherwise -> leftover bs

-- | Take the given number of bytes, if available.
--
-- Since 0.3.0
take :: Monad m => Int -> Consumer S.ByteString m L.ByteString
take n0 =
    go n0 id
  where
    go n front =
        await >>= maybe (return $ L.fromChunks $ front []) go'
      where
        go' bs =
            case S.length bs `compare` n of
                LT -> go (n - S.length bs) (front . (bs:))
                EQ -> return $ L.fromChunks $ front [bs]
                GT ->
                    let (x, y) = S.splitAt n bs
                     in assert (not $ S.null y) $ leftover y >> return (L.fromChunks $ front [x])

-- | Drop up to the given number of bytes.
--
-- Since 0.5.0
drop :: Monad m => Int -> Consumer S.ByteString m ()
drop =
    go
  where
    go n =
        await >>= maybe (return ()) go'
      where
        go' bs =
            case S.length bs `compare` n of
                LT -> go (n - S.length bs)
                EQ -> return ()
                GT ->
                    let y = S.drop n bs
                     in assert (not $ S.null y) $ leftover y >> return ()

-- | Split the input bytes into lines. In other words, split on the LF byte
-- (10), and strip it from the output.
--
-- Since 0.3.0
lines :: Monad m => Conduit S.ByteString m S.ByteString
lines =
    loop id
  where
    loop front = await >>= maybe (finish front) (go front)

    finish front =
        let final = front S.empty
         in unless (S.null final) (yield final)

    go sofar more =
        case S.uncons second of
            Just (_, second') -> yield (sofar first) >> go id second'
            Nothing ->
                let rest = sofar more
                 in loop $ S.append rest
      where
        (first, second) = S.breakByte 10 more

-- | Stream the chunks from a lazy bytestring.
--
-- Since 0.5.0
sourceLbs :: Monad m => L.ByteString -> Producer m S.ByteString
sourceLbs = sourceList . L.toChunks
