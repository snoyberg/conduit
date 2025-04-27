{-# LANGUAGE CPP, RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | /NOTE/ It is recommended to start using "Data.Conduit.Combinators" instead
-- of this module.
--
-- Functions for interacting with bytes.
--
-- For many purposes, it's recommended to use the conduit-combinators library,
-- which provides a more complete set of functions.
module Data.Conduit.Binary
    ( -- * Files and @Handle@s

      -- | Note that most of these functions live in the @MonadResource@ monad
      -- to ensure resource finalization even in the presence of exceptions. In
      -- order to run such code, you will need to use @runResourceT@.

      -- ** Sources
      CC.sourceFile
    , CC.sourceHandle
    , CC.sourceHandleUnsafe
    , CC.sourceIOHandle
    , sourceFileRange
    , sourceHandleRange
    , sourceHandleRangeWithBuffer
    , CC.withSourceFile
      -- ** Sinks
    , CC.sinkFile
    , CC.sinkFileCautious
    , CC.sinkTempFile
    , CC.sinkSystemTempFile
    , CC.sinkHandle
    , CC.sinkIOHandle
    , CC.sinkHandleBuilder
    , CC.sinkHandleFlush
    , CC.withSinkFile
    , CC.withSinkFileBuilder
    , CC.withSinkFileCautious
      -- ** Conduits
    , conduitFile
    , conduitHandle
      -- * Utilities
      -- ** Sources
    , sourceLbs
      -- ** Sinks
    , head
    , dropWhile
    , take
    , drop
    , sinkCacheLength
    , sinkLbs
    , mapM_
      -- *** Storable
    , sinkStorable
    , sinkStorableEx
      -- ** Conduits
    , isolate
    , takeWhile
    , Data.Conduit.Binary.lines
    ) where

import qualified Data.Conduit.Combinators as CC
import Prelude hiding (head, take, drop, takeWhile, dropWhile, mapM_)
import qualified Data.ByteString as S
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Conduit.List (sourceList, consume)
import Control.Exception (assert, finally)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Resource (allocate, release, MonadThrow (..))
import Control.Monad.Trans.Class (lift)
import qualified System.IO as IO
import Data.Word (Word8, Word64)
#if (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif
import System.Directory (getTemporaryDirectory, removeFile)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.ByteString.Internal (ByteString (PS), accursedUnutterablePerformIO)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
#ifndef ALLOW_UNALIGNED_ACCESS
import Foreign.Marshal (alloca, copyBytes)
#endif

-- | Stream the contents of a file as binary data, starting from a certain
-- offset and only consuming up to a certain number of bytes.
--
-- Since 0.3.0
sourceFileRange :: MonadResource m
                => FilePath
                -> Maybe Integer -- ^ Offset
                -> Maybe Integer -- ^ Maximum count
                -> ConduitT i S.ByteString m ()
sourceFileRange fp offset count = bracketP
    (IO.openBinaryFile fp IO.ReadMode)
    IO.hClose
    (\h -> sourceHandleRange h offset count)

-- | Stream the contents of a handle as binary data, starting from a certain
-- offset and only consuming up to a certain number of bytes.
--
-- Since 1.0.8
sourceHandleRange :: MonadIO m
                  => IO.Handle
                  -> Maybe Integer -- ^ Offset
                  -> Maybe Integer -- ^ Maximum count
                  -> ConduitT i S.ByteString m ()
sourceHandleRange handle offset count =
  sourceHandleRangeWithBuffer handle offset count defaultChunkSize

-- | Stream the contents of a handle as binary data, starting from a certain
-- offset and only consuming up to a certain number of bytes. This function
-- consumes chunks as specified by the buffer size.
--
-- Since 1.1.8
sourceHandleRangeWithBuffer :: MonadIO m
                  => IO.Handle
                  -> Maybe Integer -- ^ Offset
                  -> Maybe Integer -- ^ Maximum count
                  -> Int -- ^ Buffer size
                  -> ConduitT i S.ByteString m ()
sourceHandleRangeWithBuffer handle offset count buffer = do
    case offset of
        Nothing -> return ()
        Just off -> liftIO $ IO.hSeek handle IO.AbsoluteSeek off
    case count of
        Nothing -> pullUnlimited
        Just c -> pullLimited (fromInteger c)
  where
    pullUnlimited = do
        bs <- liftIO $ S.hGetSome handle buffer
        if S.null bs
            then return ()
            else do
                yield bs
                pullUnlimited

    pullLimited c = do
        bs <- liftIO $ S.hGetSome handle (min c buffer)
        let c' = c - S.length bs
        assert (c' >= 0) $
            if S.null bs
                then return ()
                else do
                    yield bs
                    pullLimited c'

-- | Stream the contents of the input to a file, and also send it along the
-- pipeline. Similar in concept to the Unix command @tee@.
--
-- Since 0.3.0
conduitFile :: MonadResource m
            => FilePath
            -> ConduitT S.ByteString S.ByteString m ()
conduitFile fp = bracketP
    (IO.openBinaryFile fp IO.WriteMode)
    IO.hClose
    conduitHandle

-- | Stream the contents of the input to a @Handle@, and also send it along the
-- pipeline. Similar in concept to the Unix command @tee@. Like @sourceHandle@,
-- does not close the handle on completion. Related to: @conduitFile@.
--
-- Since 1.0.9
conduitHandle :: MonadIO m => IO.Handle -> ConduitT S.ByteString S.ByteString m ()
conduitHandle h = awaitForever $ \bs -> liftIO (S.hPut h bs) >> yield bs

-- | Ensure that only up to the given number of bytes are consumed by the inner
-- sink. Note that this does /not/ ensure that all of those bytes are in fact
-- consumed.
--
-- Since 0.3.0
isolate :: Monad m
        => Int
        -> ConduitT S.ByteString S.ByteString m ()
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
head :: Monad m => ConduitT S.ByteString o m (Maybe Word8)
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
takeWhile :: Monad m => (Word8 -> Bool) -> ConduitT S.ByteString S.ByteString m ()
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
dropWhile :: Monad m => (Word8 -> Bool) -> ConduitT S.ByteString o m ()
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
take :: Monad m => Int -> ConduitT S.ByteString o m L.ByteString
take  0 = return L.empty
take n0 = go n0 id
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
drop :: Monad m => Int -> ConduitT S.ByteString o m ()
drop  0 = return ()
drop n0 = go n0
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
lines :: Monad m => ConduitT S.ByteString S.ByteString m ()
lines =
    loop []
  where
    loop acc = await >>= maybe (finish acc) (go acc)

    finish acc =
        let final = S.concat $ reverse acc
         in unless (S.null final) (yield final)

    go acc more =
        case S.uncons second of
            Just (_, second') -> yield (S.concat $ reverse $ first:acc) >> go [] second'
            Nothing -> loop $ more:acc
      where
        (first, second) = S.break (== 10) more

-- | Stream the chunks from a lazy bytestring.
--
-- Since 0.5.0
sourceLbs :: Monad m => L.ByteString -> ConduitT i S.ByteString m ()
sourceLbs = sourceList . L.toChunks

-- | Stream the input data into a temp file and count the number of bytes
-- present. When complete, return a new @Source@ reading from the temp file
-- together with the length of the input in bytes.
--
-- All resources will be cleaned up automatically.
--
-- Since 1.0.5
sinkCacheLength :: (MonadResource m1, MonadResource m2)
                => ConduitT S.ByteString o m1 (Word64, ConduitT i S.ByteString m2 ())
sinkCacheLength = do
    tmpdir <- liftIO getTemporaryDirectory
    (releaseKey, (fp, h)) <- allocate
        (IO.openBinaryTempFile tmpdir "conduit.cache")
        (\(fp, h) -> IO.hClose h `finally` removeFile fp)
    len <- sinkHandleLen h
    liftIO $ IO.hClose h
    cleanup <- fromMaybe (pure ()) <$> unprotect releaseKey
    return (len, CC.sourceFile fp >> void (register cleanup))
  where
    sinkHandleLen :: MonadResource m => IO.Handle -> ConduitT S.ByteString o m Word64
    sinkHandleLen h =
        loop 0
      where
        loop x =
            await >>= maybe (return x) go
          where
            go bs = do
                liftIO $ S.hPut h bs
                loop $ x + fromIntegral (S.length bs)

-- | Consume a stream of input into a lazy bytestring. Note that no lazy I\/O
-- is performed, but rather all content is read into memory strictly.
--
-- Since 1.0.5
sinkLbs :: Monad m => ConduitT S.ByteString o m L.ByteString
sinkLbs = fmap L.fromChunks consume

mapM_BS :: Monad m => (Word8 -> m ()) -> S.ByteString -> m ()
mapM_BS f (PS fptr offset len) = do
    let start = unsafeForeignPtrToPtr fptr `plusPtr` offset
        end = start `plusPtr` len
        loop ptr
            | ptr >= end = accursedUnutterablePerformIO (touchForeignPtr fptr) `seq` return ()
            | otherwise = do
                f (accursedUnutterablePerformIO (peek ptr))
                loop (ptr `plusPtr` 1)
    loop start
{-# INLINE mapM_BS #-}

-- | Perform a computation on each @Word8@ in a stream.
--
-- Since 1.0.10
mapM_ :: Monad m => (Word8 -> m ()) -> ConduitT S.ByteString o m ()
mapM_ f = awaitForever (lift . mapM_BS f)
{-# INLINE mapM_ #-}

-- | Consume some instance of @Storable@ from the incoming byte stream. In the
-- event of insufficient bytes in the stream, returns a @Nothing@ and returns
-- all unused input as leftovers.
--
-- @since 1.1.13
sinkStorable :: (Monad m, Storable a) => ConduitT S.ByteString o m (Maybe a)
sinkStorable = sinkStorableHelper Just (return Nothing)

-- | Same as 'sinkStorable', but throws a 'SinkStorableInsufficientBytes'
-- exception (via 'throwM') in the event of insufficient bytes. This can be
-- more efficient to use than 'sinkStorable' as it avoids the need to
-- construct/deconstruct a @Maybe@ wrapper in the success case.
--
-- @since 1.1.13
sinkStorableEx :: (MonadThrow m, Storable a) => ConduitT S.ByteString o m a
sinkStorableEx = sinkStorableHelper id (throwM SinkStorableInsufficientBytes)

sinkStorableHelper :: forall m a b o. (Monad m, Storable a)
                   => (a -> b)
                   -> (ConduitT S.ByteString o m b)
                   -> ConduitT S.ByteString o m b
sinkStorableHelper wrap failure = do
    start
  where
    size = sizeOf (undefined :: a)

    -- try the optimal case: next chunk has all the data we need
    start = do
        mbs <- await
        case mbs of
            Nothing -> failure
            Just bs
                | S.null bs -> start
                | otherwise ->
                    case compare (S.length bs) size of
                        LT -> do
                            -- looks like we're stuck concating
                            leftover bs
                            lbs <- take size
                            let bs' = S.concat $ L.toChunks lbs
                            case compare (S.length bs') size of
                                LT -> do
                                    leftover bs'
                                    failure
                                EQ -> process bs'
                                GT -> assert False (process bs')
                        EQ -> process bs
                        GT -> do
                            let (x, y) = S.splitAt size bs
                            leftover y
                            process x

    -- Given a bytestring of exactly the correct size, grab the value
    process bs = return $! wrap $! accursedUnutterablePerformIO $!
        unsafeUseAsCString bs (safePeek undefined . castPtr)

    safePeek :: a -> Ptr a -> IO a
#ifdef ALLOW_UNALIGNED_ACCESS
    safePeek _ = peek
#else
    safePeek val ptr = alloca (\t -> copyBytes t ptr (sizeOf val) >> peek t)
#endif
{-# INLINE sinkStorableHelper #-}

data SinkStorableException = SinkStorableInsufficientBytes
    deriving (Show, Typeable)
instance Exception SinkStorableException
