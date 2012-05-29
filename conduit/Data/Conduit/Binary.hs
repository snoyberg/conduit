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
    , Data.Conduit.Binary.lines
    ) where

import Prelude hiding (head, take, takeWhile, dropWhile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Control.Exception (assert)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified System.IO as IO
import Control.Monad.Trans.Resource (allocate, release)
import Data.Word (Word8)
import Data.Monoid (mempty)
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
-- Since 0.3.0
openFile :: MonadResource m
         => FilePath
         -> IO.IOMode
         -> m IO.Handle
openFile fp mode = liftM snd $ allocate (IO.openBinaryFile fp mode) IO.hClose

-- | Stream the contents of a file as binary data.
--
-- Since 0.3.0
sourceFile :: MonadResource m
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
-- Since 0.3.0
sourceHandle :: MonadIO m
             => IO.Handle
             -> Source m S.ByteString
sourceHandle h =
    src
  where
    src = PipeM pull close

    pull = do
        bs <- liftIO (S.hGetSome h 4096)
        if S.null bs
            then return $ Done ()
            else return $ HaveOutput src close bs

    close = return ()

-- | An alternative to 'sourceHandle'.
-- Instead of taking a pre-opened 'IO.Handle', it takes an action that opens
-- a 'IO.Handle' (in read mode), so that it can open it only when needed
-- and close it as soon as possible.
--
-- Since 0.3.0
sourceIOHandle :: MonadResource m
               => IO IO.Handle
               -> Source m S.ByteString
sourceIOHandle alloc = sourceIO alloc IO.hClose
    (\handle -> do
        bs <- liftIO (S.hGetSome handle 4096)
        if S.null bs
            then return IOClosed
            else return $ IOOpen bs)

-- | Stream all incoming data to the given 'IO.Handle'. Note that this function
-- will /not/ automatically close the @Handle@ when processing completes.
--
-- Since 0.3.0
sinkHandle :: MonadIO m
           => IO.Handle
           -> Sink S.ByteString m ()
sinkHandle h =
    NeedInput push close
  where
    push input = PipeM
        (liftIO (S.hPut h input) >> return (NeedInput push close))
        (return ())
    close = return ()

-- | An alternative to 'sinkHandle'.
-- Instead of taking a pre-opened 'IO.Handle', it takes an action that opens
-- a 'IO.Handle' (in write mode), so that it can open it only when needed
-- and close it as soon as possible.
--
-- Since 0.3.0
sinkIOHandle :: MonadResource m
             => IO IO.Handle
             -> Sink S.ByteString m ()
sinkIOHandle alloc = sinkIO alloc IO.hClose
    (\handle bs -> liftIO (S.hPut handle bs) >> return IOProcessing)
    (const $ return ())

-- | Stream the contents of a file as binary data, starting from a certain
-- offset and only consuming up to a certain number of bytes.
--
-- Since 0.3.0
sourceFileRange :: MonadResource m
                => FilePath
                -> Maybe Integer -- ^ Offset
                -> Maybe Integer -- ^ Maximum count
                -> Source m S.ByteString
sourceFileRange fp offset count = PipeM
    (do
        (key, handle) <- allocate (IO.openBinaryFile fp IO.ReadMode) IO.hClose
        case offset of
            Nothing -> return ()
            Just off -> liftIO $ IO.hSeek handle IO.AbsoluteSeek off
        case count of
            Nothing -> pullUnlimited handle key
            Just c -> pullLimited c handle key)
    (return ())
  where
    pullUnlimited handle key = do
        bs <- liftIO $ S.hGetSome handle 4096
        if S.null bs
            then do
                release key
                return $ Done ()
            else do
                let src = PipeM
                        (pullUnlimited handle key)
                        (release key)
                return $ HaveOutput src (release key) bs

    pullLimited c0 handle key = do
        let c = fromInteger c0
        bs <- liftIO $ S.hGetSome handle (min c 4096)
        let c' = c - S.length bs
        assert (c' >= 0) $
            if S.null bs
                then do
                    release key
                    return $ Done ()
                else do
                    let src = PipeM
                            (pullLimited (toInteger c') handle key)
                            (release key)
                    return $ HaveOutput src (release key) bs

-- | Stream all incoming data to the given file.
--
-- Since 0.3.0
sinkFile :: MonadResource m
         => FilePath
         -> Sink S.ByteString m ()
sinkFile fp = sinkIOHandle (IO.openBinaryFile fp IO.WriteMode)

-- | Stream the contents of the input to a file, and also send it along the
-- pipeline. Similar in concept to the Unix command @tee@.
--
-- Since 0.3.0
conduitFile :: MonadResource m
            => FilePath
            -> Conduit S.ByteString m S.ByteString
conduitFile fp = conduitIO
    (IO.openBinaryFile fp IO.WriteMode)
    IO.hClose
    (\handle bs -> do
        liftIO $ S.hPut handle bs
        return $ IOProducing [bs])
    (const $ return [])

-- | Ensure that only up to the given number of bytes are consume by the inner
-- sink. Note that this does /not/ ensure that all of those bytes are in fact
-- consumed.
--
-- Since 0.3.0
isolate :: Monad m
        => Int
        -> Conduit S.ByteString m S.ByteString
isolate count0 = conduitState
    count0
    push
    close
  where
    push 0 bs = return $ StateFinished (Just bs) []
    push count bs = do
        let (a, b) = S.splitAt count bs
        let count' = count - S.length a
        return $
            if count' == 0
                then StateFinished (if S.null b then Nothing else Just b) (if S.null a then [] else [a])
                else assert (S.null b) $ StateProducing count' [a]
    close _ = return []

-- | Return the next byte from the stream, if available.
--
-- Since 0.3.0
head :: Monad m => Sink S.ByteString m (Maybe Word8)
head =
    NeedInput push close
  where
    push bs =
        case S.uncons bs of
            Nothing -> NeedInput push close
            Just (w, bs') ->
                let lo = if S.null bs' then id else flip Leftover bs'
                 in lo $ Done (Just w)
    close = return Nothing

-- | Return all bytes while the predicate returns @True@.
--
-- Since 0.3.0
takeWhile :: Monad m => (Word8 -> Bool) -> Conduit S.ByteString m S.ByteString
takeWhile p =
    NeedInput push close
  where
    push bs
        | S.null y =
            let r = NeedInput push close
             in if S.null x
                    then r
                    else HaveOutput r (return ()) x
        | otherwise =
            let f = Leftover (Done ()) y
             in if S.null x
                    then f
                    else HaveOutput f (return ()) x
      where
        (x, y) = S.span p bs
    close = mempty

-- | Ignore all bytes while the predicate returns @True@.
--
-- Since 0.3.0
dropWhile :: Monad m => (Word8 -> Bool) -> Sink S.ByteString m ()
dropWhile p =
    NeedInput push close
  where
    push bs
        | S.null bs' = NeedInput push close
        | otherwise  = Leftover (Done ()) bs'
      where
        bs' = S.dropWhile p bs
    close = return ()

-- | Take the given number of bytes, if available.
--
-- Since 0.3.0
take :: Monad m => Int -> Sink S.ByteString m L.ByteString
take n0 =
    go n0 id
  where
    go n front =
        NeedInput push close
      where
        push bs =
            case S.length bs `compare` n of
                LT -> go (n - S.length bs) (front . (bs:))
                EQ -> Done $ L.fromChunks $ front [bs]
                GT ->
                    let (x, y) = S.splitAt n bs
                     in assert (not $ S.null y) $ Leftover (Done $ L.fromChunks $ front [x]) y
        close = return $ L.fromChunks $ front []

-- | Split the input bytes into lines. In other words, split on the LF byte
-- (10), and strip it from the output.
--
-- Since 0.3.0
lines :: Monad m => Conduit S.ByteString m S.ByteString
lines = NeedInput (push id) (close S.empty)
  where
    push :: Monad m => (S.ByteString -> S.ByteString)
                    -> S.ByteString
                    -> Conduit S.ByteString m S.ByteString
    push sofar more =
        case S.uncons second of
            Just (_, second') -> HaveOutput (push id second') (return ()) (sofar first)
            Nothing ->
                let rest = sofar more
                 in NeedInput (push $ S.append rest) (close rest)
      where
        (first, second) = S.breakByte 10 more

    close rest
        | S.null rest = Done ()
        | otherwise   = HaveOutput (Done ()) (return ()) rest

