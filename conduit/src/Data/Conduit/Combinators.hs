{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

-- | This module is meant as a replacement for 'Data.Conduit.List'.
-- That module follows a naming scheme which was originally inspired
-- by its enumerator roots. This module is meant to introduce a naming
-- scheme which encourages conduit best practices.
--
-- Mention take (Conduit) vs drop (Consumer)
module Data.Conduit.Combinators
    ( -- * Naming Scheme
      -- $naming_scheme

      -- * Producers
      -- ** Pure
      yieldMany
    , unfold
    , enumFromTo
    , iterate
    , repeat
    , replicate
    , sourceLazy

      -- ** Monadic
    , repeatM
    , repeatWhileM
    , replicateM

      -- ** I\/O
    , sourceFile
    , sourceFileBS
    , sourceHandle
    , sourceHandleUnsafe
    , sourceIOHandle
    , stdin
    , withSourceFile

      -- ** Filesystem
    , sourceDirectory
    , sourceDirectoryDeep

      -- * Consumers
      -- ** Pure
    , drop
    , dropE
    , dropWhile
    , dropWhileE
    , fold
    , foldE
    , foldl
    , foldl1
    , foldlE
    , foldMap
    , foldMapE
    , all
    , allE
    , any
    , anyE
    , and
    , andE
    , or
    , orE
    , asum
    , elem
    , elemE
    , notElem
    , notElemE
    , sinkLazy
    , sinkList
    , sinkVector
    , sinkVectorN
    , sinkLazyBuilder
    , sinkNull
    , awaitNonNull
    , head
    , headDef
    , headE
    , peek
    , peekE
    , last
    , lastDef
    , lastE
    , length
    , lengthE
    , lengthIf
    , lengthIfE
    , maximum
    , maximumE
    , minimum
    , minimumE
    , null
    , nullE
    , sum
    , sumE
    , product
    , productE
    , find

      -- ** Monadic
    , mapM_
    , mapM_E
    , foldM
    , foldME
    , foldMapM
    , foldMapME

      -- ** I\/O
    , sinkFile
    , sinkFileCautious
    , sinkTempFile
    , sinkSystemTempFile
    , sinkFileBS
    , sinkHandle
    , sinkIOHandle
    , print
    , stdout
    , stderr
    , withSinkFile
    , withSinkFileBuilder
    , withSinkFileCautious
    , sinkHandleBuilder
    , sinkHandleFlush

      -- * Transformers
      -- ** Pure
    , map
    , mapE
    , omapE
    , concatMap
    , concatMapE
    , take
    , takeE
    , takeWhile
    , takeWhileE
    , takeExactly
    , takeExactlyE
    , concat
    , filter
    , filterE
    , mapWhile
    , conduitVector
    , scanl
    , mapAccumWhile
    , concatMapAccum
    , intersperse
    , slidingWindow
    , chunksOfE
    , chunksOfExactlyE

      -- ** Monadic
    , mapM
    , mapME
    , omapME
    , concatMapM
    , filterM
    , filterME
    , iterM
    , scanlM
    , mapAccumWhileM
    , concatMapAccumM

      -- ** Textual
    , encodeUtf8
    , decodeUtf8
    , decodeUtf8Lenient
    , line
    , lineAscii
    , unlines
    , unlinesAscii
    , takeExactlyUntilE
    , linesUnbounded
    , linesUnboundedAscii
    , splitOnUnboundedE

      -- ** Builders
    , builderToByteString
    , unsafeBuilderToByteString
    , builderToByteStringWith
    , builderToByteStringFlush
    , builderToByteStringWithFlush
    , BufferAllocStrategy
    , allNewBuffersStrategy
    , reuseBufferStrategy

      -- * Special
    , vectorBuilder
    , mapAccumS
    , peekForever
    , peekForeverE
    ) where

-- BEGIN IMPORTS

import           Data.ByteString.Builder     (Builder, toLazyByteString, hPutBuilder)
import qualified Data.ByteString.Builder.Internal as BB (flush)
import qualified Data.ByteString.Builder.Extra as BB (runBuilder, Next(Done, More, Chunk))
import qualified Data.NonNull as NonNull
import qualified Data.Traversable
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Control.Applicative         (Alternative(..), (<$>))
import           Control.Exception           (catch, throwIO, finally, bracket, try, evaluate)
import           Control.Category            (Category (..))
import           Control.Monad               (unless, when, (>=>), liftM, forever)
import           Control.Monad.IO.Unlift     (MonadIO (..), MonadUnliftIO, withRunInIO)
import           Control.Monad.Primitive     (PrimMonad, PrimState, unsafePrimToPrim)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Resource (MonadResource, MonadThrow, allocate, throwM)
import           Data.Conduit
import           Data.Conduit.Internal       (ConduitT (..), Pipe (..))
import qualified Data.Conduit.List           as CL
import           Data.IORef
import           Data.Maybe                  (fromMaybe, isNothing, isJust)
import           Data.Monoid                 (Monoid (..))
import           Data.MonoTraversable
import qualified Data.Sequences              as Seq
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import           Data.Void                   (absurd)
import           Prelude                     (Bool (..), Eq (..), Int,
                                              Maybe (..), Either (..), Monad (..), Num (..),
                                              Ord (..), fromIntegral, maybe, either,
                                              ($), Functor (..), Enum, seq, Show, Char,
                                              otherwise, Either (..), not,
                                              ($!), succ, FilePath, IO, String)
import Data.Word (Word8)
import qualified Prelude
import qualified System.IO                   as IO
import           System.IO.Error             (isDoesNotExistError)
import           System.IO.Unsafe            (unsafePerformIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Conduit.Combinators.Stream
import Data.Conduit.Internal.Fusion
import           Data.Primitive.MutVar       (MutVar, newMutVar, readMutVar,
                                              writeMutVar)
import qualified Data.Streaming.FileRead as FR
import qualified Data.Streaming.Filesystem as F
import           GHC.ForeignPtr (mallocPlainForeignPtrBytes, unsafeForeignPtrToPtr)
import           Foreign.ForeignPtr (touchForeignPtr, ForeignPtr)
import           Foreign.Ptr (Ptr, plusPtr, minusPtr)
import           Data.ByteString.Internal (ByteString (PS), mallocByteString)
import           System.FilePath ((</>), (<.>), takeDirectory, takeFileName)
import           System.Directory (renameFile, getTemporaryDirectory, removeFile)

import qualified Data.Sequences as DTE
import           Data.Sequences (LazySequence (..))

-- Defines INLINE_RULE0, INLINE_RULE, STREAMING0, and STREAMING.
#include "fusion-macros.h"

-- END IMPORTS

-- TODO:
--
--   * The functions sourceRandom* are based on, initReplicate and
--   initRepeat have specialized versions for when they're used with
--   ($$).  How does this interact with stream fusion?
--
--   * Is it possible to implement fusion for vectorBuilder?  Since it
--   takes a Sink yielding function as an input, the rewrite rule
--   would need to trigger when that parameter looks something like
--   (\x -> unstream (...)).  I don't see anything preventing doing
--   this, but it would be quite a bit of code.

-- NOTE: Fusion isn't possible for the following operations:
--
--   * Due to a lack of leftovers:
--     - dropE, dropWhile, dropWhileE
--     - headE
--     - peek, peekE
--     - null, nullE
--     - takeE, takeWhile, takeWhileE
--     - mapWhile
--     - codeWith
--     - line
--     - lineAscii
--
--   * Due to a use of leftover in a dependency:
--     - Due to "codeWith": encodeBase64, decodeBase64, encodeBase64URL, decodeBase64URL, decodeBase16
--     - due to "CT.decode": decodeUtf8, decodeUtf8Lenient
--
--   * Due to lack of resource cleanup (e.g. bracketP):
--     - sourceDirectory
--     - sourceDirectoryDeep
--     - sourceFile
--
--   * takeExactly / takeExactlyE - no monadic bind.  Another way to
--   look at this is that subsequent streams drive stream evaluation,
--   so there's no way for the conduit to guarantee a certain amount
--   of demand from the upstream.

-- $naming_scheme
--
-- [@Functions ending with C@]: This postfix indicates a function returns a
--   'ConduitM'
--
-- [@Functions starting with o@]: This comes from \"mono-traversable\". The o
--   choice is a bit strange, but it makes sense when you realize that the
--   letter m is already used for a lot of things in Haskell.
--
-- [@Functions ending with E@]: Used when we want to apply our function to the
--   elements of the stream, not the chunks themselves. For example, if you have
--   a stream of 'Text', you want to apply the function to the 'Char' elements
--   inside the 'Text', not the Text itself.
--

-- | Yield each of the values contained by the given @MonoFoldable@.
--
-- This will work on many data structures, including lists, @ByteString@s, and @Vector@s.
--
-- Subject to fusion
--
-- @since 1.3.0
yieldMany, yieldManyC :: (Monad m, MonoFoldable mono)
                      => mono
                      -> ConduitT i (Element mono) m ()
yieldManyC = ofoldMap yield
{-# INLINE yieldManyC #-}
STREAMING(yieldMany, yieldManyC, yieldManyS, x)

-- | Generate a producer from a seed value.
--
-- Subject to fusion
--
-- @since 1.3.0
unfold :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> ConduitT i a m ()
INLINE_RULE(unfold, f x, CL.unfold f x)

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Subject to fusion
--
-- @since 1.3.0
enumFromTo :: (Monad m, Enum a, Ord a) => a -> a -> ConduitT i a m ()
INLINE_RULE(enumFromTo, f t, CL.enumFromTo f t)

-- | Produces an infinite stream of repeated applications of f to x.
--
-- Subject to fusion
--
-- @since 1.3.0
iterate :: Monad m => (a -> a) -> a -> ConduitT i a m ()
INLINE_RULE(iterate, f t, CL.iterate f t)

-- | Produce an infinite stream consisting entirely of the given value.
--
-- Subject to fusion
--
-- @since 1.3.0
repeat :: Monad m => a -> ConduitT i a m ()
INLINE_RULE(repeat, x, iterate id x)

-- | Produce a finite stream consisting of n copies of the given value.
--
-- Subject to fusion
--
-- @since 1.3.0
replicate :: Monad m
          => Int
          -> a
          -> ConduitT i a m ()
INLINE_RULE(replicate, n x, CL.replicate n x)

-- | Generate a producer by yielding each of the strict chunks in a @LazySequence@.
--
-- For more information, see 'toChunks'.
--
-- Subject to fusion
--
-- @since 1.3.0
sourceLazy :: (Monad m, LazySequence lazy strict)
           => lazy
           -> ConduitT i strict m ()
INLINE_RULE(sourceLazy, x, yieldMany (toChunks x))

-- | Repeatedly run the given action and yield all values it produces.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
repeatM, repeatMC :: Monad m
                  => m a
                  -> ConduitT i a m ()
repeatMC m = forever $ lift m >>= yield
{-# INLINE repeatMC #-}
STREAMING(repeatM, repeatMC, repeatMS, m)

-- | Repeatedly run the given action and yield all values it produces, until
-- the provided predicate returns @False@.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
repeatWhileM, repeatWhileMC :: Monad m
                            => m a
                            -> (a -> Bool)
                            -> ConduitT i a m ()
repeatWhileMC m f =
    loop
  where
    loop = do
        x <- lift m
        when (f x) $ yield x >> loop
STREAMING(repeatWhileM, repeatWhileMC, repeatWhileMS, m f)

-- | Perform the given action n times, yielding each result.
--
-- Subject to fusion
--
-- @since 1.3.0
replicateM :: Monad m
           => Int
           -> m a
           -> ConduitT i a m ()
INLINE_RULE(replicateM, n m, CL.replicateM n m)

-- | Stream the contents of a file as binary data.
--
-- @since 1.3.0
sourceFile :: MonadResource m
           => FilePath
           -> ConduitT i S.ByteString m ()
sourceFile fp =
    bracketP
        (FR.openFile fp)
         FR.closeFile
         loop
  where
    loop h = do
        bs <- liftIO $ FR.readChunk h
        unless (S.null bs) $ do
            yield bs
            loop h

-- | Stream the contents of a 'IO.Handle' as binary data. Note that this
-- function will /not/ automatically close the @Handle@ when processing
-- completes, since it did not acquire the @Handle@ in the first place.
--
-- @since 1.3.0
sourceHandle :: MonadIO m
             => IO.Handle
             -> ConduitT i S.ByteString m ()
sourceHandle h =
    loop
  where
    loop = do
        bs <- liftIO (S.hGetSome h defaultChunkSize)
        if S.null bs
            then return ()
            else yield bs >> loop

-- | Same as @sourceHandle@, but instead of allocating a new buffer for each
-- incoming chunk of data, reuses the same buffer. Therefore, the @ByteString@s
-- yielded by this function are not referentially transparent between two
-- different @yield@s.
--
-- This function will be slightly more efficient than @sourceHandle@ by
-- avoiding allocations and reducing garbage collections, but should only be
-- used if you can guarantee that you do not reuse a @ByteString@ (or any slice
-- thereof) between two calls to @await@.
--
-- @since 1.3.0
sourceHandleUnsafe :: MonadIO m => IO.Handle -> ConduitT i ByteString m ()
sourceHandleUnsafe handle = do
    fptr <- liftIO $ mallocPlainForeignPtrBytes defaultChunkSize
    let ptr = unsafeForeignPtrToPtr fptr
        loop = do
            count <- liftIO $ IO.hGetBuf handle ptr defaultChunkSize
            when (count > 0) $ do
                yield (PS fptr 0 count)
                loop

    loop

    liftIO $ touchForeignPtr fptr

-- | An alternative to 'sourceHandle'.
-- Instead of taking a pre-opened 'IO.Handle', it takes an action that opens
-- a 'IO.Handle' (in read mode), so that it can open it only when needed
-- and close it as soon as possible.
--
-- @since 1.3.0
sourceIOHandle :: MonadResource m
               => IO IO.Handle
               -> ConduitT i S.ByteString m ()
sourceIOHandle alloc = bracketP alloc IO.hClose sourceHandle

-- | Same as 'sourceFile'. The alternate name is a holdover from an older
-- version, when 'sourceFile' was more polymorphic than it is today.
--
-- @since 1.3.0
sourceFileBS :: MonadResource m => FilePath -> ConduitT i ByteString m ()
sourceFileBS = sourceFile
{-# INLINE sourceFileBS #-}

-- | @sourceHandle@ applied to @stdin@.
--
-- Subject to fusion
--
-- @since 1.3.0
stdin :: MonadIO m => ConduitT i ByteString m ()
INLINE_RULE0(stdin, sourceHandle IO.stdin)

-- | Stream all incoming data to the given file.
--
-- @since 1.3.0
sinkFile :: MonadResource m
         => FilePath
         -> ConduitT S.ByteString o m ()
sinkFile fp = sinkIOHandle (IO.openBinaryFile fp IO.WriteMode)

-- | Cautious version of 'sinkFile'. The idea here is to stream the
-- values to a temporary file in the same directory of the destination
-- file, and only on successfully writing the entire file, moves it
-- atomically to the destination path.
--
-- In the event of an exception occurring, the temporary file will be
-- deleted and no move will be made. If the application shuts down
-- without running exception handling (such as machine failure or a
-- SIGKILL), the temporary file will remain and the destination file
-- will be untouched.
--
-- @since 1.3.0
sinkFileCautious
  :: MonadResource m
  => FilePath
  -> ConduitM S.ByteString o m ()
sinkFileCautious fp =
    bracketP (cautiousAcquire fp) cautiousCleanup inner
  where
    inner (tmpFP, h) = do
        sinkHandle h
        liftIO $ do
            IO.hClose h
            renameFile tmpFP fp

-- | Like 'sinkFileCautious', but uses the @with@ pattern instead of
-- @MonadResource@.
--
-- @since 1.3.0
withSinkFileCautious
  :: (MonadUnliftIO m, MonadIO n)
  => FilePath
  -> (ConduitM S.ByteString o n () -> m a)
  -> m a
withSinkFileCautious fp inner =
  withRunInIO $ \run -> bracket
    (cautiousAcquire fp)
    cautiousCleanup
    (\(tmpFP, h) -> do
        a <- run $ inner $ sinkHandle h
        IO.hClose h
        renameFile tmpFP fp
        return a)

-- | Helper function for Cautious functions above, do not export!
cautiousAcquire :: FilePath -> IO (FilePath, IO.Handle)
cautiousAcquire fp = IO.openBinaryTempFile (takeDirectory fp) (takeFileName fp <.> "tmp")

-- | Helper function for Cautious functions above, do not export!
cautiousCleanup :: (FilePath, IO.Handle) -> IO ()
cautiousCleanup (tmpFP, h) = do
  IO.hClose h
  removeFile tmpFP `Control.Exception.catch` \e ->
    if isDoesNotExistError e
      then return ()
      else throwIO e

-- | Stream data into a temporary file in the given directory with the
-- given filename pattern, and return the temporary filename. The
-- temporary file will be automatically deleted when exiting the
-- active 'ResourceT' block, if it still exists.
--
-- @since 1.3.0
sinkTempFile :: MonadResource m
             => FilePath -- ^ temp directory
             -> String -- ^ filename pattern
             -> ConduitM ByteString o m FilePath
sinkTempFile tmpdir pattern = do
    (_releaseKey, (fp, h)) <- allocate
        (IO.openBinaryTempFile tmpdir pattern)
        (\(fp, h) -> IO.hClose h `finally` (removeFile fp `Control.Exception.catch` \e ->
            if isDoesNotExistError e
                then return ()
                else throwIO e))
    sinkHandle h
    liftIO $ IO.hClose h
    return fp

-- | Same as 'sinkTempFile', but will use the default temp file
-- directory for the system as the first argument.
--
-- @since 1.3.0
sinkSystemTempFile
    :: MonadResource m
    => String -- ^ filename pattern
    -> ConduitM ByteString o m FilePath
sinkSystemTempFile pattern = do
    dir <- liftIO getTemporaryDirectory
    sinkTempFile dir pattern

-- | Stream all incoming data to the given 'IO.Handle'. Note that this function
-- does /not/ flush and will /not/ close the @Handle@ when processing completes.
--
-- @since 1.3.0
sinkHandle :: MonadIO m
           => IO.Handle
           -> ConduitT S.ByteString o m ()
sinkHandle h = awaitForever (liftIO . S.hPut h)

-- | Stream incoming builders, executing them directly on the buffer of the
-- given 'IO.Handle'. Note that this function does /not/ automatically close the
-- @Handle@ when processing completes.
-- Pass 'Data.ByteString.Builder.Extra.flush' to flush the buffer.
--
-- @since 1.3.0
sinkHandleBuilder :: MonadIO m => IO.Handle -> ConduitM Builder o m ()
sinkHandleBuilder h = awaitForever (liftIO . hPutBuilder h)

-- | Stream incoming @Flush@es, executing them on @IO.Handle@
-- Note that this function does /not/ automatically close the @Handle@ when
-- processing completes
--
-- @since 1.3.0
sinkHandleFlush :: MonadIO m
                => IO.Handle
                -> ConduitM (Flush S.ByteString) o m ()
sinkHandleFlush h =
  awaitForever $ \mbs -> liftIO $
  case mbs of
    Chunk bs -> S.hPut h bs
    Flush -> IO.hFlush h

-- | An alternative to 'sinkHandle'.
-- Instead of taking a pre-opened 'IO.Handle', it takes an action that opens
-- a 'IO.Handle' (in write mode), so that it can open it only when needed
-- and close it as soon as possible.
--
-- @since 1.3.0
sinkIOHandle :: MonadResource m
             => IO IO.Handle
             -> ConduitT S.ByteString o m ()
sinkIOHandle alloc = bracketP alloc IO.hClose sinkHandle

-- | Like 'IO.withBinaryFile', but provides a source to read bytes from.
--
-- @since 1.3.0
withSourceFile
  :: (MonadUnliftIO m, MonadIO n)
  => FilePath
  -> (ConduitM i ByteString n () -> m a)
  -> m a
withSourceFile fp inner =
  withRunInIO $ \run ->
  IO.withBinaryFile fp IO.ReadMode $
  run . inner . sourceHandle

-- | Like 'IO.withBinaryFile', but provides a sink to write bytes to.
--
-- @since 1.3.0
withSinkFile
  :: (MonadUnliftIO m, MonadIO n)
  => FilePath
  -> (ConduitM ByteString o n () -> m a)
  -> m a
withSinkFile fp inner =
  withRunInIO $ \run ->
  IO.withBinaryFile fp IO.WriteMode $
  run . inner . sinkHandle

-- | Same as 'withSinkFile', but lets you use a 'BB.Builder'.
--
-- @since 1.3.0
withSinkFileBuilder
  :: (MonadUnliftIO m, MonadIO n)
  => FilePath
  -> (ConduitM Builder o n () -> m a)
  -> m a
withSinkFileBuilder fp inner =
  withRunInIO $ \run ->
  IO.withBinaryFile fp IO.WriteMode $ \h ->
  run $ inner $ CL.mapM_ (liftIO . hPutBuilder h)

-- | Stream the contents of the given directory, without traversing deeply.
--
-- This function will return /all/ of the contents of the directory, whether
-- they be files, directories, etc.
--
-- Note that the generated filepaths will be the complete path, not just the
-- filename. In other words, if you have a directory @foo@ containing files
-- @bar@ and @baz@, and you use @sourceDirectory@ on @foo@, the results will be
-- @foo/bar@ and @foo/baz@.
--
-- @since 1.3.0
sourceDirectory :: MonadResource m => FilePath -> ConduitT i FilePath m ()
sourceDirectory dir =
    bracketP (F.openDirStream dir) F.closeDirStream go
  where
    go ds =
        loop
      where
        loop = do
            mfp <- liftIO $ F.readDirStream ds
            case mfp of
                Nothing -> return ()
                Just fp -> do
                    yield $ dir </> fp
                    loop

-- | Deeply stream the contents of the given directory.
--
-- This works the same as @sourceDirectory@, but will not return directories at
-- all. This function also takes an extra parameter to indicate whether
-- symlinks will be followed.
--
-- @since 1.3.0
sourceDirectoryDeep :: MonadResource m
                    => Bool -- ^ Follow directory symlinks
                    -> FilePath -- ^ Root directory
                    -> ConduitT i FilePath m ()
sourceDirectoryDeep followSymlinks =
    start
  where
    start :: MonadResource m => FilePath -> ConduitT i FilePath m ()
    start dir = sourceDirectory dir .| awaitForever go

    go :: MonadResource m => FilePath -> ConduitT i FilePath m ()
    go fp = do
        ft <- liftIO $ F.getFileType fp
        case ft of
            F.FTFile -> yield fp
            F.FTFileSym -> yield fp
            F.FTDirectory -> start fp
            F.FTDirectorySym
                | followSymlinks -> start fp
                | otherwise -> return ()
            F.FTOther -> return ()

-- | Ignore a certain number of values in the stream.
--
-- Note: since this function doesn't produce anything, you probably want to
-- use it with ('>>') instead of directly plugging it into a pipeline:
--
-- >>> runConduit $ yieldMany [1..5] .| drop 2 .| sinkList
-- []
-- >>> runConduit $ yieldMany [1..5] .| (drop 2 >> sinkList)
-- [3,4,5]
--
-- @since 1.3.0
drop :: Monad m
     => Int
     -> ConduitT a o m ()
INLINE_RULE(drop, n, CL.drop n)

-- | Drop a certain number of elements from a chunked stream.
--
-- Note: you likely want to use it with monadic composition. See the docs
-- for 'drop'.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
dropE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> ConduitT seq o m ()
dropE =
    loop
  where
    loop i = if i <= 0
        then return ()
        else await >>= maybe (return ()) (go i)

    go i sq = do
        unless (onull y) $ leftover y
        loop i'
      where
        (x, y) = Seq.splitAt i sq
        i' = i - fromIntegral (olength x)
{-# INLINEABLE dropE #-}

-- | Drop all values which match the given predicate.
--
-- Note: you likely want to use it with monadic composition. See the docs
-- for 'drop'.
--
-- @since 1.3.0
dropWhile :: Monad m
          => (a -> Bool)
          -> ConduitT a o m ()
dropWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x = if f x then loop else leftover x
{-# INLINE dropWhile #-}

-- | Drop all elements in the chunked stream which match the given predicate.
--
-- Note: you likely want to use it with monadic composition. See the docs
-- for 'drop'.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
dropWhileE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> ConduitT seq o m ()
dropWhileE f =
    loop
  where
    loop = await >>= maybe (return ()) go

    go sq =
        if onull x then loop else leftover x
      where
        x = Seq.dropWhile f sq
{-# INLINE dropWhileE #-}

-- | Monoidally combine all values in the stream.
--
-- Subject to fusion
--
-- @since 1.3.0
fold :: (Monad m, Monoid a)
     => ConduitT a o m a
INLINE_RULE0(fold, CL.foldMap id)

-- | Monoidally combine all elements in the chunked stream.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
foldE :: (Monad m, MonoFoldable mono, Monoid (Element mono))
      => ConduitT mono o m (Element mono)
INLINE_RULE0(foldE, CL.fold (\accum mono -> accum `mappend` ofoldMap id mono) mempty)

-- | A strict left fold.
--
-- Subject to fusion
--
-- @since 1.3.0
foldl :: Monad m => (a -> b -> a) -> a -> ConduitT b o m a
INLINE_RULE(foldl, f x, CL.fold f x)

-- | A strict left fold on a chunked stream.
--
-- Subject to fusion
--
-- @since 1.3.0
foldlE :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> a)
       -> a
       -> ConduitT mono o m a
INLINE_RULE(foldlE, f x, CL.fold (ofoldlPrime f) x)

-- Work around CPP not supporting identifiers with primes...
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
ofoldlPrime :: MonoFoldable mono => (a -> Element mono -> a) -> a -> mono -> a
ofoldlPrime = ofoldl'

-- | Apply the provided mapping function and monoidal combine all values.
--
-- Subject to fusion
--
-- @since 1.3.0
foldMap :: (Monad m, Monoid b)
        => (a -> b)
        -> ConduitT a o m b
INLINE_RULE(foldMap, f, CL.foldMap f)

-- | Apply the provided mapping function and monoidal combine all elements of the chunked stream.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
foldMapE :: (Monad m, MonoFoldable mono, Monoid w)
         => (Element mono -> w)
         -> ConduitT mono o m w
INLINE_RULE(foldMapE, f, CL.foldMap (ofoldMap f))

-- | A strict left fold with no starting value.  Returns 'Nothing'
-- when the stream is empty.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
foldl1, foldl1C :: Monad m => (a -> a -> a) -> ConduitT a o m (Maybe a)
foldl1C f =
    await >>= maybe (return Nothing) loop
  where
    loop !prev = await >>= maybe (return $ Just prev) (loop . f prev)
STREAMING(foldl1, foldl1C, foldl1S, f)

-- | A strict left fold on a chunked stream, with no starting value.
-- Returns 'Nothing' when the stream is empty.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
foldl1E :: (Monad m, MonoFoldable mono, a ~ Element mono)
        => (a -> a -> a)
        -> ConduitT mono o m (Maybe a)
INLINE_RULE(foldl1E, f, foldl (foldMaybeNull f) Nothing)

-- Helper for foldl1E
foldMaybeNull :: (MonoFoldable mono, e ~ Element mono)
              => (e -> e -> e)
              -> Maybe e
              -> mono
              -> Maybe e
foldMaybeNull f macc mono =
    case (macc, NonNull.fromNullable mono) of
        (Just acc, Just nn) -> Just $ ofoldl' f acc nn
        (Nothing, Just nn) -> Just $ NonNull.ofoldl1' f nn
        _ -> macc
{-# INLINE foldMaybeNull #-}

-- | Check that all values in the stream return True.
--
-- Subject to shortcut logic: at the first False, consumption of the stream
-- will stop.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
all, allC :: Monad m
          => (a -> Bool)
          -> ConduitT a o m Bool
allC f = fmap isNothing $ find (Prelude.not . f)
{-# INLINE allC #-}
STREAMING(all, allC, allS, f)

-- | Check that all elements in the chunked stream return True.
--
-- Subject to shortcut logic: at the first False, consumption of the stream
-- will stop.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
allE :: (Monad m, MonoFoldable mono)
     => (Element mono -> Bool)
     -> ConduitT mono o m Bool
INLINE_RULE(allE, f, all (oall f))

-- | Check that at least one value in the stream returns True.
--
-- Subject to shortcut logic: at the first True, consumption of the stream
-- will stop.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
any, anyC :: Monad m
          => (a -> Bool)
          -> ConduitT a o m Bool
anyC = fmap isJust . find
{-# INLINE anyC #-}
STREAMING(any, anyC, anyS, f)

-- | Check that at least one element in the chunked stream returns True.
--
-- Subject to shortcut logic: at the first True, consumption of the stream
-- will stop.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
anyE :: (Monad m, MonoFoldable mono)
     => (Element mono -> Bool)
     -> ConduitT mono o m Bool
INLINE_RULE(anyE, f, any (oany f))

-- | Are all values in the stream True?
--
-- Consumption stops once the first False is encountered.
--
-- Subject to fusion
--
-- @since 1.3.0
and :: Monad m => ConduitT Bool o m Bool
INLINE_RULE0(and, all id)

-- | Are all elements in the chunked stream True?
--
-- Consumption stops once the first False is encountered.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
andE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
     => ConduitT mono o m Bool
INLINE_RULE0(andE, allE id)

-- | Are any values in the stream True?
--
-- Consumption stops once the first True is encountered.
--
-- Subject to fusion
--
-- @since 1.3.0
or :: Monad m => ConduitT Bool o m Bool
INLINE_RULE0(or, any id)

-- | Are any elements in the chunked stream True?
--
-- Consumption stops once the first True is encountered.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
orE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
    => ConduitT mono o m Bool
INLINE_RULE0(orE, anyE id)

-- | 'Alternative'ly combine all values in the stream.
--
-- @since 1.3.0
asum :: (Monad m, Alternative f)
     => ConduitT (f a) o m (f a)
INLINE_RULE0(asum, foldl (<|>) empty)

-- | Are any values in the stream equal to the given value?
--
-- Stops consuming as soon as a match is found.
--
-- Subject to fusion
--
-- @since 1.3.0
elem :: (Monad m, Eq a) => a -> ConduitT a o m Bool
INLINE_RULE(elem, x, any (== x))

-- | Are any elements in the chunked stream equal to the given element?
--
-- Stops consuming as soon as a match is found.
--
-- Subject to fusion
--
--  Check the [naming scheme](#g:1) to understand prefixes/postfixes. @since
--
-- 1.3.0
elemE :: (Monad m, Seq.IsSequence seq, Eq (Element seq))
      => Element seq
      -> ConduitT seq o m Bool
INLINE_RULE(elemE, f, any (oelem f))

-- | Are no values in the stream equal to the given value?
--
-- Stops consuming as soon as a match is found.
--
-- Subject to fusion
--
-- @since 1.3.0
notElem :: (Monad m, Eq a) => a -> ConduitT a o m Bool
INLINE_RULE(notElem, x, all (/= x))

-- | Are no elements in the chunked stream equal to the given element?
--
-- Stops consuming as soon as a match is found.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
notElemE :: (Monad m, Seq.IsSequence seq, Eq (Element seq))
         => Element seq
         -> ConduitT seq o m Bool
INLINE_RULE(notElemE, x, all (onotElem x))

-- | Consume all incoming strict chunks into a lazy sequence.
-- Note that the entirety of the sequence will be resident at memory.
--
-- This can be used to consume a stream of strict ByteStrings into a lazy
-- ByteString, for example.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
sinkLazy, sinkLazyC :: (Monad m, LazySequence lazy strict)
                    => ConduitT strict o m lazy
sinkLazyC = (fromChunks . ($ [])) <$> CL.fold (\front next -> front . (next:)) id
{-# INLINE sinkLazyC #-}
STREAMING0(sinkLazy, sinkLazyC, sinkLazyS)

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory.
--
-- Subject to fusion
--
-- @since 1.3.0
sinkList :: Monad m => ConduitT a o m [a]
INLINE_RULE0(sinkList, CL.consume)

-- | Sink incoming values into a vector, growing the vector as necessary to fit
-- more elements.
--
-- Note that using this function is more memory efficient than @sinkList@ and
-- then converting to a @Vector@, as it avoids intermediate list constructors.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
sinkVector, sinkVectorC :: (V.Vector v a, PrimMonad m)
                        => ConduitT a o m (v a)
sinkVectorC = do
    let initSize = 10
    mv0 <- VM.new initSize
    let go maxSize i mv | i >= maxSize = do
            let newMax = maxSize * 2
            mv' <- VM.grow mv maxSize
            go newMax i mv'
        go maxSize i mv = do
            mx <- await
            case mx of
                Nothing -> V.slice 0 i <$> V.unsafeFreeze mv
                Just x -> do
                    VM.write mv i x
                    go maxSize (i + 1) mv
    go initSize 0 mv0
{-# INLINEABLE sinkVectorC #-}
STREAMING0(sinkVector, sinkVectorC, sinkVectorS)

-- | Sink incoming values into a vector, up until size @maxSize@.  Subsequent
-- values will be left in the stream. If there are less than @maxSize@ values
-- present, returns a @Vector@ of smaller size.
--
-- Note that using this function is more memory efficient than @sinkList@ and
-- then converting to a @Vector@, as it avoids intermediate list constructors.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
sinkVectorN, sinkVectorNC :: (V.Vector v a, PrimMonad m)
                          => Int -- ^ maximum allowed size
                          -> ConduitT a o m (v a)
sinkVectorNC maxSize = do
    mv <- VM.new maxSize
    let go i | i >= maxSize = V.unsafeFreeze mv
        go i = do
            mx <- await
            case mx of
                Nothing -> V.slice 0 i <$> V.unsafeFreeze mv
                Just x -> do
                    VM.write mv i x
                    go (i + 1)
    go 0
{-# INLINEABLE sinkVectorNC #-}
STREAMING(sinkVectorN, sinkVectorNC, sinkVectorNS, maxSize)

-- | Same as @sinkBuilder@, but afterwards convert the builder to its lazy
-- representation.
--
-- Alternatively, this could be considered an alternative to @sinkLazy@, with
-- the following differences:
--
-- * This function will allow multiple input types, not just the strict version
-- of the lazy structure.
--
-- * Some buffer copying may occur in this version.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
sinkLazyBuilder, sinkLazyBuilderC :: Monad m => ConduitT Builder o m BL.ByteString
sinkLazyBuilderC = fmap toLazyByteString fold
{-# INLINE sinkLazyBuilderC #-}
STREAMING0(sinkLazyBuilder, sinkLazyBuilderC, sinkLazyBuilderS)

-- | Consume and discard all remaining values in the stream.
--
-- Subject to fusion
--
-- @since 1.3.0
sinkNull :: Monad m => ConduitT a o m ()
INLINE_RULE0(sinkNull, CL.sinkNull)

-- | Same as @await@, but discards any leading 'onull' values.
--
-- @since 1.3.0
awaitNonNull :: (Monad m, MonoFoldable a) => ConduitT a o m (Maybe (NonNull.NonNull a))
awaitNonNull =
    go
  where
    go = await >>= maybe (return Nothing) go'

    go' = maybe go (return . Just) . NonNull.fromNullable
{-# INLINE awaitNonNull #-}

-- | Take a single value from the stream, if available.
--
-- @since 1.3.0
head :: Monad m => ConduitT a o m (Maybe a)
head = CL.head

-- | Same as 'head', but returns a default value if none are available from the stream.
--
-- @since 1.3.0
headDef :: Monad m => a -> ConduitT a o m a
headDef a = fromMaybe a <$> head

-- | Get the next element in the chunked stream.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
headE :: (Monad m, Seq.IsSequence seq) => ConduitT seq o m (Maybe (Element seq))
headE =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go x =
        case Seq.uncons x of
            Nothing -> loop
            Just (y, z) -> do
                unless (onull z) $ leftover z
                return $ Just y
{-# INLINE headE #-}

-- | View the next value in the stream without consuming it.
--
-- @since 1.3.0
peek :: Monad m => ConduitT a o m (Maybe a)
peek = CL.peek
{-# INLINE peek #-}

-- | View the next element in the chunked stream without consuming it.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
peekE :: (Monad m, MonoFoldable mono) => ConduitT mono o m (Maybe (Element mono))
peekE =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go x =
        case headMay x of
            Nothing -> loop
            Just y -> do
                leftover x
                return $ Just y
{-# INLINE peekE #-}

-- | Retrieve the last value in the stream, if present.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
last, lastC :: Monad m => ConduitT a o m (Maybe a)
lastC =
    await >>= maybe (return Nothing) loop
  where
    loop prev = await >>= maybe (return $ Just prev) loop
STREAMING0(last, lastC, lastS)

-- | Same as 'last', but returns a default value if none are available from the stream.
--
-- @since 1.3.0
lastDef :: Monad m => a -> ConduitT a o m a
lastDef a = fromMaybe a <$> last

-- | Retrieve the last element in the chunked stream, if present.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
lastE, lastEC :: (Monad m, Seq.IsSequence seq) => ConduitT seq o m (Maybe (Element seq))
lastEC =
    awaitNonNull >>= maybe (return Nothing) (loop . NonNull.last)
  where
    loop prev = awaitNonNull >>= maybe (return $ Just prev) (loop . NonNull.last)
STREAMING0(lastE, lastEC, lastES)

-- | Count how many values are in the stream.
--
-- Subject to fusion
--
-- @since 1.3.0
length :: (Monad m, Num len) => ConduitT a o m len
INLINE_RULE0(length, foldl (\x _ -> x + 1) 0)

-- | Count how many elements are in the chunked stream.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
lengthE :: (Monad m, Num len, MonoFoldable mono) => ConduitT mono o m len
INLINE_RULE0(lengthE, foldl (\x y -> x + fromIntegral (olength y)) 0)

-- | Count how many values in the stream pass the given predicate.
--
-- Subject to fusion
--
-- @since 1.3.0
lengthIf :: (Monad m, Num len) => (a -> Bool) -> ConduitT a o m len
INLINE_RULE(lengthIf, f, foldl (\cnt a -> if f a then (cnt + 1) else cnt) 0)

-- | Count how many elements in the chunked stream pass the given predicate.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
lengthIfE :: (Monad m, Num len, MonoFoldable mono)
          => (Element mono -> Bool) -> ConduitT mono o m len
INLINE_RULE(lengthIfE, f, foldlE (\cnt a -> if f a then (cnt + 1) else cnt) 0)

-- | Get the largest value in the stream, if present.
--
-- Subject to fusion
--
-- @since 1.3.0
maximum :: (Monad m, Ord a) => ConduitT a o m (Maybe a)
INLINE_RULE0(maximum, foldl1 max)

-- | Get the largest element in the chunked stream, if present.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
maximumE :: (Monad m, Seq.IsSequence seq, Ord (Element seq)) => ConduitT seq o m (Maybe (Element seq))
INLINE_RULE0(maximumE, foldl1E max)

-- | Get the smallest value in the stream, if present.
--
-- Subject to fusion
--
-- @since 1.3.0
minimum :: (Monad m, Ord a) => ConduitT a o m (Maybe a)
INLINE_RULE0(minimum, foldl1 min)

-- | Get the smallest element in the chunked stream, if present.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
minimumE :: (Monad m, Seq.IsSequence seq, Ord (Element seq)) => ConduitT seq o m (Maybe (Element seq))
INLINE_RULE0(minimumE, foldl1E min)

-- | True if there are no values in the stream.
--
-- This function does not modify the stream.
--
-- @since 1.3.0
null :: Monad m => ConduitT a o m Bool
null = (maybe True (\_ -> False)) `fmap` peek
{-# INLINE null #-}

-- | True if there are no elements in the chunked stream.
--
-- This function may remove empty leading chunks from the stream, but otherwise
-- will not modify it.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
nullE :: (Monad m, MonoFoldable mono)
      => ConduitT mono o m Bool
nullE =
    go
  where
    go = await >>= maybe (return True) go'
    go' x = if onull x then go else leftover x >> return False
{-# INLINE nullE #-}

-- | Get the sum of all values in the stream.
--
-- Subject to fusion
--
-- @since 1.3.0
sum :: (Monad m, Num a) => ConduitT a o m a
INLINE_RULE0(sum, foldl (+) 0)

-- | Get the sum of all elements in the chunked stream.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
sumE :: (Monad m, MonoFoldable mono, Num (Element mono)) => ConduitT mono o m (Element mono)
INLINE_RULE0(sumE, foldlE (+) 0)

-- | Get the product of all values in the stream.
--
-- Subject to fusion
--
-- @since 1.3.0
product :: (Monad m, Num a) => ConduitT a o m a
INLINE_RULE0(product, foldl (*) 1)

-- | Get the product of all elements in the chunked stream.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
productE :: (Monad m, MonoFoldable mono, Num (Element mono)) => ConduitT mono o m (Element mono)
INLINE_RULE0(productE, foldlE (*) 1)

-- | Find the first matching value.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
find, findC :: Monad m => (a -> Bool) -> ConduitT a o m (Maybe a)
findC f =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go x = if f x then return (Just x) else loop
{-# INLINE findC #-}
STREAMING(find, findC, findS, f)

-- | Apply the action to all values in the stream.
--
-- Note: if you want to /pass/ the values instead of /consuming/ them, use
-- 'iterM' instead.
--
-- Subject to fusion
--
-- @since 1.3.0
mapM_ :: Monad m => (a -> m ()) -> ConduitT a o m ()
INLINE_RULE(mapM_, f, CL.mapM_ f)

-- | Apply the action to all elements in the chunked stream. Check the [naming
-- scheme](#g:1) to understand prefixes/postfixes.
--
-- Note: the same caveat as with 'mapM_' applies. If you don't want to
-- consume the values, you can use 'iterM':
--
-- > iterM (omapM_ f)
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
mapM_E :: (Monad m, MonoFoldable mono) => (Element mono -> m ()) -> ConduitT mono o m ()
INLINE_RULE(mapM_E, f, CL.mapM_ (omapM_ f))

-- | A monadic strict left fold.
--
-- Subject to fusion
--
-- @since 1.3.0
foldM :: Monad m => (a -> b -> m a) -> a -> ConduitT b o m a
INLINE_RULE(foldM, f x, CL.foldM f x)

-- | A monadic strict left fold on a chunked stream. Check the [naming
-- scheme](#g:1) to understand prefixes/postfixes.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
foldME :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> m a)
       -> a
       -> ConduitT mono o m a
INLINE_RULE(foldME, f x, foldM (ofoldlM f) x)

-- | Apply the provided monadic mapping function and monoidal combine all values.
--
-- Subject to fusion
--
-- @since 1.3.0
foldMapM :: (Monad m, Monoid w) => (a -> m w) -> ConduitT a o m w
INLINE_RULE(foldMapM, f, CL.foldMapM f)

-- | Apply the provided monadic mapping function and monoidal combine all
-- elements in the chunked stream.
--
-- Subject to fusion
--
--  Check the [naming scheme](#g:1) to understand prefixes/postfixes. @since
--
-- @since 1.3.0
foldMapME :: (Monad m, MonoFoldable mono, Monoid w)
          => (Element mono -> m w)
          -> ConduitT mono o m w
INLINE_RULE(foldMapME, f, CL.foldM (ofoldlM (\accum e -> mappend accum `liftM` f e)) mempty)

-- | 'sinkFile' specialized to 'ByteString' to help with type
-- inference.
--
-- @since 1.3.0
sinkFileBS :: MonadResource m => FilePath -> ConduitT ByteString o m ()
sinkFileBS = sinkFile
{-# INLINE sinkFileBS #-}

-- | Print all incoming values to stdout.
--
-- Subject to fusion
--
-- @since 1.3.0
print :: (Show a, MonadIO m) => ConduitT a o m ()
INLINE_RULE0(print, mapM_ (liftIO . Prelude.print))

-- | @sinkHandle@ applied to @stdout@.
--
-- Subject to fusion
--
-- @since 1.3.0
stdout :: MonadIO m => ConduitT ByteString o m ()
INLINE_RULE0(stdout, sinkHandle IO.stdout)

-- | @sinkHandle@ applied to @stderr@.
--
-- Subject to fusion
--
-- @since 1.3.0
stderr :: MonadIO m => ConduitT ByteString o m ()
INLINE_RULE0(stderr, sinkHandle IO.stderr)

-- | Apply a transformation to all values in a stream.
--
-- Subject to fusion
--
-- @since 1.3.0
map :: Monad m => (a -> b) -> ConduitT a b m ()
INLINE_RULE(map, f, CL.map f)

-- | Apply a transformation to all elements in a chunked stream.
--
-- Subject to fusion.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
mapE :: (Monad m, Functor f) => (a -> b) -> ConduitT (f a) (f b) m ()
INLINE_RULE(mapE, f, CL.map (fmap f))

-- | Apply a monomorphic transformation to all elements in a chunked stream.
--
-- Unlike @mapE@, this will work on types like @ByteString@ and @Text@ which are
-- @MonoFunctor@ but not @Functor@.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
omapE :: (Monad m, MonoFunctor mono) => (Element mono -> Element mono) -> ConduitT mono mono m ()
INLINE_RULE(omapE, f, CL.map (omap f))

-- | Apply the function to each value in the stream, resulting in a foldable
-- value (e.g., a list). Then yield each of the individual values in that
-- foldable value separately.
--
-- Generalizes concatMap, mapMaybe, and mapFoldable.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
concatMap, concatMapC :: (Monad m, MonoFoldable mono)
                      => (a -> mono)
                      -> ConduitT a (Element mono) m ()
concatMapC f = awaitForever (yieldMany . f)
{-# INLINE concatMapC #-}
STREAMING(concatMap, concatMapC, concatMapS, f)

-- | Apply the function to each element in the chunked stream, resulting in a
-- foldable value (e.g., a list). Then yield each of the individual values in
-- that foldable value separately. Check the [naming scheme](#g:1) to understand
-- prefixes/postfixes.
--
-- Generalizes concatMap, mapMaybe, and mapFoldable.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
concatMapE :: (Monad m, MonoFoldable mono, Monoid w)
           => (Element mono -> w)
           -> ConduitT mono w m ()
INLINE_RULE(concatMapE, f, CL.map (ofoldMap f))

-- | Stream up to n number of values downstream.
--
-- Note that, if downstream terminates early, not all values will be consumed.
-- If you want to force /exactly/ the given number of values to be consumed,
-- see 'takeExactly'.
--
-- Subject to fusion
--
-- @since 1.3.0
take :: Monad m => Int -> ConduitT a a m ()
INLINE_RULE(take, n, CL.isolate n)

-- | Stream up to n number of elements downstream in a chunked stream.
--
-- Note that, if downstream terminates early, not all values will be consumed.
-- If you want to force /exactly/ the given number of values to be consumed,
-- see 'takeExactlyE'.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
takeE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> ConduitT seq seq m ()
takeE =
    loop
  where
    loop i = if i <= 0
        then return ()
        else await >>= maybe (return ()) (go i)

    go i sq = do
        unless (onull x) $ yield x
        unless (onull y) $ leftover y
        loop i'
      where
        (x, y) = Seq.splitAt i sq
        i' = i - fromIntegral (olength x)
{-# INLINEABLE takeE #-}

-- | Stream all values downstream that match the given predicate.
--
-- Same caveats regarding downstream termination apply as with 'take'.
--
-- @since 1.3.0
takeWhile :: Monad m
          => (a -> Bool)
          -> ConduitT a a m ()
takeWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x = if f x
        then yield x >> loop
        else leftover x
{-# INLINE takeWhile #-}

-- | Stream all elements downstream that match the given predicate in a chunked stream.
--
-- Same caveats regarding downstream termination apply as with 'takeE'.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
takeWhileE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> ConduitT seq seq m ()
takeWhileE f =
    loop
  where
    loop = await >>= maybe (return ()) go

    go sq = do
        unless (onull x) $ yield x
        if onull y
            then loop
            else leftover y
      where
        (x, y) = Seq.span f sq
{-# INLINE takeWhileE #-}

-- | Consume precisely the given number of values and feed them downstream.
--
-- This function is in contrast to 'take', which will only consume up to the
-- given number of values, and will terminate early if downstream terminates
-- early. This function will discard any additional values in the stream if
-- they are unconsumed.
--
-- Note that this function takes a downstream @ConduitT@ as a parameter, as
-- opposed to working with normal fusion. For more information, see
-- <http://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit>, the section
-- titled \"pipes and conduit: isolate\".
--
-- @since 1.3.0
takeExactly :: Monad m
            => Int
            -> ConduitT a b m r
            -> ConduitT a b m r
takeExactly count inner = take count .| do
    r <- inner
    CL.sinkNull
    return r

-- | Same as 'takeExactly', but for chunked streams.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
takeExactlyE :: (Monad m, Seq.IsSequence a)
             => Seq.Index a
             -> ConduitT a b m r
             -> ConduitT a b m r
takeExactlyE count inner = takeE count .| do
    r <- inner
    CL.sinkNull
    return r
{-# INLINE takeExactlyE #-}

-- | Flatten out a stream by yielding the values contained in an incoming
-- @MonoFoldable@ as individually yielded values.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
concat, concatC :: (Monad m, MonoFoldable mono)
                => ConduitT mono (Element mono) m ()
concatC = awaitForever yieldMany
STREAMING0(concat, concatC, concatS)

-- | Keep only values in the stream passing a given predicate.
--
-- Subject to fusion
--
-- @since 1.3.0
filter :: Monad m => (a -> Bool) -> ConduitT a a m ()
INLINE_RULE(filter, f, CL.filter f)

-- | Keep only elements in the chunked stream passing a given predicate.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
filterE :: (Seq.IsSequence seq, Monad m) => (Element seq -> Bool) -> ConduitT seq seq m ()
INLINE_RULE(filterE, f, CL.map (Seq.filter f))

-- | Map values as long as the result is @Just@.
--
-- @since 1.3.0
mapWhile :: Monad m => (a -> Maybe b) -> ConduitT a b m ()
mapWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x =
        case f x of
            Just y -> yield y >> loop
            Nothing -> leftover x
{-# INLINE mapWhile #-}

-- | Break up a stream of values into vectors of size n. The final vector may
-- be smaller than n if the total number of values is not a strict multiple of
-- n. No empty vectors will be yielded.
--
-- @since 1.3.0
conduitVector :: (V.Vector v a, PrimMonad m)
              => Int -- ^ maximum allowed size
              -> ConduitT a (v a) m ()
conduitVector size =
    loop
  where
    loop = do
        v <- sinkVectorN size
        unless (V.null v) $ do
            yield v
            loop
{-# INLINE conduitVector #-}

-- | Analog of 'Prelude.scanl' for lists.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
scanl, scanlC :: Monad m => (a -> b -> a) -> a -> ConduitT b a m ()
scanlC f =
    loop
  where
    loop seed =
        await >>= maybe (yield seed) go
      where
        go b = do
            let seed' = f seed b
            seed' `seq` yield seed
            loop seed'
STREAMING(scanl, scanlC, scanlS, f x)

-- | 'mapWhile' with a break condition dependent on a strict accumulator.
-- Equivalently, 'CL.mapAccum' as long as the result is @Right@. Instead of
-- producing a leftover, the breaking input determines the resulting
-- accumulator via @Left@.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
mapAccumWhile, mapAccumWhileC :: Monad m => (a -> s -> Either s (s, b)) -> s -> ConduitT a b m s
mapAccumWhileC f =
    loop
  where
    loop !s = await >>= maybe (return s) go
      where
        go a = either (return $!) (\(s', b) -> yield b >> loop s') $ f a s
{-# INLINE mapAccumWhileC #-}
STREAMING(mapAccumWhile, mapAccumWhileC, mapAccumWhileS, f s)

-- | 'concatMap' with an accumulator.
--
-- Subject to fusion
--
-- @since 1.3.0
concatMapAccum :: Monad m => (a -> accum -> (accum, [b])) -> accum -> ConduitT a b m ()
INLINE_RULE0(concatMapAccum, CL.concatMapAccum)

-- | Insert the given value between each two values in the stream.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
intersperse, intersperseC :: Monad m => a -> ConduitT a a m ()
intersperseC x =
    await >>= omapM_ go
  where
    go y = yield y >> concatMap (\z -> [x, z])
STREAMING(intersperse, intersperseC, intersperseS, x)

-- | Sliding window of values
-- 1,2,3,4,5 with window size 2 gives
-- [1,2],[2,3],[3,4],[4,5]
--
-- Best used with structures that support O(1) snoc.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
slidingWindow, slidingWindowC :: (Monad m, Seq.IsSequence seq, Element seq ~ a) => Int -> ConduitT a seq m ()
slidingWindowC sz = go (max 1 sz) mempty
    where goContinue st = await >>=
                          maybe (return ())
                                (\x -> do
                                   let st' = Seq.snoc st x
                                   yield st' >> goContinue (Seq.unsafeTail st')
                                )
          go 0 st = yield st >> goContinue (Seq.unsafeTail st)
          go !n st = CL.head >>= \m ->
                     case m of
                       Nothing -> yield st
                       Just x -> go (n-1) (Seq.snoc st x)
STREAMING(slidingWindow, slidingWindowC, slidingWindowS, sz)


-- | Split input into chunk of size 'chunkSize'
--
-- The last element may be smaller than the 'chunkSize' (see also
-- 'chunksOfExactlyE' which will not yield this last element)
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
chunksOfE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> ConduitT seq seq m ()
chunksOfE chunkSize = chunksOfExactlyE chunkSize >> (await >>= maybe (return ()) yield)

-- | Split input into chunk of size 'chunkSize'
--
-- If the input does not split into chunks exactly, the remainder will be
-- leftover (see also 'chunksOfE')
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
chunksOfExactlyE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> ConduitT seq seq m ()
chunksOfExactlyE chunkSize = await >>= maybe (return ()) start
    where
        start b
            | onull b = chunksOfE chunkSize
            | Seq.lengthIndex b < chunkSize = continue (Seq.lengthIndex b) [b]
            | otherwise = let (first,rest) = Seq.splitAt chunkSize b in
                            yield first >> start rest
        continue !sofar bs = do
            next <- await
            case next of
                Nothing -> leftover (mconcat $ Prelude.reverse bs)
                Just next' ->
                    let !sofar' = Seq.lengthIndex next' + sofar
                        bs' = next':bs
                    in if sofar' < chunkSize
                            then continue sofar' bs'
                            else start (mconcat (Prelude.reverse bs'))

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Subject to fusion
--
-- @since 1.3.0
mapM :: Monad m => (a -> m b) -> ConduitT a b m ()
INLINE_RULE(mapM, f, CL.mapM f)

-- | Apply a monadic transformation to all elements in a chunked stream.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
mapME :: (Monad m, Data.Traversable.Traversable f) => (a -> m b) -> ConduitT (f a) (f b) m ()
INLINE_RULE(mapME, f, CL.mapM (Data.Traversable.mapM f))

-- | Apply a monadic monomorphic transformation to all elements in a chunked stream.
--
-- Unlike @mapME@, this will work on types like @ByteString@ and @Text@ which
-- are @MonoFunctor@ but not @Functor@.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
omapME :: (Monad m, MonoTraversable mono)
       => (Element mono -> m (Element mono))
       -> ConduitT mono mono m ()
INLINE_RULE(omapME, f, CL.mapM (omapM f))

-- | Apply the monadic function to each value in the stream, resulting in a
-- foldable value (e.g., a list). Then yield each of the individual values in
-- that foldable value separately.
--
-- Generalizes concatMapM, mapMaybeM, and mapFoldableM.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
concatMapM, concatMapMC :: (Monad m, MonoFoldable mono)
                        => (a -> m mono)
                        -> ConduitT a (Element mono) m ()
concatMapMC f = awaitForever (lift . f >=> yieldMany)
STREAMING(concatMapM, concatMapMC, concatMapMS, f)

-- | Keep only values in the stream passing a given monadic predicate.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
filterM, filterMC :: Monad m
                  => (a -> m Bool)
                  -> ConduitT a a m ()
filterMC f =
    awaitForever go
  where
    go x = do
        b <- lift $ f x
        when b $ yield x
STREAMING(filterM, filterMC, filterMS, f)

-- | Keep only elements in the chunked stream passing a given monadic predicate.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
filterME :: (Monad m, Seq.IsSequence seq) => (Element seq -> m Bool) -> ConduitT seq seq m ()
INLINE_RULE(filterME, f, CL.mapM (Seq.filterM f))

-- | Apply a monadic action on all values in a stream.
--
-- This @Conduit@ can be used to perform a monadic side-effect for every
-- value, whilst passing the value through the @Conduit@ as-is.
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
--
-- Subject to fusion
--
-- @since 1.3.0
iterM :: Monad m => (a -> m ()) -> ConduitT a a m ()
INLINE_RULE(iterM, f, CL.iterM f)

-- | Analog of 'Prelude.scanl' for lists, monadic.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
-- @since 1.3.0
scanlM, scanlMC :: Monad m => (a -> b -> m a) -> a -> ConduitT b a m ()
scanlMC f =
    loop
  where
    loop seed =
        await >>= maybe (yield seed) go
      where
        go b = do
            seed' <- lift $ f seed b
            seed' `seq` yield seed
            loop seed'
STREAMING(scanlM, scanlMC, scanlMS, f x)

-- | Monadic `mapAccumWhile`.
--
-- Subject to fusion
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
mapAccumWhileM, mapAccumWhileMC :: Monad m => (a -> s -> m (Either s (s, b))) -> s -> ConduitT a b m s
mapAccumWhileMC f =
    loop
  where
    loop !s = await >>= maybe (return s) go
      where
        go a = lift (f a s) >>= either (return $!) (\(s', b) -> yield b >> loop s')
{-# INLINE mapAccumWhileMC #-}
STREAMING(mapAccumWhileM, mapAccumWhileMC, mapAccumWhileMS, f s)

-- | 'concatMapM' with an accumulator.
--
-- Subject to fusion
--
-- @since 1.3.0
concatMapAccumM :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> ConduitT a b m ()
INLINE_RULE(concatMapAccumM, f x, CL.concatMapAccumM f x)

-- | Encode a stream of text as UTF8.
--
-- Subject to fusion
--
-- @since 1.3.0
encodeUtf8 :: (Monad m, DTE.Utf8 text binary) => ConduitT text binary m ()
INLINE_RULE0(encodeUtf8, map DTE.encodeUtf8)

-- | Decode a stream of binary data as UTF8.
--
-- @since 1.3.0
decodeUtf8 :: MonadThrow m => ConduitT ByteString Text m ()
decodeUtf8 =
    loop TE.streamDecodeUtf8
  where
    loop parse =
        await >>= maybe done go
      where
        parse' = unsafePerformIO . try . evaluate . parse
        done =
          case parse' mempty of
            Left e -> throwM (e :: TEE.UnicodeException)
            Right (TE.Some t bs _) -> do
              unless (T.null t) (yield t)
              unless (S.null bs) (yield $ T.replicate (S.length bs) (T.singleton '\xFFFD'))

        go bs = do
          case parse' bs of
            Left e -> do
              leftover bs
              throwM (e :: TEE.UnicodeException)
            Right (TE.Some t _ next) -> do
              unless (T.null t) (yield t)
              loop next

-- | Decode a stream of binary data as UTF8, replacing any invalid bytes with
-- the Unicode replacement character.
--
-- @since 1.3.0
decodeUtf8Lenient :: Monad m => ConduitT ByteString Text m ()
decodeUtf8Lenient =
    loop (TE.streamDecodeUtf8With TEE.lenientDecode)
  where
    loop parse =
        await >>= maybe done go
      where
        done = do
          let TE.Some t bs _ = parse mempty
          unless (T.null t) (yield t)
          unless (S.null bs) (yield $ T.replicate (S.length bs) (T.singleton '\xFFFD'))

        go bs = do
          let TE.Some t _ next = parse bs
          unless (T.null t) (yield t)
          loop next

-- | Stream in the entirety of a single line.
--
-- Like @takeExactly@, this will consume the entirety of the line regardless of
-- the behavior of the inner Conduit.
--
-- @since 1.3.0
line :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
     => ConduitT seq o m r
     -> ConduitT seq o m r
line = takeExactlyUntilE (== '\n')
{-# INLINE line #-}

-- | Same as 'line', but operates on ASCII/binary data.
--
-- @since 1.3.0
lineAscii :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8)
          => ConduitT seq o m r
          -> ConduitT seq o m r
lineAscii = takeExactlyUntilE (== 10)
{-# INLINE lineAscii #-}

-- | Stream in the chunked input until an element matches a predicate.
--
-- Like @takeExactly@, this will consume the entirety of the prefix
-- regardless of the behavior of the inner Conduit.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
takeExactlyUntilE :: (Monad m, Seq.IsSequence seq)
                  => (Element seq -> Bool)
                  -> ConduitT seq o m r
                  -> ConduitT seq o m r
takeExactlyUntilE f inner =
    loop .| do
        x <- inner
        sinkNull
        return x
  where
    loop = await >>= omapM_ go
    go t =
        if onull y
            then yield x >> loop
            else do
                unless (onull x) $ yield x
                let y' = Seq.drop 1 y
                unless (onull y') $ leftover y'
      where
        (x, y) = Seq.break f t
{-# INLINE takeExactlyUntilE #-}

-- | Insert a newline character after each incoming chunk of data.
--
-- Subject to fusion
--
-- @since 1.3.0
unlines :: (Monad m, Seq.IsSequence seq, Element seq ~ Char) => ConduitT seq seq m ()
INLINE_RULE0(unlines, concatMap (:[Seq.singleton '\n']))

-- | Same as 'unlines', but operates on ASCII/binary data.
--
-- Subject to fusion
--
-- @since 1.3.0
unlinesAscii :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8) => ConduitT seq seq m ()
INLINE_RULE0(unlinesAscii, concatMap (:[Seq.singleton 10]))

-- | Split a stream of arbitrarily-chunked data, based on a predicate
-- on elements.  Elements that satisfy the predicate will cause chunks
-- to be split, and aren't included in these output chunks.  Note
-- that, if you have unknown or untrusted input, this function is
-- /unsafe/, since it would allow an attacker to form chunks of
-- massive length and exhaust memory.
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
splitOnUnboundedE, splitOnUnboundedEC :: (Monad m, Seq.IsSequence seq) => (Element seq -> Bool) -> ConduitT seq seq m ()
splitOnUnboundedEC f =
    start
  where
    start = await >>= maybe (return ()) (loop id)

    loop bldr t =
        if onull y
            then do
                mt <- await
                case mt of
                    Nothing -> let finalChunk = mconcat $ bldr [t]
                               in  unless (onull finalChunk) $ yield finalChunk
                    Just t' -> loop (bldr . (t:)) t'
            else yield (mconcat $ bldr [x]) >> loop id (Seq.drop 1 y)
      where
        (x, y) = Seq.break f t
STREAMING(splitOnUnboundedE, splitOnUnboundedEC, splitOnUnboundedES, f)

-- | Convert a stream of arbitrarily-chunked textual data into a stream of data
-- where each chunk represents a single line. Note that, if you have
-- unknown or untrusted input, this function is /unsafe/, since it would allow an
-- attacker to form lines of massive length and exhaust memory.
--
-- Subject to fusion
--
-- @since 1.3.0
linesUnbounded :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
               => ConduitT seq seq m ()
INLINE_RULE0(linesUnbounded, splitOnUnboundedE (== '\n'))

-- | Same as 'linesUnbounded', but for ASCII/binary data.
--
-- Subject to fusion
--
-- @since 1.3.0
linesUnboundedAscii :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8)
                    => ConduitT seq seq m ()
INLINE_RULE0(linesUnboundedAscii, splitOnUnboundedE (== 10))

-- | Incrementally execute builders and pass on the filled chunks as
-- bytestrings.
--
-- @since 1.3.0
builderToByteString :: PrimMonad m => ConduitT Builder S.ByteString m ()
builderToByteString = builderToByteStringWith defaultStrategy
{-# INLINE builderToByteString #-}

-- | Same as 'builderToByteString', but input and output are wrapped in
-- 'Flush'.
--
-- @since 1.3.0
builderToByteStringFlush :: PrimMonad m
                         => ConduitT (Flush Builder) (Flush S.ByteString) m ()
builderToByteStringFlush = builderToByteStringWithFlush defaultStrategy
{-# INLINE builderToByteStringFlush #-}

-- | Incrementally execute builders on the given buffer and pass on the filled
-- chunks as bytestrings. Note that, if the given buffer is too small for the
-- execution of a build step, a larger one will be allocated.
--
-- WARNING: This conduit yields bytestrings that are NOT
-- referentially transparent. Their content will be overwritten as soon
-- as control is returned from the inner sink!
--
-- @since 1.3.0
unsafeBuilderToByteString :: PrimMonad m
                          => ConduitT Builder S.ByteString m ()
unsafeBuilderToByteString =
  builderToByteStringWith (reuseBufferStrategy (allocBuffer defaultChunkSize))
{-# INLINE unsafeBuilderToByteString #-}


-- | A conduit that incrementally executes builders and passes on the
-- filled chunks as bytestrings to an inner sink.
--
-- INV: All bytestrings passed to the inner sink are non-empty.
--
-- @since 1.3.0
builderToByteStringWith :: PrimMonad m
                        => BufferAllocStrategy
                        -> ConduitT Builder S.ByteString m ()
builderToByteStringWith =
    bbhelper (liftM (fmap Chunk) await) yield'
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs
{-# INLINE builderToByteStringWith #-}

-- |
--
-- @since 1.3.0
builderToByteStringWithFlush
    :: PrimMonad m
    => BufferAllocStrategy
    -> ConduitT (Flush Builder) (Flush S.ByteString) m ()
builderToByteStringWithFlush = bbhelper await yield
{-# INLINE builderToByteStringWithFlush #-}

bbhelper
  :: PrimMonad m
  => m (Maybe (Flush Builder))
  -> (Flush S.ByteString -> m ())
  -> BufferAllocStrategy
  -> m ()
bbhelper await' yield' strat = do
    (recv, finish) <- unsafePrimToPrim $ newByteStringBuilderRecv strat
    let loop = await' >>= maybe finish' cont
        finish' = do
            mbs <- unsafePrimToPrim finish
            maybe (return ()) (yield' . Chunk) mbs
        cont fbuilder = do
            let builder =
                    case fbuilder of
                        Flush -> BB.flush
                        Chunk b -> b
            popper <- unsafePrimToPrim $ recv builder
            let cont' = do
                    bs <- unsafePrimToPrim popper
                    unless (S.null bs) $ do
                        yield' (Chunk bs)
                        cont'
            cont'
            case fbuilder of
                Flush -> yield' Flush
                Chunk _ -> return ()
            loop
    loop
{-# INLINE bbhelper #-}

-- | Provides a series of @ByteString@s until empty, at which point it provides
-- an empty @ByteString@.
--
-- @since 1.3.0
--
type BuilderPopper = IO S.ByteString

type BuilderRecv = Builder -> IO BuilderPopper

type BuilderFinish = IO (Maybe S.ByteString)

newByteStringBuilderRecv :: BufferAllocStrategy -> IO (BuilderRecv, BuilderFinish)
newByteStringBuilderRecv (ioBufInit, nextBuf) = do
    refBuf <- newIORef ioBufInit
    return (push refBuf, finish refBuf)
  where
    finish refBuf = do
        ioBuf <- readIORef refBuf
        buf <- ioBuf
        return $ unsafeFreezeNonEmptyBuffer buf

    push refBuf builder = do
        refWri <- newIORef $ Left $ BB.runBuilder builder
        return $ popper refBuf refWri

    popper refBuf refWri = do
        ioBuf <- readIORef refBuf
        ebWri <- readIORef refWri
        case ebWri of
            Left bWri -> do
                !buf@(Buffer _ _ op ope) <- ioBuf
                (bytes, next) <- bWri op (ope `minusPtr` op)
                let op' = op `plusPtr` bytes
                case next of
                    BB.Done -> do
                        writeIORef refBuf $ return $ updateEndOfSlice buf op'
                        return S.empty
                    BB.More minSize bWri' -> do
                        let buf' = updateEndOfSlice buf op'
                            {-# INLINE cont #-}
                            cont mbs = do
                                -- sequencing the computation of the next buffer
                                -- construction here ensures that the reference to the
                                -- foreign pointer `fp` is lost as soon as possible.
                                ioBuf' <- nextBuf minSize buf'
                                writeIORef refBuf ioBuf'
                                writeIORef refWri $ Left bWri'
                                case mbs of
                                    Just bs | not $ S.null bs -> return bs
                                    _ -> popper refBuf refWri
                        cont $ unsafeFreezeNonEmptyBuffer buf'
                    BB.Chunk bs bWri' -> do
                        let buf' = updateEndOfSlice buf op'
                        let yieldBS = do
                                nextBuf 1 buf' >>= writeIORef refBuf
                                writeIORef refWri $ Left bWri'
                                if S.null bs
                                    then popper refBuf refWri
                                    else return bs
                        case unsafeFreezeNonEmptyBuffer buf' of
                            Nothing -> yieldBS
                            Just bs' -> do
                                writeIORef refWri $ Right yieldBS
                                return bs'
            Right action -> action

-- | A buffer @Buffer fpbuf p0 op ope@ describes a buffer with the underlying
-- byte array @fpbuf..ope@, the currently written slice @p0..op@ and the free
-- space @op..ope@.
--
-- @since 1.3.0
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8) -- underlying pinned array
                     {-# UNPACK #-} !(Ptr Word8)        -- beginning of slice
                     {-# UNPACK #-} !(Ptr Word8)        -- next free byte
                     {-# UNPACK #-} !(Ptr Word8)        -- first byte after buffer

-- | Convert the buffer to a bytestring. This operation is unsafe in the sense
-- that created bytestring shares the underlying byte array with the buffer.
-- Hence, depending on the later use of this buffer (e.g., if it gets reset and
-- filled again) referential transparency may be lost.
--
-- @since 1.3.0
--
{-# INLINE unsafeFreezeBuffer #-}
unsafeFreezeBuffer :: Buffer -> S.ByteString
unsafeFreezeBuffer (Buffer fpbuf p0 op _) =
    PS fpbuf (p0 `minusPtr` unsafeForeignPtrToPtr fpbuf) (op `minusPtr` p0)

-- | Convert a buffer to a non-empty bytestring. See 'unsafeFreezeBuffer' for
-- the explanation of why this operation may be unsafe.
--
-- @since 1.3.0
--
{-# INLINE unsafeFreezeNonEmptyBuffer #-}
unsafeFreezeNonEmptyBuffer :: Buffer -> Maybe S.ByteString
unsafeFreezeNonEmptyBuffer buf
  | sliceSize buf <= 0 = Nothing
  | otherwise          = Just $ unsafeFreezeBuffer buf

-- | Update the end of slice pointer.
--
-- @since 1.3.0
--
{-# INLINE updateEndOfSlice #-}
updateEndOfSlice :: Buffer    -- Old buffer
                 -> Ptr Word8 -- New end of slice
                 -> Buffer    -- Updated buffer
updateEndOfSlice (Buffer fpbuf p0 _ ope) op' = Buffer fpbuf p0 op' ope

-- | The size of the written slice in the buffer.
--
-- @since 1.3.0
--
sliceSize :: Buffer -> Int
sliceSize (Buffer _ p0 op _) = op `minusPtr` p0

-- | A buffer allocation strategy @(buf0, nextBuf)@ specifies the initial
-- buffer to use and how to compute a new buffer @nextBuf minSize buf@ with at
-- least size @minSize@ from a filled buffer @buf@. The double nesting of the
-- @IO@ monad helps to ensure that the reference to the filled buffer @buf@ is
-- lost as soon as possible, but the new buffer doesn't have to be allocated
-- too early.
--
-- @since 1.3.0
type BufferAllocStrategy = (IO Buffer, Int -> Buffer -> IO (IO Buffer))

-- | Safe default: allocate new buffers of default chunk size
--
-- @since 1.3.0
defaultStrategy :: BufferAllocStrategy
defaultStrategy = allNewBuffersStrategy defaultChunkSize

-- | The simplest buffer allocation strategy: whenever a buffer is requested,
-- allocate a new one that is big enough for the next build step to execute.
--
-- NOTE that this allocation strategy may spill quite some memory upon direct
-- insertion of a bytestring by the builder. Thats no problem for garbage
-- collection, but it may lead to unreasonably high memory consumption in
-- special circumstances.
--
-- @since 1.3.0
allNewBuffersStrategy :: Int                 -- Minimal buffer size.
                      -> BufferAllocStrategy
allNewBuffersStrategy bufSize =
    ( allocBuffer bufSize
    , \reqSize _ -> return (allocBuffer (max reqSize bufSize)) )

-- | An unsafe, but possibly more efficient buffer allocation strategy:
-- reuse the buffer, if it is big enough for the next build step to execute.
--
-- @since 1.3.0
reuseBufferStrategy :: IO Buffer
                    -> BufferAllocStrategy
reuseBufferStrategy buf0 =
    (buf0, tryReuseBuffer)
  where
    tryReuseBuffer reqSize buf
      | bufferSize buf >= reqSize = return $ return (reuseBuffer buf)
      | otherwise                 = return $ allocBuffer reqSize

-- | The size of the whole byte array underlying the buffer.
--
-- @since 1.3.0
--
bufferSize :: Buffer -> Int
bufferSize (Buffer fpbuf _ _ ope) =
    ope `minusPtr` unsafeForeignPtrToPtr fpbuf

-- | @allocBuffer size@ allocates a new buffer of size @size@.
--
-- @since 1.3.0
--
{-# INLINE allocBuffer #-}
allocBuffer :: Int -> IO Buffer
allocBuffer size = do
    fpbuf <- mallocByteString size
    let !pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf pbuf pbuf (pbuf `plusPtr` size)

-- | Resets the beginning of the next slice and the next free byte such that
-- the whole buffer can be filled again.
--
-- @since 1.3.0
--
{-# INLINE reuseBuffer #-}
reuseBuffer :: Buffer -> Buffer
reuseBuffer (Buffer fpbuf _ _ ope) = Buffer fpbuf p0 p0 ope
  where
    p0 = unsafeForeignPtrToPtr fpbuf

-- | Generally speaking, yielding values from inside a Conduit requires
-- some allocation for constructors. This can introduce an overhead,
-- similar to the overhead needed to represent a list of values instead of
-- a vector. This overhead is even more severe when talking about unboxed
-- values.
--
-- This combinator allows you to overcome this overhead, and efficiently
-- fill up vectors. It takes two parameters. The first is the size of each
-- mutable vector to be allocated. The second is a function. The function
-- takes an argument which will yield the next value into a mutable
-- vector.
--
-- Under the surface, this function uses a number of tricks to get high
-- performance. For more information on both usage and implementation,
-- please see:
-- <https://www.fpcomplete.com/user/snoyberg/library-documentation/vectorbuilder>
--
-- @since 1.3.0
vectorBuilder :: (PrimMonad m, PrimMonad n, V.Vector v e, PrimState m ~ PrimState n)
              => Int -- ^ size
              -> ((e -> n ()) -> ConduitT i Void m r)
              -> ConduitT i (v e) m r
vectorBuilder size inner = do
    ref <- do
        mv <- VM.new size
        newMutVar $! S 0 mv id
    res <- onAwait (yieldS ref) (inner (addE ref))
    vs <- do
        S idx mv front <- readMutVar ref
        end <-
            if idx == 0
                then return []
                else do
                    v <- V.unsafeFreeze mv
                    return [V.unsafeTake idx v]
        return $ front end
    Prelude.mapM_ yield vs
    return res
{-# INLINE vectorBuilder #-}

data S s v e = S
    {-# UNPACK #-} !Int -- index
    !(V.Mutable v s e)
    ([v e] -> [v e])

onAwait :: Monad m
        => ConduitT i o m ()
        -> ConduitT i Void m r
        -> ConduitT i o m r
onAwait (ConduitT callback) (ConduitT sink0) = ConduitT $ \rest -> let
    go (Done r) = rest r
    go (HaveOutput _ o) = absurd o
    go (NeedInput f g) = callback $ \() -> NeedInput (go . f) (go . g)
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover f i) = Leftover (go f) i
    in go (sink0 Done)
{-# INLINE onAwait #-}

yieldS :: PrimMonad m
       => MutVar (PrimState m) (S (PrimState m) v e)
       -> ConduitT i (v e) m ()
yieldS ref = do
    S idx mv front <- readMutVar ref
    Prelude.mapM_ yield (front [])
    writeMutVar ref $! S idx mv id
{-# INLINE yieldS #-}

addE :: (PrimMonad m, V.Vector v e)
     => MutVar (PrimState m) (S (PrimState m) v e)
     -> e
     -> m ()
addE ref e = do
    S idx mv front <- readMutVar ref
    VM.write mv idx e
    let idx' = succ idx
        size = VM.length mv
    if idx' >= size
        then do
            v <- V.unsafeFreeze mv
            let front' = front . (v:)
            mv' <- VM.new size
            writeMutVar ref $! S 0 mv' front'
        else writeMutVar ref $! S idx' mv front
{-# INLINE addE #-}

-- | Consume a source with a strict accumulator, in a way piecewise defined by
-- a controlling stream. The latter will be evaluated until it terminates.
--
-- >>> let f a s = liftM (:s) $ mapC (*a) =$ CL.take a
-- >>> reverse $ runIdentity $ yieldMany [0..3] $$ mapAccumS f [] (yieldMany [1..])
-- [[],[1],[4,6],[12,15,18]] :: [[Int]]
mapAccumS
  :: Monad m
  => (a -> s -> ConduitT b Void m s)
  -> s
  -> ConduitT () b m ()
  -> ConduitT a Void m s
mapAccumS f s xs = do
    (_, u) <- loop (sealConduitT xs, s)
    return u
    where loop r@(ys, !t) = await >>= maybe (return r) go
              where go a  = lift (ys $$++ f a t) >>= loop
{-# INLINE mapAccumS #-}

-- | Run a consuming conduit repeatedly, only stopping when there is no more
-- data available from upstream.
--
-- @since 1.3.0
peekForever :: Monad m => ConduitT i o m () -> ConduitT i o m ()
peekForever inner =
    loop
  where
    loop = do
        mx <- peek
        case mx of
            Nothing -> return ()
            Just _ -> inner >> loop

-- | Run a consuming conduit repeatedly, only stopping when there is no more
-- data available from upstream.
--
-- In contrast to 'peekForever', this function will ignore empty
-- chunks of data. So for example, if a stream of data contains an
-- empty @ByteString@, it is still treated as empty, and the consuming
-- function is not called.
--
-- @since 1.3.0
--
-- Check the [naming scheme](#g:1) to better understand prefixes/postfixes.
--
peekForeverE :: (Monad m, MonoFoldable i)
             => ConduitT i o m ()
             -> ConduitT i o m ()
peekForeverE inner =
    loop
  where
    loop = do
        mx <- peekE
        case mx of
            Nothing -> return ()
            Just _ -> inner >> loop
