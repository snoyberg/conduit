{-# LANGUAGE Rank2Types #-}
-- | A collection of utility functions for dealing with 'Enumerator's.
module Network.Wai.Enumerator
    ( -- * Utilities
      mapE
      -- * Conversions
    , -- ** Lazy byte strings
      toLBS
    , fromLBS
    , fromLBS'
      -- ** Source
    , toSource
      -- ** Handle
    , fromHandle
      -- ** FilePath
    , fromFile
    , fromEitherFile
    ) where

import Network.Wai (Enumerator (..), Source (..))
import qualified Network.Wai.Source as Source
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.IO (withBinaryFile, IOMode (ReadMode), Handle, hIsEOF)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad ((<=<))

-- | Performs a specified conversion on each 'B.ByteString' output by an
-- enumerator.
mapE :: (B.ByteString -> B.ByteString) -> Enumerator -> Enumerator
mapE f (Enumerator e) = Enumerator $ \iter -> e (iter' iter) where
    iter' iter a = iter a . f

-- | This uses 'unsafeInterleaveIO' to lazily read from an enumerator. All
-- normal lazy I/O warnings apply. In addition, since it is based on
-- 'toSource', please observe all precautions for that function.
toLBS :: Enumerator -> IO L.ByteString
toLBS = Source.toLBS <=< toSource

-- | This function safely converts a lazy bytestring into an enumerator.
fromLBS :: L.ByteString -> Enumerator
fromLBS lbs = Enumerator $ \iter a0 -> helper iter a0 $ L.toChunks lbs where
    helper _ a [] = return $ Right a
    helper iter a (x:xs) = do
        ea <- iter a x
        case ea of
            Left a' -> return $ Left a'
            Right a' -> helper iter a' xs

-- | Same as 'fromLBS', but the lazy bytestring is in the IO monad. This allows
-- you to lazily read a file into memory, perform some mapping on the data and
-- convert it into an enumerator.
fromLBS' :: IO L.ByteString -> Enumerator
fromLBS' lbs' = Enumerator $ \iter a0 -> lbs' >>= \lbs ->
    runEnumerator (fromLBS lbs) iter a0

-- | This function uses another thread to convert an 'Enumerator' to a
-- 'Source'. In essence, this allows you to write code which \"pulls\" instead
-- of code which is pushed to. While this can be a useful technique, some
-- caveats apply:
--
-- * It will be more resource heavy than using the 'Enumerator' directly.
--
-- * You *must* consume all input. If you do not, then the other thread will be
-- deadlocked.
toSource :: Enumerator -> IO Source
toSource (Enumerator e) = do
    buffer <- newEmptyMVar
    _ <- forkIO $ e (helper buffer) () >> putMVar buffer Nothing
    return $ Source () $ source buffer
      where
        helper :: MVar (Maybe B.ByteString)
               -> ()
               -> B.ByteString
               -> IO (Either () ())
        helper buffer _ bs = do
            putMVar buffer $ Just bs
            return $ Right ()
        source :: MVar (Maybe B.ByteString) -> ()
               -> IO (Maybe (B.ByteString, ()))
        source mmbs () = do
            mbs <- takeMVar mmbs
            case mbs of
                Nothing -> do
                    -- By putting Nothing back in, the source can be called
                    -- again without causing a deadlock.
                    putMVar mmbs Nothing
                    return Nothing
                Just bs -> return $ Just (bs, ())

-- | Read a chunk of data from the given 'Handle' at a time. We use
-- 'defaultChunkSize' from the bytestring package to determine the largest
-- chunk to take.
fromHandle :: Handle -> Enumerator
fromHandle h = Enumerator $ \iter a -> do
    eof <- hIsEOF h
    if eof
        then return $ Right a
        else do
            bs <- B.hGet h defaultChunkSize
            ea' <- iter a bs
            case ea' of
                Left a' -> return $ Left a'
                Right a' -> runEnumerator (fromHandle h) iter a'

-- | A little wrapper around 'fromHandle' which first opens a file for reading.
fromFile :: FilePath -> Enumerator
fromFile fp = Enumerator $ \iter a0 -> withBinaryFile fp ReadMode $ \h ->
    runEnumerator (fromHandle h) iter a0

-- | Since the response body is defined as an 'Either' 'FilePath' 'Enumerator',
-- this function simply reduces the whole operator to an enumerator. This can
-- be convenient for server implementations not optimizing file sending.
fromEitherFile :: Either FilePath Enumerator -> Enumerator
fromEitherFile = either fromFile id
