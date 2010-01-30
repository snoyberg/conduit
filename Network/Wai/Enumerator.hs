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

import Network.Wai (Enumerator)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO (withBinaryFile, IOMode (ReadMode), Handle, hIsEOF)

-- | Performs a specified conversion on each 'B.ByteString' output by an
-- enumerator.
mapE :: (B.ByteString -> B.ByteString) -> Enumerator a -> Enumerator a
mapE f e iter = e iter' where
    iter' a = iter a . f

-- | This uses 'unsafeInterleaveIO' to lazily read from an enumerator. All
-- normal lazy I/O warnings apply.
toLBS :: Enumerator (Maybe B.ByteString) -> IO L.ByteString
toLBS e = L.fromChunks `fmap` helper where
    helper = unsafeInterleaveIO $ do
                x <- toSource e
                case x of
                    Nothing -> return []
                    Just x' -> do
                        xs <- helper
                        return $ x' : xs

fromLBS :: L.ByteString -> (forall a. Enumerator a)
fromLBS lbs iter a0 = helper a0 $ L.toChunks lbs where
    helper a [] = return $ Right a
    helper a (x:xs) = do
        ea <- iter a x
        case ea of
            Left a' -> return $ Left a'
            Right a' -> helper a' xs

fromLBS' :: IO L.ByteString -> (forall a. Enumerator a)
fromLBS' lbs' iter a0 = lbs' >>= \lbs -> fromLBS lbs iter a0

-- | A source is a more standard way of accessing data from an 'Enumerator'.
-- Each time you call it, it returns the next chunk of data if available, or
-- 'Nothing' if the data has been completely consumed.
toSource :: Enumerator (Maybe B.ByteString) -> IO (Maybe B.ByteString)
toSource e = fmap (either id id) $ e (const $ return . Left . Just) Nothing

chunkSize :: Int
chunkSize = 1024 -- FIXME

fromHandle :: Handle -> Enumerator a
fromHandle h iter a = do
    eof <- hIsEOF h
    if eof
        then return $ Right a
        else do
            bs <- B.hGet h chunkSize
            ea' <- iter a bs
            case ea' of
                Left a' -> return $ Left a'
                Right a' -> fromHandle h iter a'

fromFile :: FilePath -> Enumerator a
fromFile fp iter a0 = withBinaryFile fp ReadMode $ \h -> fromHandle h iter a0

fromEitherFile :: Either FilePath (Enumerator a) -> Enumerator a
fromEitherFile (Left fp) = fromFile fp
fromEitherFile (Right e) = e
