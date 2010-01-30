-- | A collection of utility functions for dealing with 'Enumerator's.
module Network.Wai.Enumerator
    ( -- * Lazy byte strings
      toLBS
    , fromLBS
    , fromLBS'
      -- * Source
    , toSource
    ) where

import Network.Wai (Enumerator)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafeInterleaveIO)

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

fromLBS :: L.ByteString -> Enumerator a
fromLBS lbs iter a0 = helper a0 $ L.toChunks lbs where
    helper a [] = return a
    helper a (x:xs) = do
        ea <- iter a x
        case ea of
            Left a' -> return a'
            Right a' -> helper a' xs

fromLBS' :: IO L.ByteString -> Enumerator a
fromLBS' lbs' iter a0 = lbs' >>= \lbs -> fromLBS lbs iter a0

-- | A source is a more standard way of accessing data from an 'Enumerator'.
-- Each time you call it, it returns the next chunk of data if available, or
-- 'Nothing' if the data has been completely consumed.
toSource :: Enumerator (Maybe B.ByteString) -> IO (Maybe B.ByteString)
toSource e = e (const $ return . Left . Just) Nothing
