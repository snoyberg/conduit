module Network.Wai.Source
    (
    -- * Conversions
      toEnumerator
    , toLBS
    ) where

import Network.Wai
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe (unsafeInterleaveIO)

-- | This function safely converts a 'Source' (where you pull data) to an
-- 'Enumerator' (which pushes the data to you). There should be no significant
-- performance impact from its use, and it uses no unsafe functions.
toEnumerator :: Source -> Enumerator
toEnumerator (Source b0 source) = Enumerator $ helper b0 where
    helper b iter a = do
        next <- source b
        case next of
            Nothing -> return $ Right a
            Just (bs, b') -> do
                res <- iter a bs
                case res of
                    Left a' -> return $ Left a'
                    Right a' -> helper b' iter a'

-- | Uses lazy I\/O (via 'unsafeInterleaveIO') to provide a lazy interface to
-- the given 'Source'. Normal lazy I\/O warnings apply.
toLBS :: Source -> IO L.ByteString
toLBS (Source b0 source) = L.fromChunks `fmap` helper b0 where
    helper b = unsafeInterleaveIO $ do
        next <- source b
        case next of
            Nothing -> return []
            Just (bs, b') -> do
                rest <- helper b'
                return $ bs : rest
