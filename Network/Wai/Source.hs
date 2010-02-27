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
toEnumerator source0 = Enumerator $ helper source0 where
    helper source iter a = do
        next <- runSource source
        case next of
            Nothing -> return $ Right a
            Just (bs, source') -> do
                res <- iter a bs
                case res of
                    Left a' -> return $ Left a'
                    Right a' -> helper source' iter a'

-- | Uses lazy I\/O (via 'unsafeInterleaveIO') to provide a lazy interface to
-- the given 'Source'. Normal lazy I\/O warnings apply.
toLBS :: Source -> IO L.ByteString
toLBS source0 = L.fromChunks `fmap` helper source0 where
    helper source = unsafeInterleaveIO $ do
        next <- runSource source
        case next of
            Nothing -> return []
            Just (bs, source') -> do
                rest <- helper source'
                return $ bs : rest
