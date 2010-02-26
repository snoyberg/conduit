module Network.Wai.Source
    ( toEnumerator
    , toLBS
    ) where

import Network.Wai
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe (unsafeInterleaveIO)

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

toLBS :: Source -> IO L.ByteString
toLBS (Source b0 source) = L.fromChunks `fmap` helper b0 where
    helper b = unsafeInterleaveIO $ do
        next <- source b
        case next of
            Nothing -> return []
            Just (bs, b') -> do
                rest <- helper b'
                return $ bs : rest
