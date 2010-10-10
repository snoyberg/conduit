module Network.Wai.Zlib (compress) where

import Prelude
import Network.Wai
import Data.ByteString (ByteString)

import Codec.Zlib

compress :: Enumerator -> Enumerator
compress enum =
    Enumerator $ \iter acc -> do
        def <- initDeflate 7 $ WindowBits 31
        compressInner iter acc enum def

compressInner :: (acc -> ByteString -> IO (Either acc acc))
              -> acc
              -> Enumerator
              -> Deflate
              -> IO (Either acc acc)
compressInner iter acc enum def = do
    eacc <- runEnumerator enum (compressIter iter def) acc
    case eacc of
        Left acc' -> return $ Left acc'
        Right acc' -> finishStream iter def acc'

finishStream :: (acc -> ByteString -> IO (Either acc acc))
             -> Deflate
             -> acc
             -> IO (Either acc acc)
finishStream iter def acc = finishDeflate def $ drain iter acc

compressIter :: (acc -> ByteString -> IO (Either acc acc))
             -> Deflate
             -> acc
             -> ByteString
             -> IO (Either acc acc)
compressIter iter def acc bsI = withDeflateInput def bsI $ drain iter acc

drain :: (acc -> ByteString -> IO (Either acc acc))
      -> acc
      -> IO (Maybe ByteString)
      -> IO (Either acc acc)
drain iter acc pop = do
    mbs <- pop
    case mbs of
        Nothing -> return $ Right acc
        Just bs -> do
            eacc' <- iter acc bs
            case eacc' of
                Left acc' -> return $ Left acc'
                Right acc' -> drain iter acc' pop
