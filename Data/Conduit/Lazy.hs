{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I/O. Warning: All normal warnings of lazy I/O apply. However, if
-- you consume the content within the ResourceT, you should be safe.
module Data.Conduit.Lazy
    ( lazyConsume
    ) where

import Data.Conduit
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans.Resource

lazyConsume :: SourceM IO a -> ResourceT IO [a]
lazyConsume (SourceM msrc) = msrc >>= go

go :: Source IO a -> ResourceT IO [a]
go src =
    ResourceT go'
  where
    go' r = unsafeInterleaveIO $ do
        let (ResourceT msx) = sourcePull src
        sx <- msx r
        case sx of
            EOF x -> return x
            Chunks x -> do
                y <- go' r
                return $ x ++ y
