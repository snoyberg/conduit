{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I/O. Warning: All normal warnings of lazy I/O apply. However, if
-- you consume the content within the ResourceT, you should be safe.
module Data.Conduit.Lazy
    ( lazyConsume
    ) where

import Data.Conduit
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans.Control

lazyConsume :: MonadBaseControl IO m => SourceM m a -> ResourceT m [a]
lazyConsume (SourceM msrc) = msrc >>= go

go :: MonadBaseControl IO m => Source m a -> ResourceT m [a]
go src = liftBaseOp_ unsafeInterleaveIO $ do
    SourceResult state x <- sourcePull src
    case state of
        StreamClosed -> return x
        StreamOpen -> do
            y <- go src
            return $ x ++ y
