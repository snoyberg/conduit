{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I/O. Warning: All normal warnings of lazy I/O apply. However, if
-- you consume the content within the ResourceT, you should be safe.
module Data.Conduit.Lazy
    ( lazyConsume
    ) where

import Data.Conduit
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

lazyConsume :: MonadBaseControl IO m => SourceM m a -> ResourceT m [a]
lazyConsume (SourceM (ResourceT msrc)) = ResourceT $ \r -> msrc r >>= go r

run r (ResourceT f) = f r

--go :: MonadBaseControl IO m => Source m a -> m [a]
go r src = liftBaseOp_ unsafeInterleaveIO $ do
    SourceResult state x <- run r $ sourcePull src
    case state of
        StreamClosed -> return x
        StreamOpen -> do
            y <- go r src
            return $ x ++ y
