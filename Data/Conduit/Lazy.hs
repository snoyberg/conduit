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

lazyConsume :: MonadBaseControl IO m => Source m a -> ResourceT m [a]
lazyConsume (Source (ResourceT msrc)) =
    ResourceT $ \r -> msrc r >>= go r
  where

    run r (ResourceT f) = f r

    go r src = liftBaseOp_ unsafeInterleaveIO $ do
        SourceResult state x <- run r $ sourcePull src
        case state of
            StreamClosed -> return x
            StreamOpen -> do
                y <- go r src
                return $ x ++ y
