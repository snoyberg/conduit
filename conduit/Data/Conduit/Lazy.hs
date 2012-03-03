{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I\/O for consuming the contents of a source. Warning: All normal
-- warnings of lazy I\/O apply. However, if you consume the content within the
-- ResourceT, you should be safe.
module Data.Conduit.Lazy
    ( lazyConsume
    ) where

import Data.Conduit
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (resourceActive)

-- | Use lazy I\/O to consume all elements from a @Source@.
--
-- Since 0.2.0
lazyConsume :: (MonadBaseControl IO m, MonadIO m) => Source (ResourceT m) a -> ResourceT m [a] -- FIXME provide an alternative that doesn't live in ResourceT?
lazyConsume Closed = return []
lazyConsume (Open src _ x) = do
    xs <- lazyConsume src
    return $ x : xs
lazyConsume (SourceM msrc _) = liftBaseOp_ unsafeInterleaveIO $ do
    ra <- resourceActive
    if ra
        then msrc >>= lazyConsume
        else return []
