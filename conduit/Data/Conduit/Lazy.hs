{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I/O. Warning: All normal warnings of lazy I/O apply. However, if
-- you consume the content within the ResourceT, you should be safe.
module Data.Conduit.Lazy
    ( lazyConsume
    ) where

import Data.Conduit
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans.Control

lazyConsume :: MonadBaseControl IO m => Source m a -> ResourceT m [a]
lazyConsume (Source msrc) = do
    src <- msrc
    go src
  where

    go src = liftBaseOp_ unsafeInterleaveIO $ do
        res <- sourcePull src
        case res of
            Closed -> return []
            Open x -> do
                y <- go src
                return $ x : y
