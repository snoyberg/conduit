{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I\/O for consuming the contents of a source. Warning: All normal
-- warnings of lazy I\/O apply. However, if you consume the content within the
-- ResourceT, you should be safe.
module Data.Conduit.Lazy
    ( lazyConsume
    ) where

import Data.Conduit
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans.Control

-- | Use lazy I\/O to consume all elements from a @Source@.
--
-- Since 0.0.0
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
