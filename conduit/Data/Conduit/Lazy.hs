{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I\/O for consuming the contents of a source. Warning: All normal
-- warnings of lazy I\/O apply. In particular, if you are using this with a
-- @ResourceT@ transformer, you must force the list to be evaluated before
-- exiting the @ResourceT@.
module Data.Conduit.Lazy
    ( lazyConsume
    ) where

import Data.Conduit
import Data.Conduit.Internal (Pipe (..), unConduitM)
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans.Control (liftBaseOp_)
import Control.Monad.Trans.Resource (MonadActive (monadActive))

-- | Use lazy I\/O to consume all elements from a @Source@.
--
-- This function relies on 'monadActive' to determine if the underlying monadic
-- state has been closed.
--
-- Since 0.3.0
lazyConsume :: (MonadBaseControl IO m, MonadActive m) => Source m a -> m [a]
lazyConsume =
    go . unConduitM
  where
    go (Done _) = return []
    go (HaveOutput src _ x) = do
        xs <- go src
        return $ x : xs
    go (PipeM msrc) = liftBaseOp_ unsafeInterleaveIO $ do
        a <- monadActive
        if a
            then msrc >>= go
            else return []
    go (NeedInput _ c) = go (c ())
    go (Leftover p _) = go p
