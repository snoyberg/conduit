{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I\/O for consuming the contents of a source. Warning: All normal
-- warnings of lazy I\/O apply. In particular, if you are using this with a
-- @ResourceT@ transformer, you must force the list to be evaluated before
-- exiting the @ResourceT@.
module Data.Conduit.Lazy
    ( lazyConsume
    ) where

import Data.Conduit
import Data.Conduit.Internal (Pipe (..))
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.Trans.Control (liftBaseOp_)
import Control.Monad.Trans.Resource (MonadActive (monadActive))

-- | Use lazy I\/O to consume all elements from a @Source@.
--
-- This function relies on 'monadActive' to determine if the underlying monadic
-- state has been closed.
--
-- Since 0.3.0
lazyConsume :: (MonadBaseControl IO m, MonadActive m) => Pipe l i a () m r -> m [a]
lazyConsume (Done _) = return []
lazyConsume (HaveOutput src _ x) = do
    xs <- lazyConsume src
    return $ x : xs
lazyConsume (PipeM msrc) = liftBaseOp_ unsafeInterleaveIO $ do
    a <- monadActive
    if a
        then msrc >>= lazyConsume
        else return []
lazyConsume (NeedInput _ c) = lazyConsume (c ())
lazyConsume (Leftover p _) = lazyConsume p
