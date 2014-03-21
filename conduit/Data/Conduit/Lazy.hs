{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Use lazy I\/O for consuming the contents of a source. Warning: All normal
-- warnings of lazy I\/O apply. In particular, if you are using this with a
-- @ResourceT@ transformer, you must force the list to be evaluated before
-- exiting the @ResourceT@.
module Data.Conduit.Lazy
    ( lazyConsume
    , MonadActive (..)
    ) where

import Data.Conduit
import Data.Conduit.Internal (Pipe (..), unConduitM)
import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Monad.Trans.Control (liftBaseOp_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

import Data.Monoid (Monoid)
import Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as Lazy
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.Resource.Internal (ResourceT (ResourceT), ReleaseMap (ReleaseMapClosed))
import qualified Data.IORef as I

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
        xs <- liftBaseOp_ unsafeInterleaveIO $ go src
        return $ x : xs
    go (PipeM msrc) = liftBaseOp_ unsafeInterleaveIO $ do
        a <- monadActive
        if a
            then msrc >>= go
            else return []
    go (NeedInput _ c) = go (c ())
    go (Leftover p _) = go p

-- | Determine if some monad is still active. This is intended to prevent usage
-- of a monadic state after it has been closed.  This is necessary for such
-- cases as lazy I\/O, where an unevaluated thunk may still refer to a
-- closed @ResourceT@.
--
-- Since 0.3.0
class Monad m => MonadActive m where
    monadActive :: m Bool

instance (MonadIO m, MonadActive m) => MonadActive (ResourceT m) where
    monadActive = ResourceT $ \rmMap -> do
        rm <- liftIO $ I.readIORef rmMap
        case rm of
            ReleaseMapClosed -> return False
            _ -> monadActive -- recurse

instance MonadActive Identity where
    monadActive = return True

instance MonadActive IO where
    monadActive = return True

instance MonadActive (ST s) where
    monadActive = return True

instance MonadActive (Lazy.ST s) where
    monadActive = return True

#define GO(T) instance MonadActive m => MonadActive (T m) where monadActive = lift monadActive
#define GOX(X, T) instance (X, MonadActive m) => MonadActive (T m) where monadActive = lift monadActive
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(StateT s)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
#undef GO
#undef GOX

instance MonadActive m => MonadActive (Pipe l i o u m) where
    monadActive = lift monadActive
instance MonadActive m => MonadActive (ConduitM i o m) where
    monadActive = lift monadActive
