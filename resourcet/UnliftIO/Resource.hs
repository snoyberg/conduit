-- | Unlifted "Control.Monad.Trans.Resource".
--
-- @since 1.1.10
module UnliftIO.Resource
  ( ResourceT
  , runResourceT
  , liftResourceT
  -- FIXME add relevant reexports
  ) where

import qualified Control.Monad.Trans.Resource as Res
import Control.Monad.Trans.Resource.Internal (ResourceT (..))
import Control.Monad.IO.Unlift

-- | Unlifted version of 'Res.runResourceT'.
--
-- @since 1.1.10
runResourceT :: MonadUnliftIO m => ResourceT m a -> m a
runResourceT m = withRunInIO $ \run -> Res.runResourceT $ Res.transResourceT run m

-- | Lifted version of 'Res.liftResourceT'.
--
-- @since 1.1.10
liftResourceT :: MonadIO m => ResourceT IO a -> ResourceT m a
liftResourceT (ResourceT f) = ResourceT $ liftIO . f
