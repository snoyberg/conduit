{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
-- Can only mark as Safe when using a newer GHC, otherwise we get build
-- failures due to the manual Typeable instance below.
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

module Control.Monad.Trans.Resource.Internal(
    InvalidAccess(..)
  , MonadResource(..)
  , ReleaseKey(..)
  , ReleaseMap(..)
  , ResIO
  , ResourceT(..)
  , stateAlloc
  , stateCleanup
  , transResourceT
  , register'
  , registerType
) where

import Control.Exception (throw,Exception,SomeException)
import Control.Applicative (Applicative (..), Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Trans.Control
    ( MonadTransControl (..), MonadBaseControl (..) )
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Cont.Class   ( MonadCont (..) )
import Control.Monad.Error.Class  ( MonadError (..) )
import Control.Monad.RWS.Class    ( MonadRWS )
import Control.Monad.Reader.Class ( MonadReader (..) )
import Control.Monad.State.Class  ( MonadState (..) )
import Control.Monad.Writer.Class ( MonadWriter (..) )

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Except   ( ExceptT  )
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

import Control.Monad.IO.Class (MonadIO (..))
#if !(MIN_VERSION_monad_control(1,0,0))
import Control.Monad (liftM)
#endif
import qualified Control.Exception as E
import Control.Monad.Catch (MonadThrow (..), MonadCatch (..)
#if MIN_VERSION_exceptions(0,6,0)
    , MonadMask (..)
#endif
    )
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IORef as I
import Data.Monoid
import Data.Typeable
import Data.Word(Word)
import Prelude hiding (catch)
import Data.Acquire.Internal (ReleaseType (..))

import Control.Monad.Morph

-- | A @Monad@ which allows for safe resource allocation. In theory, any monad
-- transformer stack which includes a @ResourceT@ can be an instance of
-- @MonadResource@.
--
-- Note: @runResourceT@ has a requirement for a @MonadBaseControl IO m@ monad,
-- which allows control operations to be lifted. A @MonadResource@ does not
-- have this requirement. This means that transformers such as @ContT@ can be
-- an instance of @MonadResource@. However, the @ContT@ wrapper will need to be
-- unwrapped before calling @runResourceT@.
--
-- Since 0.3.0
class (MonadThrow m, MonadIO m, Applicative m, MonadBase IO m) => MonadResource m where
    -- | Lift a @ResourceT IO@ action into the current @Monad@.
    --
    -- Since 0.4.0
    liftResourceT :: ResourceT IO a -> m a


-- | A lookup key for a specific release action. This value is returned by
-- 'register' and 'allocate', and is passed to 'release'.
--
-- Since 0.3.0
data ReleaseKey = ReleaseKey !(I.IORef ReleaseMap) !Int
    deriving Typeable

type RefCount = Word
type NextKey = Int

data ReleaseMap =
    ReleaseMap !NextKey !RefCount !(IntMap (ReleaseType -> IO ()))
  | ReleaseMapClosed

-- | Convenient alias for @ResourceT IO@.
type ResIO a = ResourceT IO a


instance MonadCont m => MonadCont (ResourceT m) where
  callCC f = ResourceT $ \i -> callCC $ \c -> unResourceT (f (ResourceT . const . c)) i

instance MonadError e m => MonadError e (ResourceT m) where
  throwError = lift . throwError
  catchError r h = ResourceT $ \i -> unResourceT r i `catchError` \e -> unResourceT (h e) i

instance MonadRWS r w s m => MonadRWS r w s (ResourceT m)

instance MonadReader r m => MonadReader r (ResourceT m) where
  ask = lift ask
  local = mapResourceT . local

mapResourceT :: (m a -> n b) -> ResourceT m a -> ResourceT n b
mapResourceT f = ResourceT . (f .) . unResourceT

instance MonadState s m => MonadState s (ResourceT m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (ResourceT m) where
  tell   = lift . tell
  listen = mapResourceT listen
  pass   = mapResourceT pass

instance MonadThrow m => MonadThrow (ResourceT m) where
    throwM = lift . throwM
instance MonadCatch m => MonadCatch (ResourceT m) where
  catch (ResourceT m) c =
      ResourceT $ \r -> m r `catch` \e -> unResourceT (c e) r
#if MIN_VERSION_exceptions(0,6,0)
instance MonadMask m => MonadMask (ResourceT m) where
#endif
  mask a = ResourceT $ \e -> mask $ \u -> unResourceT (a $ q u) e
    where q u (ResourceT b) = ResourceT (u . b)
  uninterruptibleMask a =
    ResourceT $ \e -> uninterruptibleMask $ \u -> unResourceT (a $ q u) e
      where q u (ResourceT b) = ResourceT (u . b)
instance (MonadThrow m, MonadBase IO m, MonadIO m, Applicative m) => MonadResource (ResourceT m) where
    liftResourceT = transResourceT liftIO

-- | Transform the monad a @ResourceT@ lives in. This is most often used to
-- strip or add new transformers to a stack, e.g. to run a @ReaderT@.
--
-- Note that this function is a slight generalization of 'hoist'.
--
-- Since 0.3.0
transResourceT :: (m a -> n b)
               -> ResourceT m a
               -> ResourceT n b
transResourceT f (ResourceT mx) = ResourceT (\r -> f (mx r))

-- | Since 0.4.7
instance MFunctor ResourceT where
    hoist f (ResourceT mx) = ResourceT (\r -> f (mx r))
-- | Since 0.4.7
instance MMonad ResourceT where
    embed f m = ResourceT (\i -> unResourceT (f (unResourceT m i)) i)

-- | The Resource transformer. This transformer keeps track of all registered
-- actions, and calls them upon exit (via 'runResourceT'). Actions may be
-- registered via 'register', or resources may be allocated atomically via
-- 'allocate'. @allocate@ corresponds closely to @bracket@.
--
-- Releasing may be performed before exit via the 'release' function. This is a
-- highly recommended optimization, as it will ensure that scarce resources are
-- freed early. Note that calling @release@ will deregister the action, so that
-- a release action will only ever be called once.
--
-- Since 0.3.0
newtype ResourceT m a = ResourceT { unResourceT :: I.IORef ReleaseMap -> m a }
#if __GLASGOW_HASKELL__ >= 707
        deriving Typeable
#else
instance Typeable1 m => Typeable1 (ResourceT m) where
    typeOf1 = goType undefined
      where
        goType :: Typeable1 m => m a -> ResourceT m a -> TypeRep
        goType m _ =
            mkTyConApp
#if __GLASGOW_HASKELL__ >= 704
                (mkTyCon3 "resourcet" "Control.Monad.Trans.Resource" "ResourceT")
#else
                (mkTyCon "Control.Monad.Trans.Resource.ResourceT")
#endif
                [ typeOf1 m
                ]
#endif

-- | Indicates either an error in the library, or misuse of it (e.g., a
-- @ResourceT@'s state is accessed after being released).
--
-- Since 0.3.0
data InvalidAccess = InvalidAccess { functionName :: String }
    deriving Typeable

instance Show InvalidAccess where
    show (InvalidAccess f) = concat
        [ "Control.Monad.Trans.Resource."
        , f
        , ": The mutable state is being accessed after cleanup. Please contact the maintainers."
        ]

instance Exception InvalidAccess

-------- All of our monad et al instances
instance Functor m => Functor (ResourceT m) where
    fmap f (ResourceT m) = ResourceT $ \r -> fmap f (m r)

instance Applicative m => Applicative (ResourceT m) where
    pure = ResourceT . const . pure
    ResourceT mf <*> ResourceT ma = ResourceT $ \r ->
        mf r <*> ma r

-- | Since 1.1.5
instance Alternative m => Alternative (ResourceT m) where
    empty = ResourceT $ \_ -> empty
    (ResourceT mf) <|> (ResourceT ma) = ResourceT $ \r -> mf r <|> ma r

-- | Since 1.1.5
instance MonadPlus m => MonadPlus (ResourceT m) where
    mzero = ResourceT $ \_ -> mzero
    (ResourceT mf) `mplus` (ResourceT ma) = ResourceT $ \r -> mf r `mplus` ma r

instance Monad m => Monad (ResourceT m) where
#if !MIN_VERSION_base(4,8,0)
    return = ResourceT . const . return
#endif
    ResourceT ma >>= f = ResourceT $ \r -> do
        a <- ma r
        let ResourceT f' = f a
        f' r

-- | @since 1.1.8
instance MonadFix m => MonadFix (ResourceT m) where
  mfix f = ResourceT $ \r -> mfix $ \a -> unResourceT (f a) r

instance MonadTrans ResourceT where
    lift = ResourceT . const

instance MonadIO m => MonadIO (ResourceT m) where
    liftIO = lift . liftIO

instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase = lift . liftBase

instance MonadTransControl ResourceT where
#if MIN_VERSION_monad_control(1,0,0)
    type StT ResourceT a = a
    liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> t r
    restoreT = ResourceT . const
#else
    newtype StT ResourceT a = StReader {unStReader :: a}
    liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> liftM StReader $ t r
    restoreT = ResourceT . const . liftM unStReader
#endif
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
#if MIN_VERSION_monad_control(1,0,0)
     type StM (ResourceT m) a = StM m a
     liftBaseWith f = ResourceT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(ResourceT r) -> r reader'  )
     restoreM = ResourceT . const . restoreM
#else
     newtype StM (ResourceT m) a = StMT (StM m a)
     liftBaseWith f = ResourceT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ liftM StMT . runInBase . (\(ResourceT r) -> r reader'  )
     restoreM (StMT base) = ResourceT $ const $ restoreM base
#endif

#define GO(T) instance (MonadResource m) => MonadResource (T m) where liftResourceT = lift . liftResourceT
#define GOX(X, T) instance (X, MonadResource m) => MonadResource (T m) where liftResourceT = lift . liftResourceT
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
#if MIN_VERSION_exceptions(0, 8, 0)
GO(ExceptT e)
#endif
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
#undef GO
#undef GOX

stateAlloc :: I.IORef ReleaseMap -> IO ()
stateAlloc istate = do
    I.atomicModifyIORef istate $ \rm ->
        case rm of
            ReleaseMap nk rf m ->
                (ReleaseMap nk (rf + 1) m, ())
            ReleaseMapClosed -> throw $ InvalidAccess "stateAlloc"

stateCleanup :: ReleaseType -> I.IORef ReleaseMap -> IO ()
stateCleanup rtype istate = E.mask_ $ do
    mm <- I.atomicModifyIORef istate $ \rm ->
        case rm of
            ReleaseMap nk rf m ->
                let rf' = rf - 1
                 in if rf' == minBound
                        then (ReleaseMapClosed, Just m)
                        else (ReleaseMap nk rf' m, Nothing)
            ReleaseMapClosed -> throw $ InvalidAccess "stateCleanup"
    case mm of
        Just m ->
            mapM_ (\x -> try (x rtype) >> return ()) $ IntMap.elems m
        Nothing -> return ()
  where
    try :: IO a -> IO (Either SomeException a)
    try = E.try

register' :: I.IORef ReleaseMap
          -> IO ()
          -> IO ReleaseKey
register' istate rel = I.atomicModifyIORef istate $ \rm ->
    case rm of
        ReleaseMap key rf m ->
            ( ReleaseMap (key - 1) rf (IntMap.insert key (const rel) m)
            , ReleaseKey istate key
            )
        ReleaseMapClosed -> throw $ InvalidAccess "register'"

-- |
--
-- Since 1.1.2
registerType :: I.IORef ReleaseMap
             -> (ReleaseType -> IO ())
             -> IO ReleaseKey
registerType istate rel = I.atomicModifyIORef istate $ \rm ->
    case rm of
        ReleaseMap key rf m ->
            ( ReleaseMap (key - 1) rf (IntMap.insert key rel m)
            , ReleaseKey istate key
            )
        ReleaseMapClosed -> throw $ InvalidAccess "register'"
