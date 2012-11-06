{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif
-- | Allocate resources which are guaranteed to be released.
--
-- For more information, see <http://www.yesodweb.com/book/conduits>.
--
-- One point to note: all register cleanup actions live in the @IO@ monad, not
-- the main monad. This allows both more efficient code, and for monads to be
-- transformed.
module Control.Monad.Trans.Resource
    ( -- * Data types
      ResourceT
    , ResIO
    , ReleaseKey
      -- * Unwrap
    , runResourceT
      -- * Special actions
    , resourceForkIO
      -- * Monad transformation
    , transResourceT
      -- * A specific Exception transformer
    , ExceptionT (..)
    , runExceptionT_
      -- * Registering/releasing
    , allocate
    , register
    , release
    , resourceMask
      -- * Type class/associated types
    , MonadResource (..)
    , MonadUnsafeIO (..)
    , MonadThrow (..)
    , MonadActive (..)
    , MonadResourceBase
      -- ** Low-level
    , InvalidAccess (..)
      -- * Re-exports
    , MonadBaseControl
    ) where

import Data.Typeable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Exception (SomeException, throw, Exception)
import Control.Monad.Trans.Control
    ( MonadTransControl (..), MonadBaseControl (..)
    , ComposeSt, defaultLiftBaseWith, defaultRestoreM
    , liftBaseDiscard, control
    )
import qualified Data.IORef as I
import Control.Monad.Base (MonadBase, liftBase)
import Control.Applicative (Applicative (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (liftM)
import qualified Control.Exception as E
import Data.Monoid (Monoid)
import qualified Control.Exception.Lifted as L

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )

import Control.Monad.Cont.Class   ( MonadCont (..) )
import Control.Monad.Error.Class  ( MonadError (..) )
import Control.Monad.RWS.Class    ( MonadRWS )
import Control.Monad.Reader.Class ( MonadReader (..) )
import Control.Monad.State.Class  ( MonadState (..) )
import Control.Monad.Writer.Class ( MonadWriter (..) )

import Data.Word (Word)

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import Control.Concurrent (ThreadId, forkIO)

import Control.Monad.ST (ST)
#if __GLASGOW_HASKELL__ >= 704
import Control.Monad.ST.Unsafe (unsafeIOToST)
#else
import Control.Monad.ST (unsafeIOToST)
#endif

#if __GLASGOW_HASKELL__ >= 704
import qualified Control.Monad.ST.Lazy.Unsafe as LazyUnsafe
#else
import qualified Control.Monad.ST.Lazy as LazyUnsafe
#endif

import qualified Control.Monad.ST.Lazy as Lazy

import Data.Functor.Identity (Identity)

-- | A lookup key for a specific release action. This value is returned by
-- 'register' and 'allocate', and is passed to 'release'.
--
-- Since 0.3.0
newtype ReleaseKey = ReleaseKey Int
    deriving Typeable

type RefCount = Word
type NextKey = Int

data ReleaseMap =
    ReleaseMap !NextKey !RefCount !(IntMap (IO ()))
  | ReleaseMapClosed

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

-- | Convenient alias for @ResourceT IO@.
type ResIO a = ResourceT IO a

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


-- | A @Monad@ which allows for safe resource allocation. In theory, any monad
-- transformer stack included a @ResourceT@ can be an instance of
-- @MonadResource@.
--
-- Note: @runResourceT@ has a requirement for a @MonadBaseControl IO m@ monad,
-- which allows control operations to be lifted. A @MonadResource@ does not
-- have this requirement. This means that transformers such as @ContT@ can be
-- an instance of @MonadResource@. However, the @ContT@ wrapper will need to be
-- unwrapped before calling @runResourceT@.
--
-- Since 0.3.0
class (MonadThrow m, MonadUnsafeIO m, MonadIO m, Applicative m) => MonadResource m where
    -- | Lift a @ResourceT IO@ action into the current @Monad@.
    --
    -- Since 0.4.0
    liftResourceT :: ResourceT IO a -> m a

-- | Register some action that will be called precisely once, either when
-- 'runResourceT' is called, or when the 'ReleaseKey' is passed to 'release'.
--
-- Since 0.3.0
register :: MonadResource m => IO () -> m ReleaseKey
register = liftResourceT . registerRIO

-- | Call a release action early, and deregister it from the list of cleanup
-- actions to be performed.
--
-- Since 0.3.0
release :: MonadResource m => ReleaseKey -> m ()
release = liftResourceT . releaseRIO

-- | Perform some allocation, and automatically register a cleanup action.
--
-- This is almost identical to calling the allocation and then
-- @register@ing the release action, but this properly handles masking of
-- asynchronous exceptions.
--
-- Since 0.3.0
allocate :: MonadResource m
         => IO a -- ^ allocate
         -> (a -> IO ()) -- ^ free resource
         -> m (ReleaseKey, a)
allocate a = liftResourceT . allocateRIO a

-- | Perform asynchronous exception masking.
--
-- This is more general then @Control.Exception.mask@, yet more efficient
-- than @Control.Exception.Lifted.mask@.
--
-- Since 0.3.0
resourceMask :: MonadResource m => ((forall a. ResourceT IO a -> ResourceT IO a) -> ResourceT IO b) -> m b
resourceMask = liftResourceT . resourceMaskRIO

instance (MonadThrow m, MonadUnsafeIO m, MonadIO m, Applicative m) => MonadResource (ResourceT m) where
    liftResourceT = transResourceT liftIO

allocateRIO :: IO a -> (a -> IO ()) -> ResourceT IO (ReleaseKey, a)
allocateRIO acquire rel = ResourceT $ \istate -> liftIO $ E.mask $ \restore -> do
    a <- restore acquire
    key <- register' istate $ rel a
    return (key, a)

registerRIO :: IO () -> ResourceT IO ReleaseKey
registerRIO rel = ResourceT $ \istate -> liftIO $ register' istate rel

releaseRIO :: ReleaseKey -> ResourceT IO ()
releaseRIO rk = ResourceT $ \istate -> liftIO $ release' istate rk

resourceMaskRIO :: ((forall a. ResourceT IO a -> ResourceT IO a) -> ResourceT IO b) -> ResourceT IO b
resourceMaskRIO f = ResourceT $ \istate -> liftIO $ E.mask $ \restore ->
    let ResourceT f' = f (go restore)
     in f' istate
  where
    go :: (forall a. IO a -> IO a) -> (forall a. ResourceT IO a -> ResourceT IO a)
    go r (ResourceT g) = ResourceT (\i -> r (g i))

#define GO(T) instance (MonadResource m) => MonadResource (T m) where liftResourceT = lift . liftResourceT
#define GOX(X, T) instance (X, MonadResource m) => MonadResource (T m) where liftResourceT = lift . liftResourceT
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
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

register' :: I.IORef ReleaseMap
          -> IO ()
          -> IO ReleaseKey
register' istate rel = I.atomicModifyIORef istate $ \rm ->
    case rm of
        ReleaseMap key rf m ->
            ( ReleaseMap (key - 1) rf (IntMap.insert key rel m)
            , ReleaseKey key
            )
        ReleaseMapClosed -> throw $ InvalidAccess "register'"

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

release' :: I.IORef ReleaseMap
         -> ReleaseKey
         -> IO ()
release' istate (ReleaseKey key) = E.mask $ \restore -> key `seq` do
    maction <- I.atomicModifyIORef istate lookupAction
    maybe (return ()) restore maction
  where
    lookupAction rm@(ReleaseMap next rf m) =
        case IntMap.lookup key m of
            Nothing -> (rm, Nothing)
            Just action ->
                ( ReleaseMap next rf $ IntMap.delete key m
                , Just action
                )
    lookupAction ReleaseMapClosed = throw $ InvalidAccess "release'"

stateAlloc :: I.IORef ReleaseMap -> IO ()
stateAlloc istate = do
    I.atomicModifyIORef istate $ \rm ->
        case rm of
            ReleaseMap nk rf m ->
                (ReleaseMap nk (rf + 1) m, ())
            ReleaseMapClosed -> throw $ InvalidAccess "stateAlloc"

stateCleanup :: I.IORef ReleaseMap -> IO ()
stateCleanup istate = E.mask_ $ do
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
            mapM_ (\x -> try x >> return ()) $ IntMap.elems m
        Nothing -> return ()
  where
    try :: IO a -> IO (Either SomeException a)
    try = E.try

-- | Unwrap a 'ResourceT' transformer, and call all registered release actions.
--
-- Note that there is some reference counting involved due to 'resourceForkIO'.
-- If multiple threads are sharing the same collection of resources, only the
-- last call to @runResourceT@ will deallocate the resources.
--
-- Since 0.3.0
runResourceT :: MonadBaseControl IO m => ResourceT m a -> m a
runResourceT (ResourceT r) = do
    istate <- liftBase $ I.newIORef
        $ ReleaseMap maxBound minBound IntMap.empty
    bracket_
        (stateAlloc istate)
        (stateCleanup istate)
        (r istate)

bracket_ :: MonadBaseControl IO m => IO () -> IO () -> m a -> m a
bracket_ alloc cleanup inside =
    control $ \run -> E.bracket_ alloc cleanup (run inside)

-- | Transform the monad a @ResourceT@ lives in. This is most often used to
-- strip or add new transformers to a stack, e.g. to run a @ReaderT@.
--
-- Since 0.3.0
transResourceT :: (m a -> n b)
               -> ResourceT m a
               -> ResourceT n b
transResourceT f (ResourceT mx) = ResourceT (\r -> f (mx r))

-------- All of our monad et al instances
instance Functor m => Functor (ResourceT m) where
    fmap f (ResourceT m) = ResourceT $ \r -> fmap f (m r)

instance Applicative m => Applicative (ResourceT m) where
    pure = ResourceT . const . pure
    ResourceT mf <*> ResourceT ma = ResourceT $ \r ->
        mf r <*> ma r

instance Monad m => Monad (ResourceT m) where
    return = ResourceT . const . return
    ResourceT ma >>= f = ResourceT $ \r -> do
        a <- ma r
        let ResourceT f' = f a
        f' r

instance MonadTrans ResourceT where
    lift = ResourceT . const

instance MonadIO m => MonadIO (ResourceT m) where
    liftIO = lift . liftIO

instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase = lift . liftBase

instance MonadTransControl ResourceT where
    newtype StT ResourceT a = StReader {unStReader :: a}
    liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> liftM StReader $ t r
    restoreT = ResourceT . const . liftM unStReader
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
     newtype StM (ResourceT m) a = StMT (StM m a)
     liftBaseWith f = ResourceT $ \reader ->
         liftBaseWith $ \runInBase ->
             f $ liftM StMT . runInBase . (\(ResourceT r) -> r reader)
     restoreM (StMT base) = ResourceT $ const $ restoreM base
instance Monad m => MonadThrow (ExceptionT m) where
    monadThrow = ExceptionT . return . Left . E.toException


-- | The express purpose of this transformer is to allow non-@IO@-based monad
-- stacks to catch exceptions via the 'MonadThrow' typeclass.
--
-- Since 0.3.0
newtype ExceptionT m a = ExceptionT { runExceptionT :: m (Either SomeException a) }

-- | Same as 'runExceptionT', but immediately 'E.throw' any exception returned.
--
-- Since 0.3.0
runExceptionT_ :: Monad m => ExceptionT m a -> m a
runExceptionT_ = liftM (either E.throw id) . runExceptionT

instance Monad m => Functor (ExceptionT m) where
    fmap f = ExceptionT . (liftM . fmap) f . runExceptionT
instance Monad m => Applicative (ExceptionT m) where
    pure = ExceptionT . return . Right
    ExceptionT mf <*> ExceptionT ma = ExceptionT $ do
        ef <- mf
        case ef of
            Left e -> return (Left e)
            Right f -> do
                ea <- ma
                case ea of
                    Left e -> return (Left e)
                    Right x -> return (Right (f x))
instance Monad m => Monad (ExceptionT m) where
    return = pure
    ExceptionT ma >>= f = ExceptionT $ do
        ea <- ma
        case ea of
            Left e -> return (Left e)
            Right a -> runExceptionT (f a)
instance MonadBase b m => MonadBase b (ExceptionT m) where
    liftBase = lift . liftBase
instance MonadTrans ExceptionT where
    lift = ExceptionT . liftM Right
instance MonadTransControl ExceptionT where
    newtype StT ExceptionT a = StExc { unStExc :: Either SomeException a }
    liftWith f = ExceptionT $ liftM return $ f $ liftM StExc . runExceptionT
    restoreT = ExceptionT . liftM unStExc
instance MonadBaseControl b m => MonadBaseControl b (ExceptionT m) where
    newtype StM (ExceptionT m) a = StE { unStE :: ComposeSt ExceptionT m a }
    liftBaseWith = defaultLiftBaseWith StE
    restoreM = defaultRestoreM unStE

instance MonadCont m => MonadCont (ExceptionT m) where
  callCC f = ExceptionT $
    callCC $ \c ->
    runExceptionT (f (\a -> ExceptionT $ c (Right a)))

instance MonadError e m => MonadError e (ExceptionT m) where
  throwError = lift . throwError
  catchError r h = ExceptionT $ runExceptionT r `catchError` (runExceptionT . h)

instance MonadRWS r w s m => MonadRWS r w s (ExceptionT m)

instance MonadReader r m => MonadReader r (ExceptionT m) where
  ask = lift ask
  local = mapExceptionT . local

mapExceptionT :: (m (Either SomeException a) -> n (Either SomeException b)) -> ExceptionT m a -> ExceptionT n b
mapExceptionT f = ExceptionT . f . runExceptionT

instance MonadState s m => MonadState s (ExceptionT m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (ExceptionT m) where
  tell   = lift . tell
  listen = mapExceptionT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a
  pass   = mapExceptionT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Left  l      -> (Left  l, id)
        Right (r, f) -> (Right r, f)

-- | A @Monad@ which can throw exceptions. Note that this does not work in a
-- vanilla @ST@ or @Identity@ monad. Instead, you should use the 'ExceptionT'
-- transformer in your stack if you are dealing with a non-@IO@ base monad.
--
-- Since 0.3.0
class Monad m => MonadThrow m where
    monadThrow :: E.Exception e => e -> m a

instance MonadThrow IO where
    monadThrow = E.throwIO

#define GO(T) instance (MonadThrow m) => MonadThrow (T m) where monadThrow = lift . monadThrow
#define GOX(X, T) instance (X, MonadThrow m) => MonadThrow (T m) where monadThrow = lift . monadThrow
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(ContT r)
GO(ResourceT)
GO(StateT s)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
#undef GO
#undef GOX

-- | Introduce a reference-counting scheme to allow a resource context to be
-- shared by multiple threads. Once the last thread exits, all remaining
-- resources will be released.
--
-- Note that abuse of this function will greatly delay the deallocation of
-- registered resources. This function should be used with care. A general
-- guideline:
--
-- If you are allocating a resource that should be shared by multiple threads,
-- and will be held for a long time, you should allocate it at the beginning of
-- a new @ResourceT@ block and then call @resourceForkIO@ from there.
--
-- Since 0.3.0
resourceForkIO :: MonadBaseControl IO m => ResourceT m () -> ResourceT m ThreadId
resourceForkIO (ResourceT f) = ResourceT $ \r -> L.mask $ \restore ->
    -- We need to make sure the counter is incremented before this call
    -- returns. Otherwise, the parent thread may call runResourceT before
    -- the child thread increments, and all resources will be freed
    -- before the child gets called.
    bracket_
        (stateAlloc r)
        (return ())
        (liftBaseDiscard forkIO $ bracket_
            (return ())
            (stateCleanup r)
            (restore $ f r))

-- | A @Monad@ based on some monad which allows running of some 'IO' actions,
-- via unsafe calls. This applies to 'IO' and 'ST', for instance.
--
-- Since 0.3.0
class Monad m => MonadUnsafeIO m where
    unsafeLiftIO :: IO a -> m a

instance MonadUnsafeIO IO where
    unsafeLiftIO = id

instance MonadUnsafeIO (ST s) where
    unsafeLiftIO = unsafeIOToST

instance MonadUnsafeIO (Lazy.ST s) where
    unsafeLiftIO = LazyUnsafe.unsafeIOToST

instance (MonadTrans t, MonadUnsafeIO m, Monad (t m)) => MonadUnsafeIO (t m) where
    unsafeLiftIO = lift . unsafeLiftIO

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

-- | A @Monad@ which can be used as a base for a @ResourceT@.
--
-- A @ResourceT@ has some restrictions on its base monad:
--
-- * @runResourceT@ requires an instance of @MonadBaseControl IO@.
-- * @MonadResource@ requires an instance of @MonadThrow@, @MonadUnsafeIO@, @MonadIO@, and @Applicative@.
--
-- While any instance of @MonadBaseControl IO@ should be an instance of the
-- other classes, this is not guaranteed by the type system (e.g., you may have
-- a transformer in your stack with does not implement @MonadThrow@). Ideally,
-- we would like to simply create an alias for the five type classes listed,
-- but this is not possible with GHC currently.
--
-- Instead, this typeclass acts as a proxy for the other five. Its only purpose
-- is to make your type signatures shorter.
--
-- Note that earlier versions of @conduit@ had a typeclass @ResourceIO@. This
-- fulfills much the same role.
--
-- Since 0.3.2
#if __GLASGOW_HASKELL__ >= 704
type MonadResourceBase m = (MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, MonadIO m, Applicative m)
#else
class (MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, MonadIO m, Applicative m) => MonadResourceBase m
instance (MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, MonadIO m, Applicative m) => MonadResourceBase m
#endif
