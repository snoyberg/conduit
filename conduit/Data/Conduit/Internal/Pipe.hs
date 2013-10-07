{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Conduit.Internal.Pipe
    ( Pipe (..)
    , (.:)
    ) where

import Control.Monad.Error.Class(MonadError(..))
import Control.Monad.Reader.Class(MonadReader(..))
import Control.Monad.RWS.Class(MonadRWS())
import Control.Monad.Writer.Class(MonadWriter(..))
import Control.Monad.State.Class(MonadState(..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans.Resource
import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Monoid (Monoid (mappend, mempty))
import Control.Monad ((>=>), liftM, ap)

(.:) :: (y -> z)
     -> (w -> x -> y)
     -> (w -> x -> z)
(.:) f g x y = f (g x y)

data Pipe i o d t m r
    = Pure [i] r
    | M (m (Pipe i o d t m r))
    | Yield (Pipe i o d t m r) ([o] -> d -> Pipe i o d t m r) o
    | Empty ([o] -> d -> Pipe i o d t m r)
    | Await (i -> Pipe i o d t m r) (Pipe i o d t m r)
    | Check (Pipe i o d t m r) ([o] -> d -> Pipe i o d t m r)
    | Terminate [i] t

instance Monad m => Functor (Pipe i o d t m) where
    fmap = liftM

instance Monad m => Applicative (Pipe i o d t m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Pipe i o d t m) where
    return = Pure []

    Pure [] r >>= f = f r
    Pure is0 r0 >>= f =
        inject is0 (f r0)
      where
        inject :: Monad m
               => [i]
               -> Pipe i o d t m r
               -> Pipe i o d t m r
        inject [] = id
        inject leftovers@(i:is) =
            go
          where
            go (Pure is' r) = Pure (is' ++ leftovers) r
            go (M m) = M (liftM go m)
            go (Yield more done o) = Yield (go more) (go .: done) o
            go (Empty done) = Empty (go .: done)
            go (Await more _) = inject is (more i)
            go (Check more done) = Check (go more) (go .: done)
            go (Terminate is' t) = Terminate (is' ++ leftovers) t
    M m >>= f = M (liftM (>>= f) m)
    Yield next done o >>= f = Yield (next >>= f) (\x y -> done x y >>= f) o
    Empty done >>= f = Empty (\x y -> done x y >>= f)
    Await next done >>= f = Await (next >=> f) (done >>= f)
    Check next done >>= f = Check (next >>= f) (\x y -> done x y >>= f)
    Terminate is t >>= _ = Terminate is t
    {-# INLINE (>>=) #-}

instance MonadBase base m => MonadBase base (Pipe i o d t m) where
    liftBase = lift . liftBase

instance MonadTrans (Pipe i o d t) where
    lift = M . liftM (Pure [])

instance MonadIO m => MonadIO (Pipe i o d t m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (Pipe i o d t m) where
    monadThrow = lift . monadThrow

instance MonadActive m => MonadActive (Pipe i o d t m) where
    monadActive = lift monadActive

instance Monad m => Monoid (Pipe i o d t m ()) where
    mempty = return ()
    mappend = (>>)

instance MonadResource m => MonadResource (Pipe i o d t m) where
    liftResourceT = lift . liftResourceT

instance MonadReader r m => MonadReader r (Pipe i o d t m) where
    ask = lift ask
    local f (Yield p d o) = Yield (local f p) (local f .: d) o
    local f (Check p d) = Check (local f p) (local f .: d)
    local f (Empty d) = Empty (local f .: d)
    local f (Await p d) = Await (local f . p) (local f d)
    local _ (Pure is x) = Pure is x
    local _ (Terminate is t) = Terminate is t
    local f (M mp) = M (local f mp)

-- Provided for doctest
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x, y, z) 0
#endif

instance MonadWriter w m => MonadWriter w (Pipe i o d t m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif

    tell = lift . tell

    listen (Yield p d o) = Yield (listen p) (listen .: d) o
    listen (Await p d) = Await (listen . p) (listen d)
    listen (Pure is x) = Pure is (x, mempty)
    listen (M mp) =
      M $
      do (p,w) <- listen mp
         return $ do (x,w') <- listen p
                     return (x, w `mappend` w')
    listen (Empty d) = Empty (listen .: d)
    listen (Check p d) = Check (listen p) (listen .: d)
    listen (Terminate is t) = Terminate is t

    pass (Yield p d o) = Yield (pass p) (pass .: d) o
    pass (Await p d) = Await (pass . p) (pass d)
    pass (M mp) = M $ mp >>= (return . pass)
    pass (Pure is (x,w)) = M $ pass $ return (Pure is x, w)
    pass (Empty d) = Empty (pass .: d)
    pass (Check p d) = Check (pass p) (pass .: d)
    pass (Terminate is t) = Terminate is t

instance MonadState s m => MonadState s (Pipe i o d t m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (Pipe i o d t m)

instance MonadError e m => MonadError e (Pipe i o d t m) where
    throwError = lift . throwError
    catchError (Yield p d o) f = Yield (catchError p f) (\x y -> catchError (d x y) f) o
    catchError (Check more done) f = Check (catchError more f) (\x y -> catchError (done x y) f)
    catchError (Await p d) f = Await (\i -> catchError (p i) f) (catchError d f)
    catchError (Pure is x) _ = Pure is x
    catchError (Terminate is t) _ = Terminate is t
    catchError (Empty done) f = Empty (\x y -> catchError (done x y) f)
    catchError (M mp) f =
      M $ catchError (liftM (flip catchError f) mp) (\e -> return (f e))

instance MFunctor (Pipe i o d t) where
    hoist f =
        go
      where
        go (Yield p d o) = Yield (go p) (go .: d) o
        go (Await p d) = Await (go . p) (go d)
        go (Pure is r) = Pure is r
        go (Check p d) = Check (go p) (go .: d)
        go (Empty p) = Empty (go .: p)
        go (Terminate is t) = Terminate is t
        go (M mp) =
            M (f $ liftM go $ collapse mp)
          where
            -- Combine a series of monadic actions into a single action.  Since we
            -- throw away side effects between different actions, an arbitrary break
            -- between actions will lead to a violation of the monad transformer laws.
            -- Example available at:
            --
            -- http://hpaste.org/75520
            collapse mpipe = do
                pipe' <- mpipe
                case pipe' of
                    M mpipe' -> collapse mpipe'
                    _ -> return pipe'
