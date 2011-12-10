{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Defines the types for a sink, which is a consumer of data.
module Data.Conduit.Types.Sink
    ( SinkResult (..)
    , Sink (..)
    , SinkM (..)
    ) where

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad (liftM)
import Control.Applicative (Applicative (..))
import Control.Monad.Base (MonadBase (liftBase))
import qualified Data.IORef as I

-- | When a sink is given data, it can return both leftover data and a result.
data SinkResult input output = SinkResult [input] output
instance Functor (SinkResult input) where
    fmap f (SinkResult input output) = SinkResult input (f output)

-- | In general, a sink will consume data and eventually produce an output when
-- it has consumed \"enough\" data. There are two caveats to that statement:
--
-- * Some sinks do not actually require any data to produce an output. This is
-- included with a sink in order to allow for a 'Monad' instance.
--
-- * Some sinks will consume all available data and only produce a result at
-- the \"end\" of a data stream (e.g., @sum@).
--
-- To allow for the first caveat, we have the 'SinkNoData' constructor. For the
-- second, the 'SinkData' constructor has two records: one for receiving more
-- input, and the other to indicate the end of a stream. Note that, at the end
-- of a stream, some output is required. If a specific 'Sink' implementation
-- cannot always produce output, this should be indicated in its return value,
-- using something like a 'Maybe' or 'Either'.
--
-- Invariants:
--
-- * After a 'Sink' produces a result (either via 'sinkPush' or 'sinkClose'),
-- neither of those two functions may be called on the @Sink@ again.
--
-- * If a @Sink@ needs to clean up any resources (e.g., close a file handle),
-- it must do so whenever it returns a result, either via @sinkPush@ or
-- @sinkClose@.
data Sink input m output =
    SinkNoData output
  | SinkData
        { sinkPush :: [input] -> ResourceT m (SinkResult input (Maybe output))
        , sinkClose :: ResourceT m (SinkResult input output)
        }

instance Monad m => Functor (Sink input m) where
    fmap f (SinkNoData x) = SinkNoData (f x)
    fmap f (SinkData p c) = SinkData
        { sinkPush = liftM (fmap (fmap f)) . p
        , sinkClose = liftM (fmap f) c
        }

-- | Most 'Sink's require some type of state, similar to 'Source's. Like a
-- @SourceM@ for a @Source@, a @SinkM@ is a simple monadic wrapper around a
-- @Sink@ which allows initialization of such state. See @SourceM@ for further
-- caveats.
--
-- Note that this type provides a 'Monad' instance, allowing you to easily
-- compose @SinkM@s together.
newtype SinkM input m output = SinkM { genSink :: ResourceT m (Sink input m output) }

instance Monad m => Functor (SinkM input m) where
    fmap f (SinkM msink) = SinkM (liftM (fmap f) msink)

instance MonadBase IO m => Applicative (SinkM input m) where
    pure x = SinkM (return (SinkNoData x))
    SinkM mf <*> SinkM ma = SinkM $ do
        f <- mf
        a <- ma
        case (f, a) of
            (SinkNoData f', SinkNoData a') -> return (SinkNoData (f' a'))
            _ -> do
                let toEither (SinkData x y) = SinkPair x y
                    toEither (SinkNoData x) = SinkOutput x
                istate <- liftBase (I.newIORef (toEither f, toEither a))
                return $ appHelper istate

type SinkPush input m output = [input] -> ResourceT m (SinkResult input (Maybe output))
type SinkClose input m output = ResourceT m (SinkResult input output)
data SinkEither input m output = SinkPair (SinkPush input m output) (SinkClose input m output) | SinkOutput output
type SinkState input m a b = I.IORef (SinkEither input m (a -> b), SinkEither input m a)

appHelper :: MonadBase IO m => SinkState input m a b -> Sink input m b
appHelper istate = SinkData (pushHelper istate) (closeHelper istate)

pushHelper :: MonadBase IO m
           => SinkState input m a b
           -> [input]
           -> ResourceT m (SinkResult input (Maybe b))
pushHelper istate stream0 = do
    state <- liftBase $ I.readIORef istate
    go state stream0
  where
    go (SinkPair f _, eb) stream = do
        SinkResult leftover mres <- f stream
        case mres of
            Nothing -> return $ SinkResult leftover Nothing
            Just res -> do
                let state' = (SinkOutput res, eb)
                liftBase $ I.writeIORef istate state'
                go state' leftover
    go (f@SinkOutput{}, SinkPair b _) stream = do
        SinkResult leftover mres <- b stream
        case mres of
            Nothing -> return $ SinkResult leftover Nothing
            Just res -> do
                let state' = (f, SinkOutput res)
                liftBase $ I.writeIORef istate state'
                go state' leftover
    go (SinkOutput f, SinkOutput b) leftover = return $ SinkResult leftover (Just $ f b)

closeHelper :: MonadBase IO m
            => SinkState input m a b
            -> ResourceT m (SinkResult input b)
closeHelper istate = do
    (sf, sa) <- liftBase $ I.readIORef istate
    case sf of
        SinkOutput f -> go' f sa
        SinkPair _ close -> do
            SinkResult leftover f <- close
            go f leftover sa
  where
    go f leftover sa
        | null leftover = go' f sa
    go f leftover (SinkOutput a) = return $ SinkResult leftover (f a)
    go f leftover sp@(SinkPair push _) = do
        SinkResult leftover' mres <- push leftover
        case mres of
            Just a -> return $ SinkResult leftover' (f a)
            Nothing -> go f leftover' sp
    go' f (SinkPair _ close) = do
        SinkResult leftover a <- close
        return $ SinkResult leftover (f a)
    go' f (SinkOutput a) = return $ SinkResult [] (f a)

instance MonadBase IO m => Monad (SinkM input m) where
    return = pure
    x >>= f = sinkJoin (fmap f x)

instance MonadBase IO m => MonadBase IO (SinkM input m) where
    liftBase = lift . liftBase

instance MonadTrans (SinkM input) where
    lift f = SinkM (lift (liftM SinkNoData f))

sinkJoin :: MonadBase IO m => SinkM a m (SinkM a m b) -> SinkM a m b
sinkJoin = error "sinkJoin"
{-
sinkJoin (Sink msinkI) = Sink $ do
    sinkI <- msinkI
    case sinkI of
        SinkNoData (Sink inner) -> inner
        SinkData msink -> do
            istate <- liftBase $ I.newIORef Nothing
            return $ SinkData $ go istate msink
  where
    go :: MonadBaseControl IO m
       => I.IORef (Maybe (SinkInsideData a m b))
       -> (Stream a -> ResourceT m (SinkResult a (Sink a m b)))
       -> SinkInsideData a m b
    go istate outer stream = do
        state <- liftBase $ I.readIORef istate
        case state of
            Nothing -> do
                SinkResult leftover minner' <- outer stream
                case minner' of
                    Just (Sink msink) -> do
                        sink <- msink
                        case sink of
                            SinkData inner -> do
                                liftBase $ I.writeIORef istate $ Just inner
                                if null leftover
                                    then return $ SinkResult [] Nothing
                                    else go istate outer $ Chunks leftover
                            SinkNoData x -> return $ SinkResult leftover $ Just x
                    Nothing -> return $ SinkResult leftover Nothing
            Just inner -> inner stream
            -}
