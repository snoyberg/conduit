{- | Functions currently under development which have not been moved to their
     final destination.
-}

module Data.Conduit.Extra.Utils where

import           Control.Applicative
import           Control.Monad.Loops
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Conduit
import           Data.Conduit.List as CL
import           Data.Foldable
import           Data.Sequence as Seq
import           Data.Vector as Boxed (Vector, freeze)
import           Data.Vector.Mutable as Boxed hiding (length)
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Unboxed

takeWhile :: Monad m => (a -> Bool) -> Conduit a m a
takeWhile f = loop where
    loop = await >>= maybe (return ()) go
    go x | f x = yield x >> loop
         | otherwise = leftover x

collect :: PrimMonad m => Int -> Sink a m (Vector a)
collect size = do
    v <- lift $ unsafeNew size
    forM_ [0..size-1] $ \i -> do
        me <- await
        case me of
            Nothing ->
                error $ "Too many elements for a vector of size "
                     ++ show size
            Just e  -> lift $ unsafeWrite v i e
    lift $ freeze v

collectUnboxed :: (PrimMonad m, Unboxed.Unbox a)
               => Int -> Sink a m (Unboxed.Vector a)
collectUnboxed size = do
    v <- lift $ Unboxed.unsafeNew size
    forM_ [0..size-1] $ \i -> do
        me <- await
        case me of
            Nothing ->
                error $ "Too many elements for an unboxed vector of size "
                     ++ show size
            Just e  -> lift $ Unboxed.unsafeWrite v i e
    lift $ Unboxed.freeze v

-- | Remove the last N elements from the stream.  This requires holding up to
--   N elements in memory.
dropRight :: Monad m => Int -> Conduit a m a
dropRight size = do
    xs <- Seq.fromList <$> CL.take size
    flip evalStateT xs $ whileM_ ((== size) . Seq.length <$> get) $ do
        xs' <- get
        case viewl xs' of
            EmptyL -> error "impossible"
            y :< ys -> do
                mz <- lift await
                case mz of
                    Nothing -> put Seq.empty
                    Just z  -> put (ys |> z) >> lift (yield y)
