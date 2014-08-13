{-# LANGUAGE ExistentialQuantification, BangPatterns, RankNTypes #-}
-- Idea: we have two different reps for monadic streams: a separate StepM
-- constructor, or having the step function in Stream live in a monad. Let's
-- test performance of each for both pure and impure code, and for a variety of
-- monad stacks (shallow and deep).
--
-- Inside == a separate Step constructor, outside == part of Stream

import Criterion.Main
import System.Random.MWC
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (runReaderT)
import qualified Control.Monad.Trans.State.Lazy as Lazy (evalStateT)
import qualified Control.Monad.Trans.State.Strict as Strict (evalStateT)

-- Inside
data StepI m s a
    = DoneI
    | SkipI !s
    | YieldI a !s
    | StepM (m s)
data StreamI m a = forall s. StreamI (s -> StepI m s a) !s

staticIntsI :: Monad m => StreamI m Int
staticIntsI =
    StreamI go 10000
  where
    go 0 = DoneI
    go i = YieldI i (i - 1)
{-# INLINE staticIntsI #-}

randomIntsI :: MonadIO m => GenIO -> StreamI m Int
randomIntsI gen =
    StreamI go (Left 10000)
  where
    go (Left 0) = DoneI
    go (Left i) = StepM $ do
        x <- liftIO $ uniformR (1, 10) gen
        return $ Right (x, i :: Int)
    go (Right (x, i)) = YieldI x (Left $ i - 1)
{-# INLINE randomIntsI #-}

sumI :: Monad m => StreamI m Int -> m Int
sumI (StreamI step s0) =
    loop 0 s0
  where
    loop !t s =
        case step s of
            DoneI -> return t
            SkipI s' -> loop t s'
            YieldI a s' -> loop (t + a) s'
            StepM ms -> ms >>= loop t
{-# INLINE sumI #-}

-------------------------------------------------------

-- Outside
data StepO s a
    = DoneO
    | SkipO !s
    | YieldO a !s
data StreamO m a = forall s. StreamO (s -> m (StepO s a)) !s

staticIntsO :: Monad m => StreamO m Int
staticIntsO =
    StreamO go (10000 :: Int)
  where
    go 0 = return DoneO
    go i = return (YieldO i (i - 1))
{-# INLINE staticIntsO #-}

randomIntsO :: MonadIO m => GenIO -> StreamO m Int
randomIntsO gen =
    StreamO go 10000
  where
    go 0 = return DoneO
    go i = do
        x <- liftIO $ uniformR (1, 10) gen
        return $ YieldO x (i - 1)
{-# INLINE randomIntsO #-}

sumO :: Monad m => StreamO m Int -> m Int
sumO (StreamO step s0) =
    loop 0 s0
  where
    loop !t s = do
        res <- step s
        case res of
            DoneO -> return t
            SkipO s' -> loop t s'
            YieldO a s' -> loop (t + a) s'
{-# INLINE sumO #-}
-------------------------------------------------------
main :: IO ()
main = do
    gen <- createSystemRandom
    defaultMain
        [ groupHelper "static" staticIntsI staticIntsO
        , groupHelper "random" (randomIntsI gen) (randomIntsO gen)
        ]

groupHelper :: String
            -> (forall m. MonadIO m => StreamI m Int)
            -> (forall m. MonadIO m => StreamO m Int)
            -> Benchmark
groupHelper name srcI srcO = bgroup name
    [ transHelper "IO" srcI srcO id
    , transHelper "ReaderT" srcI srcO (flip runReaderT ())
    , transHelper "strict StateT" srcI srcO (flip Strict.evalStateT ())
    , transHelper "lazy StateT" srcI srcO (flip Lazy.evalStateT ())
    ]

transHelper :: Monad m
            => String
            -> StreamI m Int
            -> StreamO m Int
            -> (m Int -> IO Int)
            -> Benchmark
transHelper name srcI srcO run = bgroup name
    [ bench "inside"  $ whnfIO $ run $ sumI srcI
    , bench "outside" $ whnfIO $ run $ sumO srcO
    ]
