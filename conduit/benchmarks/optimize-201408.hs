{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE RankNTypes                #-}
-- Collection of three benchmarks: a simple integral sum, monte carlo analysis,
-- and sliding vector.
import           Control.DeepSeq
import           Control.Monad               (foldM)
import           Control.Monad               (when, liftM)
import           Control.Monad.Codensity     (lowerCodensity)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Criterion.Main
import           Data.Conduit
import           Data.Conduit.Internal       (ConduitM (..), Pipe (..))
import qualified Data.Conduit.Internal       as CI
import qualified Data.Conduit.List           as CL
import qualified Data.Foldable               as F
import           Data.Functor.Identity       (runIdentity)
import           Data.IORef
import           Data.List                   (foldl')
import           Data.Monoid                 (mempty)
import qualified Data.Sequence               as Seq
import qualified Data.Vector                 as VB
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed         as VU
import           System.Environment          (withArgs)
import qualified System.Random.MWC           as MWC
import           Test.Hspec

data TestBench = TBGroup String [TestBench]
               | TBBench Benchmark
               | forall a b. (Eq b, Show b) => TBPure String a b (a -> b)
               | forall a. (Eq a, Show a) => TBIO String a (IO a)
               | forall a. (Eq a, Show a) => TBIOTest String (a -> IO ()) (IO a)
               | forall a. (Eq a, Show a) => TBIOBench String a (IO a) (IO ())

toSpec :: TestBench -> Spec
toSpec (TBGroup name tbs) = describe name $ mapM_ toSpec tbs
toSpec (TBBench _) = return ()
toSpec (TBPure name a b f) = it name $ f a `shouldBe` b
toSpec (TBIO name a f) = it name $ f >>= (`shouldBe` a)
toSpec (TBIOTest name spec f) = it name $ f >>= spec
toSpec (TBIOBench name a f _) = it name $ f >>= (`shouldBe` a)

toBench :: TestBench -> Benchmark
toBench (TBGroup name tbs) = bgroup name $ map toBench tbs
toBench (TBBench b) = b
toBench (TBPure name a _ f) = bench name $ whnf f a
toBench (TBIO name _ f) = bench name $ whnfIO f
toBench (TBIOTest name _ f) = bench name $ whnfIO f
toBench (TBIOBench name _ _ f) = bench name $ whnfIO f

runTestBench :: [TestBench] -> IO ()
runTestBench tbs = do
    withArgs [] $ hspec $ mapM_ toSpec tbs
    defaultMain $ map toBench tbs

main :: IO ()
main = runTestBench =<< sequence
    [ sumTB
    , mapSumTB
    , monteCarloTB
    , fmap (TBGroup "sliding window") $ sequence
        [ slidingWindow 10
        , slidingWindow 30
        , slidingWindow 100
        , slidingWindow 1000
        ]
    ]

-----------------------------------------------------------------------

sumTB :: IO TestBench
sumTB = do
    upperRef <- newIORef upper0
    return $ TBGroup "sum"
        [ TBPure "Data.List.foldl'" upper0 expected
            $ \upper -> foldl' (+) 0 [1..upper]
        , TBIO "Control.Monad.foldM" expected $ do
            upper <- readIORef upperRef
            foldM plusM 0 [1..upper]
        , TBPure "low level" upper0 expected $ \upper ->
            let go x !t
                    | x > upper = t
                    | otherwise = go (x + 1) (t + x)
             in go 1 0
        , TBIO "boxed vectors, I/O" expected $ do
            upper <- readIORef upperRef
            VB.foldM' plusM 0 $ VB.enumFromTo 1 upper
        , TBPure "boxed vectors" upper0 expected
            $ \upper -> VB.foldl' (+) 0 (VB.enumFromTo 1 upper)
        , TBPure "unboxed vectors" upper0 expected
            $ \upper -> VU.foldl' (+) 0 (VU.enumFromTo 1 upper)
        , TBPure "conduit, pure, fold" upper0 expected
            $ \upper -> runIdentity $ CL.enumFromTo 1 upper $$ CL.fold (+) 0
        , TBPure "conduit, pure, foldM" upper0 expected
            $ \upper -> runIdentity $ CL.enumFromTo 1 upper $$ CL.foldM plusM 0
        , TBIO "conduit, IO, fold" expected $ do
            upper <- readIORef upperRef
            CL.enumFromTo 1 upper $$ CL.fold (+) 0
        , TBIO "conduit, IO, foldM" expected $ do
            upper <- readIORef upperRef
            CL.enumFromTo 1 upper $$ CL.foldM plusM 0
        ]
  where
    upper0 = 10000 :: Int
    expected = sum [1..upper0]

    plusM x y = return $! x + y

-----------------------------------------------------------------------

mapSumTB :: IO TestBench
mapSumTB = return $ TBGroup "map + sum"
    [ TBPure "boxed vectors" upper0 expected
        $ \upper -> VB.foldl' (+) 0
                  $ VB.map (+ 1)
                  $ VB.map (* 2)
                  $ VB.enumFromTo 1 upper
    , TBPure "unboxed vectors" upper0 expected
        $ \upper -> VU.foldl' (+) 0
                  $ VU.map (+ 1)
                  $ VU.map (* 2)
                  $ VU.enumFromTo 1 upper
    , TBPure "conduit, connect1" upper0 expected $ \upper -> runIdentity
        $ CL.enumFromTo 1 upper
       $$ CL.map (* 2)
      =$= CL.map (+ 1)
      =$= CL.fold (+) 0
    , TBPure "conduit, connect2" upper0 expected $ \upper -> runIdentity
        $ CL.enumFromTo 1 upper
      =$= CL.map (* 2)
       $$ CL.map (+ 1)
      =$= CL.fold (+) 0
    , TBPure "conduit, connect3" upper0 expected $ \upper -> runIdentity
        $ CL.enumFromTo 1 upper
      =$= CL.map (* 2)
      =$= CL.map (+ 1)
       $$ CL.fold (+) 0
    , TBPure "conduit, inner fuse" upper0 expected $ \upper -> runIdentity
        $ CL.enumFromTo 1 upper
      =$= (CL.map (* 2)
      =$= CL.map (+ 1))
       $$ CL.fold (+) 0
    ]
  where
    upper0 = 10000 :: Int
    expected = sum $ map (+ 1) $ map (* 2) [1..upper0]

-----------------------------------------------------------------------

monteCarloTB :: IO TestBench
monteCarloTB = return $ TBGroup "monte carlo"
    [ TBIOTest "conduit" closeEnough $ do
        gen <- MWC.createSystemRandom
        successes <- CL.replicateM count (MWC.uniform gen)
                  $$ CL.fold (\t (x, y) ->
                                if (x*x + y*(y :: Double) < 1)
                                    then t + 1
                                    else t)
                        (0 :: Int)
        return $ fromIntegral successes / fromIntegral count * 4
    , TBIOTest "low level" closeEnough $ do
        gen <- MWC.createSystemRandom
        let go :: Int -> Int -> IO Double
            go 0 !t = return $! fromIntegral t / fromIntegral count * 4
            go i !t = do
                (x, y) <- MWC.uniform gen
                let t'
                        | x*x + y*(y :: Double) < 1 = t + 1
                        | otherwise = t
                go (i - 1) t'
        go count (0 :: Int)
    ]
  where
    count = 100000 :: Int

    closeEnough x
        | abs (x - 3.14159 :: Double) < 0.2 = return ()
        | otherwise = error $ "Monte carlo analysis too inaccurate: " ++ show x

-----------------------------------------------------------------------

slidingWindow :: Int -> IO TestBench
slidingWindow window = do
    upperRef <- newIORef upper0
    return $ TBGroup (show window)
        [ TBIOBench "low level, Seq" expected
            (swLowLevelSeq window upperRef id (\x y -> x . (F.toList y:)) ($ []))
            (swLowLevelSeq window upperRef () (\() y -> rnf y) id)
        , TBIOBench "conduit, Seq" expected
            (swConduitSeq window upperRef id (\x y -> x . (F.toList y:)) ($ []))
            (swConduitSeq window upperRef () (\() y -> rnf y) id)
        {- https://ghc.haskell.org/trac/ghc/ticket/9446
        , TBIOBench "low level, boxed Vector" expected
            (swLowLevelVector window upperRef id (\x y -> x . (VB.toList y:)) ($ []))
            (swLowLevelVector window upperRef () (\() y -> rnf (y :: VB.Vector Int)) id)
            -}
        , TBBench $ bench "low level, boxed Vector" $ whnfIO $
            swLowLevelVector window upperRef () (\() y -> rnf (y :: VB.Vector Int)) id

        {- https://ghc.haskell.org/trac/ghc/ticket/9446
        , TBIOBench "conduit, boxed Vector" expected
            (swConduitVector window upperRef id (\x y -> x . (VB.toList y:)) ($ []))
            (swConduitVector window upperRef () (\() y -> rnf (y :: VB.Vector Int)) id)
        -}

        , TBBench $ bench "conduit, boxed Vector" $ whnfIO $
            swConduitVector window upperRef () (\() y -> rnf (y :: VB.Vector Int)) id


        , TBIOBench "low level, unboxed Vector" expected
            (swLowLevelVector window upperRef id (\x y -> x . (VU.toList y:)) ($ []))
            (swLowLevelVector window upperRef () (\() y -> rnf (y :: VU.Vector Int)) id)
        , TBIOBench "conduit, unboxed Vector" expected
            (swConduitVector window upperRef id (\x y -> x . (VU.toList y:)) ($ []))
            (swConduitVector window upperRef () (\() y -> rnf (y :: VU.Vector Int)) id)
        ]
  where
    upper0 = 10000
    expected =
        loop [1..upper0]
      where
        loop input
            | length x == window = x : loop y
            | otherwise = []
          where
            x = take window input
            y = drop 1 input

swLowLevelSeq :: Int -> IORef Int -> t -> (t -> Seq.Seq Int -> t) -> (t -> t') -> IO t'
swLowLevelSeq window upperRef t0 f final = do
    upper <- readIORef upperRef

    let phase1 i !s
            | i > window = phase2 i s t0
            | otherwise = phase1 (i + 1) (s Seq.|> i)

        phase2 i !s !t
            | i > upper = t'
            | otherwise = phase2 (i + 1) s' t'
          where
            t' = f t s
            s' = Seq.drop 1 s Seq.|> i

    return $! final $! phase1 1 mempty

swLowLevelVector :: V.Vector v Int
                 => Int
                 -> IORef Int
                 -> t
                 -> (t -> v Int -> t)
                 -> (t -> t')
                 -> IO t'
swLowLevelVector window upperRef t0 f final = do
    upper <- readIORef upperRef

    let go !i !t _ _ _ | i > upper = return $! final $! t
        go !i !t !end _mv mv2 | end == bufSz  = newBuf >>= go i t sz mv2
        go !i !t !end mv mv2 = do
            VM.unsafeWrite mv end i
            when (end > sz) $ VM.unsafeWrite mv2 (end - sz) i
            let end' = end + 1
            t' <-
                if end' < sz
                    then return t
                    else do
                        v <- V.unsafeFreeze $ VM.unsafeSlice (end' - sz) sz mv
                        return $! f t v
            go (i + 1) t' end' mv mv2

    mv <- newBuf
    mv2 <- newBuf
    go 1 t0 0 mv mv2
  where
    sz = window
    bufSz = 2 * window
    newBuf = VM.new bufSz

swConduitSeq :: Int
             -> IORef Int
             -> t
             -> (t -> Seq.Seq Int -> t)
             -> (t -> t')
             -> IO t'
swConduitSeq window upperRef t0 f final = do
    upper <- readIORef upperRef

    t <- CL.enumFromTo 1 upper
        $= slidingWindowC window
        $$ CL.fold f t0
    return $! final t

swConduitVector :: V.Vector v Int
                => Int
                -> IORef Int
                -> t
                -> (t -> v Int -> t)
                -> (t -> t')
                -> IO t'
swConduitVector window upperRef t0 f final = do
    upper <- readIORef upperRef

    t <- CL.enumFromTo 1 upper
        $= slidingVectorC window
        $$ CL.fold f t0
    return $! final t

slidingWindowC :: Monad m => Int -> Conduit a m (Seq.Seq a)
slidingWindowC = slidingWindowCC
{-# INLINE [0] slidingWindowC #-}
{-# RULES "unstream slidingWindowC"
    forall i. slidingWindowC i = CI.unstream (CI.streamConduit (slidingWindowCC i) (slidingWindowS i))
  #-}

slidingWindowCC :: Monad m => Int -> Conduit a m (Seq.Seq a)
slidingWindowCC sz =
    go sz mempty
  where
    goContinue st = await >>=
                    maybe (return ())
                          (\x -> do
                             let st' = st Seq.|> x
                             yield st' >> goContinue (Seq.drop 1 st')
                          )
    go 0 st = yield st >> goContinue (Seq.drop 1 st)
    go !n st = CL.head >>= \m ->
               case m of
                 Nothing | n < sz -> yield st
                         | otherwise -> return ()
                 Just x -> go (n-1) (st Seq.|> x)
{-# INLINE slidingWindowCC #-}

slidingWindowS :: Monad m => Int -> CI.Stream m a () -> CI.Stream m (Seq.Seq a) ()
slidingWindowS sz (CI.Stream step ms0) =
    CI.Stream step' $ liftM (\s -> Left (s, sz, mempty)) ms0
  where
    step' (Left (s, 0, st)) = return $ CI.Emit (Right (s, st)) st
    step' (Left (s, i, st)) = do
        res <- step s
        return $ case res of
            CI.Stop () -> CI.Stop ()
            CI.Skip s' -> CI.Skip $ Left (s', i, st)
            CI.Emit s' a -> CI.Skip $ Left (s', i - 1, st Seq.|> a)
    step' (Right (s, st)) = do
        res <- step s
        return $ case res of
            CI.Stop () -> CI.Stop ()
            CI.Skip s' -> CI.Skip $ Right (s', st)
            CI.Emit s' a ->
                let st' = Seq.drop 1 st Seq.|> a
                 in CI.Emit (Right (s', st')) st'
{-# INLINE slidingWindowS #-}

slidingVectorC :: V.Vector v a => Int -> Conduit a IO (v a)
slidingVectorC = slidingVectorCC
{-# INLINE [0] slidingVectorC #-}
{-# RULES "unstream slidingVectorC"
    forall i. slidingVectorC i = CI.unstream (CI.streamConduit (slidingVectorCC i) (slidingVectorS i))
  #-}

slidingVectorCC :: V.Vector v a => Int -> Conduit a IO (v a)
slidingVectorCC sz = do
    mv <- newBuf
    mv2 <- newBuf
    go 0 mv mv2
  where
    bufSz = 2 * sz
    newBuf = liftIO (VM.new bufSz)

    go !end _mv mv2 | end == bufSz  = newBuf >>= go sz mv2
    go !end mv mv2 = do
      mx <- await
      case mx of
        Nothing -> when (end > 0 && end < sz) $ do
          v <- liftIO $ V.unsafeFreeze $ VM.take end mv
          yield v
        Just x -> do
          liftIO $ do
            VM.unsafeWrite mv end x
            when (end > sz) $ VM.unsafeWrite mv2 (end - sz) x
          let end' = end + 1
          when (end' >= sz) $ do
            v <- liftIO $ V.unsafeFreeze $ VM.unsafeSlice (end' - sz) sz mv
            yield v
          go end' mv mv2

slidingVectorS :: V.Vector v a => Int -> CI.Stream IO a () -> CI.Stream IO (v a) ()
slidingVectorS sz (CI.Stream step ms0) =
    CI.Stream step' ms1
  where
    bufSz = 2 * sz
    newBuf = liftIO (VM.new bufSz)

    ms1 = do
        s <- ms0
        mv <- newBuf
        mv2 <- newBuf
        return (s, 0, mv, mv2)

    step' (_, -1, _, _) = return $ CI.Stop ()
    step' (s, end, _mv, mv2) | end == bufSz = do
        mv3 <- newBuf
        return $ CI.Skip (s, sz, mv2, mv3)
    step' (s, end, mv, mv2) = do
        res <- step s
        case res of
            CI.Stop ()
                | end > 0 && end < sz -> do
                    v <- liftIO $ V.unsafeFreeze $ VM.take end mv
                    return $ CI.Emit (s, -1, mv, mv2) v
                | otherwise -> return $ CI.Stop ()
            CI.Skip s' -> return $ CI.Skip (s', end, mv, mv2)
            CI.Emit s' x -> liftIO $ do
                VM.unsafeWrite mv end x
                when (end > sz) $ VM.unsafeWrite mv2 (end - sz) x
                let end' = end + 1
                    state = (s', end', mv, mv2)
                if end' >= sz
                    then do
                        v <- V.unsafeFreeze $ VM.unsafeSlice (end' - sz) sz mv
                        return $ CI.Emit state v
                    else return $ CI.Skip state
{-# INLINE slidingVectorS #-}
