{-# LANGUAGE BangPatterns #-}
import qualified Data.StreamConduit as SC
import qualified Data.List as DL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Criterion.Main
import Data.Functor.Identity (runIdentity)
import Data.IORef
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C

upper0 :: Int
upper0 = 10000

upper1 :: Int
upper1 = 100

main :: IO ()
main = do
    upperRef <- newIORef upper0
    defaultMain
        [ bgroup "enumFromTo/map/sum"
            [ bench "StreamConduit pure" $ flip whnf upper0 $ \upper ->
                runIdentity
                    $ SC.runConduit
                    $ SC.enumFromTo 1 upper
               SC.=$= SC.map (+ 1)
               SC.=$= SC.foldl' (+) 0
            , bench "StreamConduit I/O" $ whnfIO $ do
                upper <- readIORef upperRef
                SC.runConduit
                            $ SC.enumFromTo 1 upper
                       SC.=$= SC.map (+ 1)
                       SC.=$= SC.foldl' (+) 0
            , bench "StreamConduit full I/O" $ whnfIO $ do
                upper <- readIORef upperRef
                SC.runConduit
                            $ SC.enumFromTo 1 upper
                       SC.=$= SC.mapM (return . (+ 1))
                       SC.=$= SC.foldM' (\x y -> return $! x + y) 0
            , bench "list" $ flip whnf upper0 $ \upper ->
                DL.foldl' (+) 0 $ map (+ 1) $ enumFromTo 1 upper
            , bench "Vector, boxed, pure" $ flip whnf upper0 $ \upper ->
                V.foldl' (+) 0 $ V.map (+ 1) $ V.enumFromTo 1 upper
            , bench "Vector, boxed, full I/O" $ whnfIO $ do
                upper <- readIORef upperRef
                return (V.enumFromTo 1 upper)
                    >>= V.mapM (return . (+ 1))
                    >>= V.foldM' (\x y -> return $! x + y) 0
            , bench "Vector, unboxed, pure" $ flip whnf upper0 $ \upper ->
                VU.foldl' (+) 0 $ VU.map (+ 1) $ VU.enumFromTo 1 upper
            , bench "Vector, unboxed, full I/O" $ whnfIO $ do
                upper <- readIORef upperRef
                return (VU.enumFromTo 1 upper)
                    >>= VU.mapM (return . (+ 1))
                    >>= VU.foldM' (\x y -> return $! x + y) 0
            , bench "low level" $ flip whnf upper0 $ \upper ->
                let go !t 10001 = t
                    go !t i = go (t + i) (succ i)
                 in go 0 1
            , bench "conduit, pure" $ flip whnf upper0 $ \upper ->
                runIdentity $ C.enumFromTo 1 upper
                         C.$$ C.map (+ 1)
                         C.=$ C.fold (+) 0
            ]
        , bgroup "monadic composition"
            [ bench "StreamConduit" $ flip whnf upper1 $ \upper ->
                let src = mapM_ (\u -> SC.enumFromTo 1 u) [1..upper]
                 in runIdentity $ SC.runConduit
                                $ src SC.=$= SC.map (+ 1) SC.=$= SC.foldl' (+) 0
            , bench "conduit" $ flip whnf upper1 $ \upper ->
                let src = mapM_ (\u -> C.enumFromTo 1 u) [1..upper]
                 in runIdentity $ src C.$$ C.map (+ 1) C.=$ C.fold (+) 0
            ]
        , bgroup "lots of yield and await"
            [
            ]
        ]
