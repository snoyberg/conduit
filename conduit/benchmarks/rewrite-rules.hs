import Data.IORef
import Control.Monad (foldM, replicateM_)
import Criterion.Main
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Conduit.Rewrite
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V
import qualified Data.Conduit.Fusion as CF

-- really bad random number generator
type Gen = IORef Int

newGen :: IO Gen
newGen = newIORef 0

rand :: Gen -> IO Int
rand = flip atomicModifyIORef $ \i -> let j = succ i in j `seq` (j, j)

test :: Int -> Bool
test = (== 0) . (`mod` 10)

lowLevel :: Int -> IO Int
lowLevel cnt0 = do
    gen <- newGen
    let loop successes 0 = return successes
        loop successes cnt = do
            i <- rand gen
            let successes'
                    | test i = successes + 1
                    | otherwise = successes
            successes' `seq` loop successes' (cnt - 1)
    loop 0 cnt0

list :: Int -> IO Int
list cnt0 = do
    gen <- newGen
    let go total _ = do
            i <- rand gen
            let total'
                    | test i = total + 1
                    | otherwise = total
            return $! total'
    foldM go 0 [1..cnt0]

vector :: Int -> IO Int
vector cnt0 = do
    gen <- newGen
    let go total _ = do
            i <- rand gen
            let total'
                    | test i = total + 1
                    | otherwise = total
            return $! total'
    V.foldM' go 0 $ V.enumFromTo 1 cnt0

vectorM :: Int -> IO Int
vectorM cnt0 = do
    gen <- newGen
    v <- V.replicateM cnt0 (rand gen)
    let go total i
            | test i = total + 1
            | otherwise = total
    return $ V.foldl' go 0 v

conduit1 :: Int -> IO Int
conduit1 cnt0 = do
    gen <- newGen
    let src = replicateM_ cnt0 (lift (rand gen) >>= yield)
        sink = CL.fold go 0
        go total i
            | test i = total + 1
            | otherwise = total
    src $$ sink

conduit2 :: Int -> IO Int
conduit2 cnt0 = do
    gen <- newGen
    let src = CL.replicateM_ cnt0 (rand gen)
        sink = CL.fold go 0
        go total i
            | test i = total + 1
            | otherwise = total
    src $$ sink

conduitFoldM :: Int -> IO Int
conduitFoldM cnt0 = do
    gen <- newGen
    let src = CL.enumFromTo 1 cnt0
        sink = CL.foldM go 0
        go total _ = do
            i <- rand gen
            return $
                if test i
                    then total + 1
                    else total
    src $$ sink

conduitFusionFoldM :: Int -> IO Int
conduitFusionFoldM cnt0 = do
    gen <- newGen
    let src = CF.enumFromTo 1 cnt0
        sink = CF.foldM go 0
        go total _ = do
            i <- rand gen
            return $
                if test i
                    then total + 1
                    else total
    CF.connect src sink

conduitFusionReplicate :: Int -> IO Int
conduitFusionReplicate cnt0 = do
    gen <- newGen
    let src = CF.replicateM_ cnt0 (rand gen)
        sink = CF.fold go 0
        go total i
            | test i = total + 1
            | otherwise = total
    CF.connect src sink

conduitIR :: Int -> IO Int
conduitIR cnt0 = initReplicate
    newGen
    rand
    cnt0 $$
    (foldRewrite go 0)
  where
    go total i
        | test i = total + 1
        | otherwise = total

conduitIRM :: Int -> IO Int
conduitIRM cnt0 = initReplicate
    newGen
    rand
    cnt0 $$
    (foldMRewrite go (return 0))
  where
    go total i
        | test i = return $! total + 1
        | otherwise = return total

conduitIRC :: Int -> IO Int
conduitIRC cnt0 = initReplicateConnect
    newGen
    rand
    cnt0
    (foldRewrite go 0)
  where
    go total i
        | test i = total + 1
        | otherwise = total

conduitIRF :: Int -> IO Int
conduitIRF cnt0 = initReplicateFold
    newGen
    rand
    cnt0
    go
    0
  where
    go total i
        | test i = total + 1
        | otherwise = total

data GenCnt = GenCnt {-# UNPACK #-} !Gen {-# UNPACK #-} !Int

conduitUnfold :: Int -> IO Int
conduitUnfold cnt0 =
    unfoldM
        (newGen >>= \gen -> return $! GenCnt gen cnt0)
        goSrc
        $$ foldMRewrite goSink (return 0)
  where
    goSrc (GenCnt _ 0) onDone _ = onDone
    goSrc (GenCnt gen cnt) _ onCont = do
        a <- rand gen
        let cnt' = cnt - 1
        cnt' `seq` onCont a (GenCnt gen cnt')
    goSink total i
        | test i = return $! total + 1
        | otherwise = return total

main :: IO ()
main = do
    lowLevel count >>= print
    list count >>= print
    vector count >>= print
    vectorM count >>= print
    conduit1 count >>= print
    conduit2 count >>= print
    conduitFoldM count >>= print
    conduitFusionFoldM count >>= print
    conduitFusionReplicate count >>= print
    conduitIR count >>= print
    conduitIRM count >>= print
    conduitIRC count >>= print
    conduitIRF count >>= print
    conduitUnfold count >>= print
    defaultMain $ reverse
        [ bench "lowLevel" $ whnfIO $ lowLevel count
        , bench "list" $ whnfIO $ list count
        , bench "vector" $ whnfIO $ vector count
        --, bench "vectorM" $ whnfIO $ vectorM count
        , bench "conduit1" $ whnfIO $ conduit1 count
        , bench "conduit2" $ whnfIO $ conduit2 count
        , bench "conduitFoldM" $ whnfIO $ conduitFoldM count
        , bench "conduitFusionFoldM" $ whnfIO $ conduitFusionFoldM count
        , bench "conduitFusionReplicate" $ whnfIO $ conduitFusionReplicate count
        , bench "conduitIR" $ whnfIO $ conduitIR count
        , bench "conduitIRM" $ whnfIO $ conduitIRM count
        , bench "conduitIRC" $ whnfIO $ conduitIRC count
        , bench "conduitIRF" $ whnfIO $ conduitIRF count
        , bench "conduitUnfold" $ whnfIO $ conduitUnfold count
        ]
  where
    count = 100000
