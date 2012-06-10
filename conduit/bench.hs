{-# LANGUAGE PackageImports #-}
import Criterion.Main
import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.ST (runST)
import Control.Monad (foldM)
import Data.List (foldl')
import "pipes" Control.Pipe
import Data.Void
import Control.Applicative
import Data.Maybe
import qualified "pipes-core" Control.Pipe as PC
import qualified "pipes-core" Control.Pipe.Combinators as PCC
import qualified "conduitfour" Data.Conduit as C4
import qualified "conduitfour" Data.Conduit.List as CL4
import qualified "conduitfour" Data.Conduit.Binary as CB4
import qualified Data.ByteString as S
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import qualified System.IO as SIO
import qualified "pipes-core" Control.Pipe.Exception as PCE

main :: IO ()
main = defaultMain
    [ bgroup "bigsum"
        [ bench "conduit5" $ whnf (\i -> (runIdentity $ CL.sourceList [1..1000 :: Int] C.$$ CL.fold (+) i)) 0
        , bench "conduit5->+>" $ whnf (\i -> (runIdentity $ C.runPipe $ CL.sourceList [1..1000 :: Int] C.>+> CL.fold (+) i)) 0
        , bench "conduit5-resume" $ flip whnf 0 $ \i -> runIdentity $ do
            (rsrc, res) <- CL.sourceList [1..1000 :: Int] C.$$+ CL.fold (+) i
            rsrc C.$$+- return ()
        , bench "conduit4" $ whnf (\i -> (runIdentity $ CL4.sourceList [1..1000 :: Int] C4.$$ CL4.fold (+) i)) 0
        , bench "pipes" $ whnf pipeTestSum 0
        , bench "pipes-core" $ whnf pipeCoreTest 0
        , bench "pipes-core-seq" $ whnf pipeCoreTestSeq 0
        , bench "pure" $ whnf (\i -> foldl' (+) i [1..1000 :: Int]) 0
        ]
    , bgroup "bytecount"
        [ bench "conduit5" (whnfIO $ C.runResourceT $ CB.sourceFile "foo" C.$$ CL.fold (\x bs -> x + S.length bs) 0)
        , bench "conduit5->+>" (whnfIO $ C.runResourceT $ C.runPipe $ CB.sourceFile "foo" C.>+> CL.fold (\x bs -> x + S.length bs) 0)
        , bench "conduit5-resume" $ whnfIO $ C.runResourceT $ do
            (rsrc, res) <-  CB.sourceFile "foo" C.$$+ CL.fold (\x bs -> x + S.length bs) 0
            rsrc C.$$+- return ()
            return res
        , bench "conduit4" (whnfIO $ C4.runResourceT $ CB4.sourceFile "foo" C4.$$ CL4.fold (\x bs -> x + S.length bs) 0)
        , bench "pipes" (whnfIO pipeTestCount)
        , bench "pipes-resource" (whnfIO pipeTestResource)
        , bench "pipes-core" (whnfIO pipesCoreTest)
        , bench "pipes-core-resource" (whnfIO pipesCoreTestResource)
        ]
    ]

pipeFold :: Monad m => (b -> a -> b) -> b -> Frame a o m b
pipeFold f =
    Frame . go
  where
    go accum = do
        mx <- await
        case mx of
            Nothing -> return $ return accum
            Just x ->
                let accum' = f accum x
                 in accum' `seq` go accum

pipeSourceList :: Monad m => [a] -> Frame i a m ()
pipeSourceList list = Frame $ do
    mapM_ yieldF list
    close $ return ()

pipeTestSum :: Int -> Int
pipeTestSum start = fromMaybe (-1) $ runIdentity $ runFrame $ (Just <$> pipeFold (+) start) <-< (Nothing <$ pipeSourceList [1..1000 :: Int])

pipesCoreFold :: Monad m => (b -> a -> b) -> b -> PC.Pipe a x m b
pipesCoreFold f = go
  where
    go x = PCC.tryAwait >>= maybe (return x) (let y = f x in y `seq` go . y)

pipeCoreTest :: Int -> Int
pipeCoreTest start = fromMaybe (-1) $ runIdentity $ PC.runPurePipe_ $ PCC.fromList [1..1000 :: Int] PCC.$$ PCC.fold (+) start

pipeCoreTestSeq :: Int -> Int
pipeCoreTestSeq start = fromMaybe (-1) $ runIdentity $ PC.runPurePipe_ $ PCC.fromList [1..1000 :: Int] PCC.$$ pipesCoreFold (+) start

sourceFilePipes :: FilePath -> Frame i S.ByteString IO Int
sourceFilePipes file = Frame $ close $ do
    h <- lift $ SIO.openBinaryFile file SIO.ReadMode
    finallyP (SIO.hClose h) (pull h)
  where
    pull h = do
        bs <- lift $ S.hGetSome h 4096
        if S.null bs
            then return 0
            else do
                yieldF bs
                pull h

sourceFilePipesResource :: FilePath -> Frame i S.ByteString (ResourceT IO) Int
sourceFilePipesResource file = Frame $ close $ do
    (key, h) <- lift $ allocate (SIO.openBinaryFile file SIO.ReadMode) SIO.hClose
    finallyP (release key) (pull h)
  where
    pull h = do
        bs <- lift $ lift $ S.hGetSome h 4096
        if S.null bs
            then return (-2)
            else do
                yieldF bs
                pull h

pipeTestCount = runFrame $ pipeFold (\x bs -> x + S.length bs) 0 <-< (sourceFilePipes "foo")
pipeTestResource = runResourceT $ runFrame $ pipeFold (\x bs -> x + S.length bs) 0 <-< (sourceFilePipesResource "foo")

sourceFilePipesCore :: FilePath -> PC.Pipe i S.ByteString IO Int
sourceFilePipesCore file =
    PCE.bracket
        (SIO.openBinaryFile file SIO.ReadMode)
        SIO.hClose
        pull
  where
    pull h = do
        bs <- lift $ S.hGetSome h 4096
        if S.null bs
            then return 0
            else do
                PC.yield bs
                pull h

sourceFilePipesCoreResource :: FilePath -> PC.Pipe i S.ByteString (ResourceT IO) Int
sourceFilePipesCoreResource file =
    PCE.bracket
        (allocate (SIO.openBinaryFile file SIO.ReadMode) SIO.hClose)
        (release . fst)
        (pull . snd)
  where
    pull h = do
        bs <- lift $ lift $ S.hGetSome h 4096
        if S.null bs
            then return 0
            else do
                PC.yield bs
                pull h

pipesCoreTest = PC.runPipe $ sourceFilePipesCore "foo" PCC.$$ PCC.fold (\x bs -> x + S.length bs) 0
pipesCoreTestResource = runResourceT $ PC.runPurePipe_ $ sourceFilePipesCoreResource "foo" PCC.$$ PCC.fold (\x bs -> x + S.length bs) 0
