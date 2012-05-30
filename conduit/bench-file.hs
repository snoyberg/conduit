{-# LANGUAGE PackageImports #-}
import Criterion.Main
import qualified Data.ByteString as S
import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.ST (runST)
import Control.Monad (foldM)
import Data.List (foldl')
import "pipes" Control.Pipe
import qualified "pipes-core" Control.Pipe as PC
import qualified "pipes-core" Control.Pipe.Combinators as PCC
import qualified "pipes-core" Control.Pipe.Exception as PCE
import Data.Void
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource
import Data.Maybe
import qualified System.IO as SIO

main :: IO ()
main = do
    C.runResourceT (CB.sourceFile "foo" C.$$ CL.fold (\x bs -> x + S.length bs) 0) >>= print
    pipeTest >>= print
    pipeTestResource >>= print
    pipesCoreTest >>= print
    pipesCoreTestResource >>= print
    defaultMain
        [ bench "conduit-bytecount" (whnfIO $ C.runResourceT $ CB.sourceFile "foo" C.$$ CL.fold (\x bs -> x + S.length bs) 0)
        , bench "conduit-spipe-bytecount" (whnfIO $ C.runResourceT $ sourceFileSPipe "foo" C.$$ CL.fold (\x bs -> x + S.length bs) 0)
        , bench "pipes-bytecount" (whnfIO pipeTest)
        , bench "pipes-bytecount-resource" (whnfIO pipeTestResource)
        , bench "pipes-core-bytecount" (whnfIO pipesCoreTest)
        , bench "pipes-core-bytecount-resource" (whnfIO pipesCoreTestResource)
        ]

sourceFileSPipe fp =
    CI.bracketSPipe
        (SIO.openFile fp SIO.ReadMode)
        SIO.hClose
        loop
  where
    loop h = do
        bs <- liftIO $ S.hGetSome h 4096
        if S.null bs
            then return ()
            else CI.yield bs >> loop h

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

pipeFold :: (Show b, MonadIO m) => (b -> a -> b) -> b -> Frame a o m b
pipeFold f =
    Frame . go
  where
    go accum = do
        mx <- await
        case mx of
            Nothing -> return $ return accum
            Just x ->
                let accum' = f accum x
                 in accum' `seq` go accum'

pipeTest = runFrame $ pipeFold (\x bs -> x + S.length bs) 0 <-< (sourceFilePipes "foo")
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
