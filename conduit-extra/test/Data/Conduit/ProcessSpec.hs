module Data.Conduit.ProcessSpec (spec, main) where

import Test.Hspec
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Data.List (sort, isSuffixOf)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent.Async (concurrently)
import System.Process (shell)
import qualified Data.ByteString.Lazy as L
import System.Exit
import Control.Concurrent.STM

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Data.Conduit.Process" $ do
    -- FIXME only run on Unix
    it "cat" $ do
        lbs <- L.readFile "README.md"
        ((sink, closeStdin), source, (), ecVar) <- conduitProcess (shell "cat")
        ((), bss) <- concurrently
            (do
                mapM_ yield (L.toChunks lbs) $$ sink
                closeStdin)
            (source $$ CL.consume)
        L.fromChunks bss `shouldBe` lbs
        ec <- atomically $ readTMVar ecVar
        ec `shouldBe` ExitSuccess
            
    return ()