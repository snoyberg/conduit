module Data.Conduit.Process.TypedSpec (spec) where

import Test.Hspec
import Data.Conduit
import Data.Conduit.Process.Typed
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B

spec :: Spec
spec = do
  it "cat works" $ do
    let fp = "ChangeLog.md"
        pc = setStdout createSource $ proc "cat" [fp]
    bs <- B.readFile fp
    bss <- withProcess_ pc $ \p ->
      runConduit (getStdout p .| CL.consume) <* waitExitCode p
    B.concat bss `shouldBe` bs
  it "cat works with withLoggedProcess_" $ do
    let fp = "ChangeLog.md"
        pc = proc "cat" [fp]
    bs <- B.readFile fp
    bss <- withLoggedProcess_ pc $ \p ->
      runConduit (getStdout p .| CL.consume) <* waitExitCode p
    B.concat bss `shouldBe` bs
  it "failing process throws" $ do
    (withLoggedProcess_ (proc "cat" ["does not exist"]) $ \p -> do
      runConduit $ getStdout p .| CL.mapM_ (error "shouldn't have data"))
      `shouldThrow` anyException
  it "failing process throws" $ do
    (withProcess_ (proc "cat" ["does not exist"]) $ const $ return ())
      `shouldThrow` anyException
