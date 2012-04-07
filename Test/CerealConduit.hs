module Test.CerealConduit (tests) where

import Control.Monad.Identity
import Test.HUnit
import qualified Data.Conduit as C
import Data.Conduit.Cereal
import Data.Conduit.List
import Data.Serialize
import qualified Data.ByteString as BS
import Test.Framework.Providers.HUnit
import System.Exit

test1 :: Test
test1 = TestCase (assertEqual "Handles starting with empty bytestring"
  (Right 1)
  (runIdentity $ (sourceList [BS.pack [], BS.pack [1]]) C.$$ (sinkGet getWord8)))

test2 :: Test
test2 = TestCase (assertEqual "Handles empty bytestring in middle"
  (Right [1, 3])
  (runIdentity $ (sourceList [BS.pack [1], BS.pack [], BS.pack [3]]) C.$$ (sinkGet (do
    x <- getWord8
    y <- getWord8
    return [x, y]))))

test3 :: Test
test3 = TestCase (assertBool "Handles no data"
  (case (runIdentity $ (sourceList []) C.$$ (sinkGet getWord8)) of
    Right _ -> False
    Left _ -> True))

test4 :: Test
test4 = TestCase (assertEqual "Consumes no data"
  (Right ())
  (runIdentity $ (sourceList [BS.pack [1]]) C.$$ (sinkGet $ return ())))

hunittests = TestList [test1, test2, test3, test4]

tests = hUnitTestToTests hunittests

main = do
  counts <- runTestTT hunittests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
