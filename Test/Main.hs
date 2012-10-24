{-# LANGUAGE FlexibleContexts #-}

import           Control.Exception.Base
import           Control.Monad.Identity
import           Control.Monad.Trans.Resource
import           Test.HUnit
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.List as L
import           Data.Word
import           System.Exit
--import           Test.Framework.Providers.HUnit

import           Data.Conduit.Cereal
import           Data.Conduit.Cereal.Internal 

-- For the sake of these tests, all SomeExceptions are equal
instance Eq SomeException where
  a == b = True

twoItemGet :: Get Word8
twoItemGet = do
  x <- getWord8
  y <- getWord8
  return $ x + y

putter :: Putter Char
putter c = put x >> put (x + 1)
  where x = (fromIntegral $ (fromEnum c) - (fromEnum 'a') :: Word8)

sinktest1 :: Test
sinktest1 = TestCase (assertEqual "Handles starting with empty bytestring"
  (Right 1)
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [], BS.pack [1]]) C.$$ (sinkGet getWord8)))

sinktest2 :: Test
sinktest2 = TestCase (assertEqual "Handles empty bytestring in middle"
  (Right [1, 3])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1], BS.pack [], BS.pack [3]]) C.$$ (sinkGet (do
    x <- getWord8
    y <- getWord8
    return [x, y]))))

sinktest3 :: Test
sinktest3 = TestCase (assertBool "Handles no data"
  (case runIdentity $ runExceptionT $ (CL.sourceList []) C.$$ (sinkGet getWord8) of
    Right _ -> False
    Left _ -> True))

sinktest4 :: Test
sinktest4 = TestCase (assertEqual "Consumes no data"
  (Right ())
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1]]) C.$$ (sinkGet $ return ())))

sinktest5 :: Test
sinktest5 = TestCase (assertEqual "Empty list"
  (Right ())
  (runIdentity $ runExceptionT $ (CL.sourceList []) C.$$ (sinkGet $ return ())))

sinktest6 :: Test
sinktest6 = TestCase (assertEqual "Leftover input works"
  (Right (1, BS.pack [2, 3, 4, 5]))
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2, 3], BS.pack [4, 5]]) C.$$ (do
    output <- sinkGet getWord8
    output' <- CL.consume
    return (output, BS.concat output'))))

-- Current sink implementation will terminate the pipe in case of error. 
-- One may need non-terminating version like one defined below to get access to Leftovers

sinkGetMaybe :: Get Word8 -> C.GLSink BS.ByteString (ExceptionT Identity) Word8
sinkGetMaybe = mkSinkGet errorHandler terminationHandler
  where errorHandler     msg = return 34
        terminationHandler f = return 0

sinktest7 :: Test
sinktest7 = TestCase (assertEqual "Leftover input with failure works"
  (Right (34, BS.pack [1, 2]))
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2]]) C.$$ (do
    output <- sinkGetMaybe (getWord8 >> fail "" :: Get Word8)
    output' <- CL.consume
    return (output, BS.concat output'))))

conduittest1 :: Test
conduittest1 = TestCase (assertEqual "Handles starting with empty bytestring"
  (Right [])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [], BS.pack [1]]) C.$= conduitGet twoItemGet C.$$ CL.consume))

conduittest2 :: Test
conduittest2 = TestCase (assertEqual "Works when the get is split across items"
  (Right [3])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1], BS.pack [2]]) C.$= conduitGet twoItemGet C.$$ CL.consume))

conduittest3 :: Test
conduittest3 = TestCase (assertEqual "Works when empty bytestring in middle of get"
  (Right [3])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1], BS.pack [], BS.pack [2]]) C.$= conduitGet twoItemGet C.$$ CL.consume))

conduittest4 :: Test
conduittest4 = TestCase (assertEqual "Works when empty bytestring at end of get"
  (Right [3])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2], BS.pack []]) C.$= conduitGet twoItemGet C.$$ CL.consume))

conduittest5 :: Test
conduittest5 = TestCase (assertEqual "Works when multiple gets are in an item"
  (Right [3, 7])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2, 3, 4]]) C.$= conduitGet twoItemGet C.$$ CL.consume))

conduittest6 :: Test
conduittest6 = TestCase (assertEqual "Works with leftovers"
  (Right [3])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2, 3]]) C.$= conduitGet twoItemGet C.$$ CL.consume))

conduittest7 :: Test
conduittest7 = let c = 10 in TestCase (assertEqual "Works with infinite lists"
  (Right $ L.replicate c ())
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2, 3]]) C.$= conduitGet (return ()) C.$$ CL.take c))

conduittest8 :: Test
conduittest8 = let c = 10 in TestCase (assertEqual "Works with empty source and infinite lists"
  (Right $ L.replicate c ())
  (runIdentity $ runExceptionT $ (CL.sourceList []) C.$= conduitGet (return ()) C.$$ CL.take c))

conduittest9 :: Test
conduittest9 = let c = 10 in TestCase (assertEqual "Works with two well-placed items"
  (Right [3, 7])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2], BS.pack [3, 4]]) C.$= conduitGet twoItemGet C.$$ CL.consume))

conduittest10 :: Test
conduittest10 = TestCase (assertBool "Failure works"
  (case runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2], BS.pack [3, 4]]) C.$= conduitGet (getWord8 >> fail "omfg") C.$$ CL.consume of
    Left _ -> True
    Right _ -> False))

conduittest11 :: Test
conduittest11 = TestCase (assertBool "Immediate failure works"
  (case runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2], BS.pack [3, 4]]) C.$= conduitGet (fail "omfg") C.$$ CL.consume of
    Left _ -> True
    Right _ -> False))

conduittest12 :: Test
conduittest12 = TestCase (assertBool "Immediate failure with empty input works"
  (case runIdentity $ runExceptionT $ (CL.sourceList []) C.$= conduitGet (fail "omfg") C.$$ CL.consume of
    Left _ -> True
    Right _ -> False))

-- This test won't work because of auto-termination. As soon as the conduit yields the "12" value,
-- it is stopped before it has a chance to run 'leftover'
conduittest13 :: Test
conduittest13 = TestCase (assertEqual "Leftover success conduit input works"
  (Right ([12], BS.pack [3, 4, 5]))
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [10, 2, 3], BS.pack [4, 5]]) C.$$ (do
    output <- (conduitGet twoItemGet) C.=$ (CL.take 1)
    output' <- CL.consume
    return (output, BS.concat output'))))

conduittest14 :: Test
conduittest14 = TestCase (assertEqual "Leftover failure conduit input works"
  (Right ([], BS.singleton 1))
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.singleton 1]) C.$$ (do
    output <- (conduitGet twoItemGet) C.=$ (CL.take 1)
    output' <- CL.consume
    return (output, BS.concat output'))))

puttest1 :: Test
puttest1 = TestCase (assertEqual "conduitPut works"
  [BS.pack [0, 1]]
  (runIdentity $ (CL.sourceList ['a']) C.$= (conduitPut putter) C.$$ CL.consume))

puttest2 :: Test
puttest2 = TestCase (assertEqual "multiple input conduitPut works"
  [BS.pack [0, 1], BS.pack [1, 2], BS.pack [2, 3]]
  (runIdentity $ (CL.sourceList ['a', 'b', 'c']) C.$= (conduitPut putter) C.$$ CL.consume))

puttest3 :: Test
puttest3 = TestCase (assertEqual "empty input conduitPut works"
  []
  (runIdentity $ (CL.sourceList []) C.$= (conduitPut putter) C.$$ CL.consume))

sinktests = TestList [ sinktest1
                     , sinktest2
                     , sinktest3
                     , sinktest4
                     , sinktest5
                     , sinktest6
                     , sinktest7
                     ]

conduittests = TestList [ conduittest1
                        , conduittest2
                        , conduittest3
                        , conduittest4
                        , conduittest5
                        , conduittest6
                        , conduittest7
                        , conduittest8
                        , conduittest9
                        , conduittest10
                        , conduittest11
                        , conduittest12
                        --, conduittest13
                        , conduittest14
                        ]

puttests = TestList [ puttest1
                    , puttest2
                    , puttest3
                    ]

hunittests = TestList [sinktests, conduittests, puttests]

--tests = hUnitTestToTests hunittests

main = do
  counts <- runTestTT hunittests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
