{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative (many, optional)
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

threeItemGet :: Get Word8
threeItemGet = do
  x <- getWord8
  y <- getWord8
  z <- getWord8
  return $ x + y + z

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

sinkGetMaybe :: Get Word8 -> C.Consumer BS.ByteString (ExceptionT Identity) Word8
sinkGetMaybe = mkSinkGet errorHandler terminationHandler
  where errorHandler       _ = return 34
        terminationHandler _ = return 114

sinktest7 :: Test
sinktest7 = TestCase (assertEqual "Leftover input with failure works"
  (Right (34, BS.pack [1, 2]))
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [1, 2]]) C.$$ (do
    output <- sinkGetMaybe (getWord8 >> fail "" :: Get Word8)
    output' <- CL.consume
    return (output, BS.concat output'))))

sinktest8 :: Test
sinktest8 = TestCase (assertEqual "Leftover with incomplete input works"
  (Right (114, BS.singleton 1))
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.singleton 1]) C.$$ (do
    output <- sinkGetMaybe twoItemGet
    output' <- CL.consume
    return (output, BS.concat output'))))

sinktest9 :: Test
sinktest9 = TestCase (assertEqual "Properly terminate Partials"
  (Right [0..255])
  (runIdentity $ runExceptionT $ mapM_ (C.yield . BS.singleton) [0..255] C.$$ sinkGet (many getWord8)))

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

conduittest13 :: Test
conduittest13 = TestCase (assertEqual "Leftover success conduit input works"
  (Right [Right 12, Right 7, Left (BS.pack [5])])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [10, 2, 3], BS.pack [4, 5]]) C.$= fancyConduit C.$$ CL.consume))
  where fancyConduit = do
          conduitGet twoItemGet C.=$= CL.map (\ x -> Right x)
          recurse
          where recurse = C.await >>= maybe (return ()) (\ x -> C.yield (Left x) >> recurse)

conduittest14 :: Test
conduittest14 = TestCase (assertEqual "Leftover coercing works"
  (Right [Left (BS.pack [10, 2])])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [10], BS.pack [2]]) C.$= fancyConduit C.$$ CL.consume))
  where fancyConduit = do
          conduitGet threeItemGet C.=$= CL.map (\ x -> Right x)
          recurse
          where recurse = C.await >>= maybe (return ()) (\ x -> C.yield (Left x) >> recurse)

conduittest15 :: Test
conduittest15 = TestCase (assertEqual "Leftover premature end conduit input works"
  (Right ([], BS.singleton 1))
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.singleton 1]) C.$$ (do
    output <- (conduitGet twoItemGet) C.=$ (CL.take 1)
    output' <- CL.consume
    return (output, BS.concat output'))))

conduittest16 :: Test
conduittest16 = TestCase (assertEqual "Leftover failure conduit input works"
  (Right [Left $ BS.pack [10, 11], Left $ BS.singleton 2])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.pack [10, 11], BS.pack [2]]) C.$= fancyConduit C.$$ CL.consume))
  where fancyConduit = do
          mkConduitGet (const $ return ()) (getWord8 >> fail "asdf" :: Get Word8) C.=$= CL.map (\ x -> Right x)
          recurse
          where recurse = C.await >>= maybe (return ()) (\ x -> C.yield (Left x) >> recurse)

conduittest17 :: Test
conduittest17 = TestCase (assertEqual "Leftover failure conduit with broken input works"
  (Right [Left $ BS.pack [10, 11], Left $ BS.singleton 12])
  (runIdentity $ runExceptionT $ (CL.sourceList [BS.singleton 10, BS.singleton 11, BS.singleton 12]) C.$= fancyConduit C.$$ CL.consume))
  where fancyConduit = do
          mkConduitGet (const $ return ()) (twoItemGet >> fail "asdf" :: Get Word8) C.=$= CL.map (\ x -> Right x)
          recurse
          where recurse = C.await >>= maybe (return ()) (\ x -> C.yield (Left x) >> recurse)

-- see https://github.com/snoyberg/conduit/issues/246
conduittest18 :: Test
conduittest18 = TestCase $ assertEqual "Deals with Get that consumes everything"
    (Right ["hello"])
    (runIdentity $ runExceptionT
                 $ (C.yield "hello"
               C.$= conduitGet2 slurp
               C.$$ CL.consume))

slurp :: Get BS.ByteString
slurp =
    loop id
  where
    loop front = do
        x <- remaining
        if x == 0
            then do
                mbs <- optional $ lookAhead $ getBytes 1
                case mbs of
                    Nothing -> do
                        let bs = BS.concat $ front []
                        if BS.null bs
                            then fail "no bytes remaining"
                            else return bs
                    Just _ -> loop front
            else do
                bs <- getBytes $ fromIntegral x
                loop (front . (bs:))

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
                     , sinktest8
                     , sinktest9
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
                        , conduittest13
                        , conduittest14
                        , conduittest15
                        , conduittest16
                        , conduittest17
                        , conduittest18
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
