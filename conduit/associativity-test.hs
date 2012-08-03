{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Trans.Writer
import Data.Conduit
import Data.Conduit.List as L
import Control.Monad.Trans.Class


tellLn = tell . (++ "\n")
finallyP fin = addCleanup (const fin)
idP = awaitForever yield
printer = awaitForever $ lift . tellLn . show
idMsg msg = finallyP (tellLn msg) idP
takeP 0 = return ()
takeP n = awaitE >>= \ex -> case ex of
  Left _u -> return ()
  Right i -> yield i >> takeP (pred n)

testPipe p = execWriter $ runPipe $ printer <+< p <+< sourceList [1..]

p1 = takeP 1
p2 = idMsg "foo"
p3 = idMsg "bar"

test1L = testPipe $ (p1 <+< p2) <+< p3
test1R = testPipe $ p1 <+< (p2 <+< p3)

test2L = testPipe $ (p2 <+< p1) <+< p3
test2R = testPipe $ p2 <+< (p1 <+< p3)

test3L = testPipe $ (p2 <+< p3) <+< p1
test3R = testPipe $ p2 <+< (p3 <+< p1)

verify testL testR p1 p2 p3
  | testL == testR = putStrLn "SUCCESS"
  | otherwise = putStrLn $ unlines
    [ "FAILURE"
    , ""
    , "(" ++ p1 ++ " <+< " ++ p2 ++ ") <+< " ++ p3
    , "------------------"
    , testL
    , ""
    , p1 ++ " <+< (" ++ p2 ++ " <+< " ++ p3 ++ ")"
    , testR
    ]
 


main = do
  verify test1L test1R "p1" "p2" "p3"
  verify test2L test2R "p2" "p1" "p3"
  verify test3L test3R "p2" "p3" "p1"
