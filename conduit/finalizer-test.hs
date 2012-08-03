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

example1 = takeP 1 <+< finallyP (tellLn "Outer") (finallyP (tellLn "Inner") idP)
example2 = takeP 2 <+< finallyP (tellLn "End") (do
  finallyP (tellLn "One") (takeP 1)
  lift (tellLn "Two")
  finallyP (tellLn "Three") idP)


main = do
  putStrLn "Nested finalizers work like a stack: inside first"
  putStrLn $ testPipe example1
  putStrLn "Pipes in monadic sequence run their personal finalizers in order,"
  putStrLn "always before the next step in the sequence can begin."
  putStrLn $ testPipe example2
