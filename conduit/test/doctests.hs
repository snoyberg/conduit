module Main where

import System.Directory
import Test.DocTest

main :: IO ()
main = do
  writeFile "input.txt" "some dummy content"
  doctest ["Data/Conduit.hs"]
  removeFile "input.txt"
  removeFile "output.txt"
