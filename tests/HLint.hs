module Main where

import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = do
  hints <- hlint ["src", "tests"]
  if null hints then exitSuccess else exitFailure
