-- | Run Haskell tr implementation.

module Main where

-- Imports
-- =======

import System.Environment (getArgs)

import Tr

-- Functionality
-- =============

-- | Main - parse args, and read from stdin.
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just trArgs -> trRepl trArgs
    Nothing     -> putStrLn usage
  return ()

trRepl :: (CharSet, Maybe CharSet) -> IO ()
trRepl args = do
  input <- getLine
  putStrLn $ tr args input
  trRepl args

usage :: String
usage = "Bad args: Run this function as `tr -d <string>` or `tr <string1>\
        \ <string2>`"
