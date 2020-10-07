module Exec (exec) where

import Frequency (wordcount, linecount, getSortedWords, relativeFrequencies )
import Utility ((|>))

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStrLn "??"
     (cmd:args) -> 
       case cmd of
         "help" -> help
         "stats" -> stats args
         "freq" -> freq args
         "rfreq" -> relFreq args
         "lfreq" -> logRelFreq args
         _ -> putStrLn "??"


help :: IO()
help = 
  do
    putStrLn ""
    putStrLn "  Commands"
    putStrLn "  ------------------------------------------------------------"
    putStrLn "  stats FILENAME                      word, line, type count"
    putStrLn "  freq  FILENAME START HOWMANYb       word frequencies"
    putStrLn "  rfreq  FILENAME START HOWMANY       relative frequencies"
    putStrLn "  lfreq  FILENAME START HOWMANY       log relative requencies"
    putStrLn ""


stats :: [String] -> IO ()
stats args = 
  let
    filePath = args !! 0
  in  
  do  
    contents <- readFile filePath
    putStrLn ""
    putStrLn $ "File: " ++ filePath 
    putStrLn $ "Words: " ++ show (wordcount contents)
    putStrLn $ "Types: " ++ (show $ length $ getSortedWords contents)
    putStrLn $ "Lines: " ++ show (linecount contents)
    putStrLn ""


freq :: [String] -> IO ()
freq args = 
  let
    filePath = args !! 0
    start = args !! 1
    howMany = args !! 2
  in  
  do  
    contents <- readFile filePath
    putStrLn ""
    putStrLn $ "File: " ++ filePath 
    putStrLn ""
    putStrLn $ howMany ++ " words from index " ++ start ++ ": " -- ++ show (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn $ formatIntPairs $ sliceSortedWords (read start) (read howMany) contents   -- (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn ""


relFreq :: [String] -> IO ()
relFreq args = 
  let
    filePath = args !! 0
    start = args !! 1
    howMany = args !! 2
  in  
  do  
    contents <- readFile filePath
    putStrLn ""
    putStrLn $ "File: " ++ filePath 
    putStrLn ""
    putStrLn $ howMany ++ " words from index " ++ start ++ ": " -- ++ show (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn $ formatDoublePairs $ relativeFrequencies $ sliceSortedWords (read start) (read howMany) contents   -- (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn ""


logRelFreq :: [String] -> IO ()
logRelFreq args = 
  let
    filePath = args !! 0
    start = args !! 1
    howMany = args !! 2
  in  
  do  
    contents <- readFile filePath
    putStrLn ""
    putStrLn $ "File: " ++ filePath 
    putStrLn ""
    putStrLn $ howMany ++ " words from index " ++ start ++ ": " -- ++ show (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn $ formatDoublePairs $ map (\(a,b) -> (a, -log b)) $ relativeFrequencies $ sliceSortedWords (read start) (read howMany) contents   -- (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn ""


sliceSortedWords :: Int -> Int ->  String -> [(String, Int)]
sliceSortedWords start howMany text =
  take howMany $ drop start $ getSortedWords text

formatIntPairs :: [(String, Int)] -> String
formatIntPairs pairs = 
  foldr (\(s, k) acc -> (padRight 15 s ++ show k ++ "\n" ++ acc)) "" pairs


formatDoublePairs :: [(String, Double)] -> String
formatDoublePairs pairs = 
  foldr (\(s, k) acc -> (padRight 15 s ++ show k ++ "\n" ++ acc)) "" pairs

-- HELPERS

padRight :: Int -> String -> String
padRight i str =
  let
    n = length str
    j = max (i - n) 0
    blanks = take j $ repeat ' ' in
    str ++ blanks