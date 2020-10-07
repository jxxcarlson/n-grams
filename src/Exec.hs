module Exec (exec) where

import Frequency (wordcount, linecount, getSortedWords, relativeFrequencies )
import Utility ((|>))


-- DISPATCHER

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



-- COMMMANDS

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
freq args = stats2 getFrequencies args

relFreq :: [String] -> IO ()
relFreq args = stats2 getRelativeFrequencies args

logRelFreq :: [String] -> IO ()
logRelFreq args = stats2 getLogRelativeFrequencies args


-- Diapatcher for freq, relFrea, logRelFreq ...
stats2 :: (String -> Int -> Int -> String) -> [String] -> IO ()
stats2 f args = 
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
    putStrLn $ howMany ++ " words from index " ++ start ++ ":\n" 
    putStrLn $ f contents (read start) (read howMany)
    putStrLn ""


-- GET FREQUENCIES

getFrequencies :: String -> Int -> Int -> String
getFrequencies contents start howMany = 
  formatIntPairs $ sliceSortedWords start howMany contents 

getRelativeFrequencies :: String -> Int -> Int -> String
getRelativeFrequencies contents start howMany = 
  formatDoublePairs $ relativeFrequencies $ sliceSortedWords start howMany contents 

getLogRelativeFrequencies :: String -> Int -> Int -> String
getLogRelativeFrequencies contents start howMany = 
   formatDoublePairs $ map (\(a,b) -> (a, -log b)) $ relativeFrequencies $ sliceSortedWords start howMany contents 


-- SELECT SLICE OF DATA

sliceSortedWords :: Int -> Int ->  String -> [(String, Int)]
sliceSortedWords start howMany text =
  take howMany $ drop start $ getSortedWords text

-- FORMATTING

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