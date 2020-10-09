module Exec (exec) where

import Frequency (wordcount, linecount, sortedChars, 
   sortedWords, slice, relativeFrequencies )
import Normalize (normalize, normalize')
import Utility ((|>))

import Numeric 
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

-- DISPATCHER

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStrLn "??"
     (cmd:args) -> 
       case cmd of
         "help" -> help
         "stats" -> stats args
         "cfreq" -> charFreq args
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
    putStrLn "  cfreq FILENAME                      character frequencies"
    putStrLn "  freq  FILENAME START HOWMANY        word frequencies"
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
    putStrLn $ "Chars: " ++ show (length contents)
    putStrLn $ "Words: " ++ show (wordcount contents)
    putStrLn $ "Types: " ++ (show $ length $ sortedWords contents)
    putStrLn $ "Lines: " ++ show (linecount contents)
    putStrLn ""


charFreq :: [String] -> IO ()
charFreq args = stats1 getRelativeCharFrequencies args    

freq :: [String] -> IO ()
freq args = stats2 getFrequencies args

relFreq :: [String] -> IO ()
relFreq args = stats2 getRelativeFrequencies args

logRelFreq :: [String] -> IO ()
logRelFreq args = stats2 getLogRelativeFrequencies args


-- Dispatcher for freq, relFrea, logRelFreq ...
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

stats1 :: (String -> String) -> [String] -> IO ()
stats1 f args = 
  let
    filePath = args !! 0
  in  
  do  
    contents <- readFile filePath
    putStrLn ""
    putStrLn $ "File: " ++ filePath 
    putStrLn ""
    putStrLn $ f contents
    putStrLn ""

-- GET CHAR FREQUENCES 




getRelativeCharFrequencies :: String -> String
getRelativeCharFrequencies contents =
   formatCharDoublePairs 5 $ relativeFrequencies $ sortedChars $ normalize' $ contents 

sliceSortedChars :: Int -> Int ->  String -> [(Char, Int)]
sliceSortedChars start howMany text =
   slice start howMany $ sortedChars text

formatCharDoublePairs :: Int -> [(Char, Double)] -> String
formatCharDoublePairs padding pairs = 
  foldr (\(s, k) acc -> (padRight padding (show s) ++ formatFloatN k 4 ++ "\n" ++ acc)) "" pairs

-- GET WORD FREQUENCIES

getFrequencies :: String -> Int -> Int -> String
getFrequencies contents start howMany = 
  formatIntPairs 15 $ sliceSortedWords start howMany contents 

getRelativeFrequencies :: String -> Int -> Int -> String
getRelativeFrequencies contents start howMany = 
  formatDoublePairs 15 $ relativeFrequencies $ sliceSortedWords start howMany contents 

getLogRelativeFrequencies :: String -> Int -> Int -> String
getLogRelativeFrequencies contents start howMany = 
   formatDoublePairs 15 $ map (\(a,b) -> (a, -log b)) $ relativeFrequencies $ sliceSortedWords start howMany contents 


-- SELECT SLICE OF DATA

sliceSortedWords :: Int -> Int ->  String -> [(String, Int)]
sliceSortedWords start howMany text =
  slice start howMany $ sortedWords text

-- FORMATTING

formatIntPairs :: Int -> [(String, Int)] -> String
formatIntPairs padding pairs = 
  foldr (\(s, k) acc -> (padRight padding s ++ show k ++ "\n" ++ acc)) "" pairs


formatDoublePairs :: Int -> [(String, Double)] -> String
formatDoublePairs padding pairs = 
  foldr (\(s, k) acc -> (padRight padding s ++ formatFloatN k 4 ++ "\n" ++ acc)) "" pairs

-- HELPERS

padRight :: Int -> String -> String
padRight i str =
  let
    n = length str
    j = max (i - n) 0
    blanks = take j $ repeat ' ' in
    str ++ blanks
