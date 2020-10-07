module Exec (exec) where

import Frequency (wordcount, linecount, getSortedWords )
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
         _ -> putStrLn "??"


help :: IO()
help = 
  do
    putStrLn ""
    putStrLn "  Commands"
    putStrLn "  ----------------------------"
    putStrLn "  stats FILENAME"
    putStrLn "  freq  FILENAME START HOWMANY"
    putStrLn ""


stats :: [String] -> IO ()
stats args = 
  let
    filePath = args !! 0
  in  
  do  
    contents <- readFile filePath
    putStrLn ""
    putStrLn "File:  whitman.txt" 
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
    putStrLn "File:  whitman.txt" 
    putStrLn ""
    putStrLn $ howMany ++ " words from index " ++ start ++ ": " -- ++ show (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn $ formatFrequenceList $ sliceSortedWords (read start) (read howMany) contents   -- (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn ""


sliceSortedWords :: Int -> Int ->  String -> [(String, Int)]
sliceSortedWords start howMany text =
  take howMany $ drop start $ getSortedWords text

formatFrequenceList :: [(String, Int)] -> String
formatFrequenceList pairs = 
  foldr (\(s, k) acc -> (padRight 10 s ++ show k ++ "\n" ++ acc)) "" pairs

padRight :: Int -> String -> String
padRight i str =
  let
    n = length str
    j = max (i - n) 0
    blanks = take j $ repeat ' '
  in
    str ++ blanks