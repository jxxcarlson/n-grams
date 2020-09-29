import System.IO  
import Lib





main :: IO ()
main = do  
    handle <- openFile "whitman.txt" ReadMode  
    contents <- hGetContents handle 
    putStrLn ""
    putStrLn "File:  whitman.txt" 
    putStrLn $ "Words: " ++ show (wordcount contents)
    putStrLn $ "Types: " ++ (show $ length $ getSortedWords contents)
    putStrLn $ "Lines: " ++ show (linecount contents)
    putStrLn ""
    putStrLn "start: "
    start <- getLine
    putStrLn "number of words: "
    howMany <- getLine
    putStrLn ""
    putStrLn $ howMany ++ " words from index " ++ start ++ ": " ++ show (take (read howMany)$ drop (read start) $ getSortedWords contents)
    putStrLn ""
    hClose handle 

   