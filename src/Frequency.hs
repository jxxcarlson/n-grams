module Frequency where

import qualified Data.Map as DM
import Data.List (sortBy)
import Utility ((|>))
import Normalize (normalize)


linecount :: String -> Int
linecount  = length . lines

wordcount :: String -> Int
wordcount  = length . words




-- > compileFrequencies ["one", "two", "one"]
-- fromList [("one",2),("two",1)]
compile:: [String] -> DM.Map String Int
compile words = 
    foldr insertWord DM.empty words



-- > digramsOfText text
--   [("^","a"),("a","b"),("b","c"),("c","$"),("^","d"),("d","e"),("e","f"),("f","$")
digramsOfText :: String -> [(String, String)]
digramsOfText str = 
    str 
      |> lines
      |> map normalize
      |> map digramsOfLine
      |> concat

        
sortedWords :: [String] -> [(String, Int)]
sortedWords words = 
    words 
      |> compile
      |> DM.toList
      |> sortBy (\(k,v) (k',v') -> compare v v')
      |> reverse


getSortedWords :: String -> [(String, Int)]
getSortedWords str =
    str
      |> normalize
      |> words
      |> sortedWords


insertWord :: Ord a => a -> DM.Map a Int -> DM.Map a Int
insertWord word dict =
    case DM.lookup word dict of
        Just v -> DM.insert word (v + 1 )dict
        Nothing -> DM.insert word 1 dict

-- > compileFrequencies ["one", "two", "one"]
-- fromList [("one",2),("two",1)]
relativeFrequencies :: [(String, Int)] ->[(String, Double)]
relativeFrequencies frequencies = 
    let
        total = fromIntegral $ sum (map snd frequencies)
    in
        map (\(key, value) -> (key, (fromIntegral value) / total)) frequencies


-- > text |> digramsOfText |> compileDigramFrequencies
--   fromList [(("^","a"),2),(("a","b"),2),(("b","c"),1),(("b","f"),1),(("c","$"),1),(("f","$"),1)]
compileDigrams :: [(String, String)] -> DM.Map (String, String) Int
compileDigrams digrams = 
    foldr insertWord DM.empty digrams

-- DIGRAMS

type Line = String

-- Assume the line is normalized
-- > digrams "a b c"
--   [("^","a"),("a","b"),("b","c"),("c","$")]
digramsOfLine :: String -> [(String, String)]
digramsOfLine str = 
    let 
        ws = words str
        ws' = "^":ws
        ws'' = reverse $ "$":(reverse ws)
    in
    zip ws' ws''
