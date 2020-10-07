module Frequency where

import qualified Data.Map as DM
import Data.List (sortBy)
import Utility ((|>))
import Normalize (normalize)


type Line = String

linecount :: String -> Int
linecount  = length . lines

wordcount :: String -> Int
wordcount  = length . words


-- FREQUENCIES

insert :: Ord a => a -> DM.Map a Int -> DM.Map a Int
insert key dict =
    case DM.lookup key dict of
        Just v -> DM.insert key (v + 1 )dict
        Nothing -> DM.insert key 1 dict


compile:: Ord a => [a] -> DM.Map a Int
compile as = 
    foldr insert DM.empty as


relativeFrequencies :: [(a, Int)] ->[(a, Double)]
relativeFrequencies frequencies = 
    let
        total = fromIntegral $ sum (map snd frequencies)
    in
        map (\(key, value) -> (key, (fromIntegral value) / total)) frequencies


sorted :: Ord a => [a] -> [(a, Int)]
sorted things =  
    things 
      |> compile
      |> DM.toList
      |> sortBy (\(k,v) (k',v') -> compare v v')
      |> reverse


slice :: Int -> Int ->  [a] -> [a]
slice start howMany as =
  take howMany $ drop start $ as

 -- CHARACTERS

sortedChars :: String -> [(Char, Int)]
sortedChars str = 
    str 
      |> compile
      |> DM.toList
      |> sortBy (\(k,v) (k',v') -> compare v v')
      |> reverse

-- WORDS


sortedWords :: String -> [(String, Int)]
sortedWords str =
    str
      |> normalize
      |> words
      |> sorted



words_ :: String -> [(String, Int)]
words_ str =
    str
      |> normalize
      |> words
      |> sorted





-- DIGRAMS


-- > digramsOfText text
--   [("^","a"),("a","b"),("b","c"),("c","$"),("^","d"),("d","e"),("e","f"),("f","$")
digramsOfText :: String -> [(String, String)]
digramsOfText str = 
    str 
      |> lines
      |> map normalize
      |> map digramsOfLine
      |> concat

    
-- > text |> digramsOfText |> compileDigrams
--   fromList [(("^","a"),2),(("a","b"),2),(("b","c"),1),(("b","f"),1),(("c","$"),1),(("f","$"),1)]
compileDigrams :: [(String, String)] -> DM.Map (String, String) Int
compileDigrams digrams = 
    foldr insert DM.empty digrams

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
