module Normalize where


import Data.List.Extra(lower)
import Data.List.Ordered (member)    

type CharSet = [Char]


normalize :: String -> String
normalize = lower . deletePunctuation

isCharInSet :: CharSet -> Char -> Bool
isCharInSet charSet c = member c charSet

punctuation :: [Char]
punctuation = [',', '.', ';', '!', ':', '?']

deleteChars :: CharSet -> String -> String
deleteChars charSet input = 
    filter (not . (isCharInSet charSet)) input

-- > deletePunctuation "a.c,d;e:f!g?"
-- "acde:f!g"
deletePunctuation :: String -> String
deletePunctuation  = deleteChars punctuation

