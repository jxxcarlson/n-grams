module Normalize (normalize, normalize') where


import Data.List.Extra(lower)
import Data.List.Ordered (member)    

type CharSet = [Char]


normalize :: String -> String
normalize = lower . deletePunctuation

normalize' :: String -> String
normalize' = (passCharSet alphabet) . lower

isCharInSet :: CharSet -> Char -> Bool
isCharInSet charSet c = member c charSet

punctuation :: [Char]
punctuation = [',', '.', ';', '!', ':', '?']

alphabet :: [Char]
alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y' ,'z']

passCharSet :: CharSet -> String -> String
passCharSet charSet input = 
    filter (isCharInSet charSet) input

deleteChars :: CharSet -> String -> String
deleteChars charSet input = 
    filter (not . (isCharInSet charSet)) input

-- > deletePunctuation "a.c,d;e:f!g?"
-- "acde:f!g"
deletePunctuation :: String -> String
deletePunctuation  = deleteChars punctuation

