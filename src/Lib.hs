module Lib where

import qualified Data.Map as DM
import Data.List.Extra(lower) 
import Data.List (sortBy)
import Data.List.Ordered (member)
-- import Text.Regex.TDFA
-- import Text.Regex.TDFA.Text ()

-- https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html

type CharSet = [Char]

(|>) ::  a -> (a -> b)-> b
(|>) a f = f a


-- SIMPLE COUNTS

linecount :: String -> Int
linecount  = length . lines

wordcount :: String -> Int
wordcount  = length . words



-- NORMALIZATION

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




-- UNIGRAMS

insertWord :: Ord a => a -> DM.Map a Int -> DM.Map a Int
insertWord word dict =
    case DM.lookup word dict of
        Just v -> DM.insert word (v + 1 )dict
        Nothing -> DM.insert word 1 dict

-- > compileFrequencies ["one", "two", "one"]
-- fromList [("one",2),("two",1)]
compileFrequencies :: [String] -> DM.Map String Int
compileFrequencies words = 
    foldr insertWord DM.empty words


        
sortedWords :: [String] -> [(String, Int)]
sortedWords words = 
    words 
      |> compileFrequencies
      |> DM.toList
      |> sortBy (\(k,v) (k',v') -> compare v v')
      |> reverse


getSortedWords :: String -> [(String, Int)]
getSortedWords str =
    str
      |> normalize
      |> words
      |> sortedWords



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


-- > digramsOfText text
--   [("^","a"),("a","b"),("b","c"),("c","$"),("^","d"),("d","e"),("e","f"),("f","$")
digramsOfText :: String -> [(String, String)]
digramsOfText str = 
    str 
      |> lines
      |> map normalize
      |> map digramsOfLine
      |> concat

text :: String
text = "a b c\na b f"

-- > text |> digramsOfText |> compileDigramFrequencies
--   fromList [(("^","a"),2),(("a","b"),2),(("b","c"),1),(("b","f"),1),(("c","$"),1),(("f","$"),1)]
compileDigramFrequencies :: [(String, String)] -> DM.Map (String, String) Int
compileDigramFrequencies digrams = 
    foldr insertWord DM.empty digrams


-- STUFF

topWords' :: [(String, Int)]
topWords' = [("the",10167),("and",5347),("of",4364),("i",2779),("to",2208),("in",1894),("a",1305),("with",1284),("you",1244),("is",1100),("or",1071),("my",1000),("for",954),("all",837),("that",833),("not",801),("as",798),("from",712),("it",653),("on",607),("are",585),("me",556),("o",547),("by",497),("at",483),("be",476),("what",453),("they",445),("but",409),("see",373),("his",373),("your",369),("their",358),("have",351),("this",327),("we",319),("one",292),("me,",292),("he",290),("these",278),("will",267),("no",264),("through",261),("shall",260),("do",258),("nor",256),("now",247),("out",246),("its",243),("old",229),("so",228),("them",224),("am",224),("than",222),("where",220),("thy",220),("any",218),("how",216),("you,",215),("yet",212),("who",209),("there",209),("more",204),("was",187),("those",180),("every",180),("thou",168),("her",167),("over",164),("some",161),("all,",157),("know",155),("has",155),("here",154),("long",152),("upon",151),("up",150),("if",147),("them,",146),("great",145),("life",143),("man",141),("an",138),("many",134),("each",133),("ever",131),("never",125),("after",125),("when",124),("him",124),("come",123),("hear",122),("then",120),("love",120),("men",119),("only",118),("give",117),("day",116),("own",114),("like",113),("thee",109),("she",107),("our",107),("young",105),("myself",104),("earth",101),("think",100),("night",100),("can",100),("were",97),("earth,",96),("soul",95),("other",94),("may",93),("time",91),("thee,",91),("love,",91),("again",91),("too",90),("would",89),("while",89),("which",89),("little",89),("new",88),("face",88),("down",88),("under",86),("before",86),("project",85),("good",85),("look",84),("body",84),("soul,",83),("toward",82),("go",82),("night,",80),("me.",80),("make",80),("far",79),("work",78),("well",78),("saw",77),("full",77),("without",76),("must",76),("till",74),("it,",73),("hand",73),("let",72),("last",72),("just",72),("around",72),("pass",71),("others",71),("life,",71),("take",70),("nothing",70),("death,",70),("much",69),("forth",69)]

topWords = ["the","and","of","i","to","in","a","with","you","is","or","my","for","all","that","not","as","from","it","on","are","me","o","by","at","be","what","they","but","see","his","your","their","have","this","we","one","me,","he","these","will","no","through","shall","do","nor","now","out","its","old","so","them","am","than","where","thy","any","how","you,","yet","who","there","more","was","those","every","thou","her","over","some","all,","know","has","here","long","upon","up","if","them,","great","life","man","an","many","each","ever","never","after","when","him","come","hear","then","love","men","only","give","day","own","like","thee","she","our","young","myself","earth","think","night","can","were","earth,","soul","other","may","time","thee,","love,","again","too","would","while","which","little","new","face","down","under","before","project","good","look","body","soul,","toward","go","night,","me.","make","far","work","well","saw","full","without","must","till","it,","hand","let","last","just","around","pass","others","life,","take","nothing","death,","much","forth"]
