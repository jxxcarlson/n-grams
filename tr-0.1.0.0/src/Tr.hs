-- | Haskell implementation of tr. Only supports swap and delete modes:
-- * tr string1 string2
-- * tr -d string
--
-- Do *not* change the interface of this file.
module Tr
    ( CharSet
    , tr
    , parseArgs
    ) where

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where i'th character in the first CharSet is mapped
-- to the i'th character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value (which corresponds to the user
-- running the program with '-d'), then 'tr' should run in delete mode where any
-- characters in the input string that match in the first CharSet should be
-- removed.
--
-- The third argument is the string to be translated (which will come from STDIN
-- and the return type is the output / translated-string (which will then be
-- sent to STDOUT).
--
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr -d "e" "hello" -> "hllo"
--
-- Try running the existing `tr` command to get a better idea of how it works.
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`. We will not be testing this edge case.
--
-- The type signatures of certain functions have been provided to do. Do *not*
-- change these type signatures, as they will be assumed to be the same during
-- testing/grading.

-- Imports
-- =======


-- Types
-- =====

-- | Just to give `tr` a more descriptive type. A CharSet denotes a string used
-- in the translation of and input string. For example, in the call `tr "bob"
-- "jim"`, "bob" and "jim" would both be CharSets.
type CharSet = String

-- Functionality
-- =============

-- implement the `tr` function here, following the instructions above.
tr :: (CharSet, Maybe CharSet) -> String -> String
tr _ _ = undefined

-- parseArgs accepts a list containing the command line arguments passed by the
-- caller of this program. If the list does not contain exactly two elements,
-- parseArgs returns `Nothing`. Otherwise, it transforms the arguments into the
-- form expected by the `tr` function and returns them as a tuple (wrapped in
-- `Just`, as we can see by the return type of this function).
parseArgs :: [String] -> Maybe (CharSet, Maybe CharSet)
parseArgs _ = undefined
