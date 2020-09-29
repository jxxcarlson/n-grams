-- | Test Haskell tr implementation.
--
-- We provide a few very simple tests here as a demonstration. You should add
-- more of your own tests!
--
-- Run the tests with `stack test`.

module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Data.Maybe (fromJust)

import Tr

type CharSet' = NonEmptyList Char

-- the fromJust function is one you should use sparingly. It's okay here because
-- parseArgs only returns Nothing if its getting a list of length != 2, and
-- we're explicitly passing a list of length 2 in this function.
tr' :: String -> String -> String -> String
tr' inset outset = tr . fromJust . parseArgs $ [inset, outset]

-- | Test harness. This is where you should add your own tests, trying to cover
-- as many cases as you think is necessary.
main :: IO ()
main = hspec $ describe "Testing tr" $ do
    describe "single translate" $
      it "a -> b" $
        tr' "a" "b" "a" `shouldBe` "b"

    describe "stream translate" $
      it "a -> b" $
        tr' "a" "b" "aaaa" `shouldBe` "bbbb"

    describe "extend input set" $
      it "abc -> d" $
        tr' "abc" "d" "abcd" `shouldBe` "dddd"

    describe "tr quick-check" $
      it "empty input is identity" $ property prop_empty_id
      
-- | An example QuickCheck test. Tests the invariant that `tr` with an empty
-- input string should produce and empty output string. You're not required to
-- add tests here.
prop_empty_id :: CharSet' -> CharSet' -> Bool
prop_empty_id (NonEmpty set1) (NonEmpty set2)
  = tr' set1 set2 "" == ""

