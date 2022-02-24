module Wordle where

import Data.Char (isAlpha)
type Guess = [WordleLetter]
type WordleLetter = (Char, Match)
data Match = NotInWord -- Letter is not in word
           | InWord    -- Letter is in word
           | InPlace   -- Letter is in word and in position
           deriving (Show, Eq)

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..(length xs)] xs

getMatches :: String -> String -> Guess
getMatches word guess = map getMatch (indexed guess)
  where getMatch (idx, letter) =
          if letter `elem` word
          then (if letter == word !! idx
                then (letter, InPlace)
                else (letter, InWord))
          else (letter, NotInWord)

wordValidator :: Int -> String -> Bool
wordValidator validLength word =
  length word == validLength && all (isAlpha) word

