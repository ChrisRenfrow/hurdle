module Wordle
  ( Guess
  , WordleLetter
  , Match(..)
  , getMatches
  , wordValidator
  ) where

import           Data.Char (isAlpha)
import           Util      (indexed)

type Guess = [WordleLetter]

type WordleLetter = (Char, Match)

data Match
  = NotInWord -- Letter is not in word
  | InWord -- Letter is in word
  | InPlace -- Letter is in word and in position
  deriving (Show, Eq)

getMatches :: String -> String -> Guess
getMatches word guess = map getMatch (indexed guess)
  where
    getMatch (idx, letter) =
      if letter `elem` word
        then (if letter == word !! idx
                then (letter, InPlace)
                else (letter, InWord))
        else (letter, NotInWord)

wordValidator :: Int -> [String] -> String -> Bool
wordValidator validLength validWords word =
  length word == validLength && all isAlpha word && word `elem` validWords
