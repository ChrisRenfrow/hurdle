module Wordle
  ( Guess
  , WordleLetter
  , Match(..)
  , getMatches
  , guessValidator
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
getMatches target guess = map getMatch (indexed guess)
  where
    getMatch (idx, letter) =
      if letter `elem` target
        then (if letter == target !! idx
                then (letter, InPlace)
                else (letter, InWord))
        else (letter, NotInWord)

guessValidator :: Int -> [String] -> String -> Bool
guessValidator validLength validWords guess =
  length guess == validLength && all isAlpha guess && guess `elem` validWords
