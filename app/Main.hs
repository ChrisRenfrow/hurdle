module Main where

import Wordle
import Render
import WordList

import Data.Char (toUpper)
import Control.Monad.State (StateT, execStateT, lift, modify, get)
import System.Random (getStdRandom, randomR)

main :: IO ()
main = do
  wordList <- readWordList "./wordlist.txt"
  idx <- getStdRandom (randomR (0, length wordList - 1))
  let answer = wordList !! idx -- Our word to guess
  let tries = 6                -- The number of tries/guesses
  putStrLn "Welcome to Wordle CLI!\nPlease enter your first guess:"
  _ <- execStateT (guessSession tries answer) []
  putStrLn "Thanks for playing!"

guessSession :: Int -> String -> StateT [Guess] IO ()
guessSession tries answer =
  do g <- lift getLine
     let match = getMatches answer g
     modify (++ [match])
     gs <- get
     if all (\(_,x) -> x == InPlace) match
       then lift $ printResults answer gs
       else if length gs < tries
            then do lift $ printGuesses gs
                    guessSession tries answer
            else lift $ printResults answer gs
