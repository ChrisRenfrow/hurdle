module Main where

import Wordle
import Render
import WordList

import Data.Char (toUpper)
import Control.Monad.State
  ( StateT
  , execStateT
  , lift
  , modify
  , get )

main :: IO ()
main = do
  wordList <- readWordList "./wordlist.txt"
  answer <- getRandomWord wordList -- Our word to guess
  let tries = 6                    -- The number of tries/guesses
  let validator = wordValidator 5 wordList
  putStrLn "Welcome to Wordle CLI!"
  _ <- execStateT (guessSession tries answer validator) []
  putStrLn "Thanks for playing!"

guessSession :: Int -> String -> (String -> Bool)
             -> StateT [Guess] IO ()
guessSession tries answer valid =
  do g <- getGuess
     let match = getMatches answer g
     modify (++ [match])
     gs <- get -- Get our guesses from state
     if all ((== InPlace) . snd) match
       then lift $ printResults answer gs
       else if length gs < tries
            then do lift $ printGuesses gs
                    guessSession tries answer valid
            else lift $ printResults answer gs
       where getGuess = do
               lift $ putStrLn "Please enter your guess:"
               g <- lift getLine
               if valid g
                 then return g
                 else do lift $ putStrLn "Bad input, try again."
                         getGuess
