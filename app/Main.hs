module Main where

import           Control.Monad.State (StateT, execStateT, get, lift, modify)
import           Data.Char           (toUpper)
import           Render
import           WordList
import           Wordle

main :: IO ()
main = do
  wordList <- readWordList "./wordlist.txt"
  validWords <- readWordList "./validwords.txt"
  answer <- getRandomWord wordList -- Our word to guess
  let tries = 6 -- The number of tries/guesses
  let validator = wordValidator 5 validWords
  putStrLn "Welcome to Wordle CLI!"
  _ <- execStateT (guessSession tries answer validator) []
  putStrLn "Thanks for playing!"

getGuess :: (String -> Bool) -> IO String
getGuess valid = do
  putStrLn "Please enter your guess:"
  g <- getLine
  if valid g
    then return g
    else do
      putStrLn "Bad input, try again."
      getGuess valid

guessSession :: Int -> String -> (String -> Bool) -> StateT [Guess] IO ()
guessSession tries answer valid = do
  g <- lift $ getGuess valid
  let match = getMatches answer g
  modify (++ [match]) -- append our guess to state
  gs <- get -- Get our guesses from state
  if all ((== InPlace) . snd) match
    then lift $ printResults answer gs
    else if length gs < tries
           then do
             lift $ printGuesses gs
             guessSession tries answer valid
           else lift $ printResults answer gs
