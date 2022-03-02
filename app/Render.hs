module Render
  ( printGuesses
  , printResults
  ) where

import           Control.Monad       (forM_, mapM_)
import           System.Console.ANSI
import           Wordle

printGuess :: Guess -> IO ()
printGuess guess = do
  forM_ guess $ \letter -> do
    case snd letter of
      NotInWord -> do
        setSGR [Reset]
        putChar $ fst letter
      InWord -> do
        setSGR [SetColor Background Dull Yellow]
        setSGR [SetColor Foreground Dull Black]
        putChar $ fst letter
      InPlace -> do
        setSGR [SetColor Background Dull Green]
        setSGR [SetColor Foreground Dull Black]
        putChar $ fst letter
  setSGR [Reset]

printGuesses :: [Guess] -> IO ()
printGuesses guesses = do
  putStrLn "Guesses:"
  forM_ guesses $ \g -> do
    printGuess g
    putChar '\n'
  putChar '\n'

guessToStr :: Guess -> String
guessToStr = map fst

printSuccess :: (String -> IO ()) -> String -> IO ()
printSuccess f msg = do
  setSGR [SetColor Foreground Dull Green]
  f msg
  setSGR [Reset]

printFail :: (String -> IO ()) -> String -> IO ()
printFail f msg = do
  setSGR [SetColor Foreground Dull Red]
  f msg
  setSGR [Reset]

printResults :: String -> [Guess] -> IO ()
printResults word guesses = do
  if guessToStr (last guesses) == word
    then printSuccess putStrLn "\nGreat job!\n"
    else printFail putStrLn "\nBetter luck next time.\n"
  putStrLn $ "Target: " ++ word ++ "\n"
  printGuesses guesses
