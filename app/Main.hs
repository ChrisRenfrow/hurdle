module Main where

import Wordle
import Data.Char (toUpper)
import Control.Monad (forM_, mapM_)
import Control.Monad.State (StateT, execStateT, lift, modify, get)
import System.Random (getStdRandom, randomR)
import System.Console.ANSI
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
  wordList <- readCSVWordList "./wordlist.csv"
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
printSuccess f msg =
  do setSGR [SetColor Foreground Dull Green]
     f msg
     setSGR [Reset]

printFail :: (String -> IO ()) -> String -> IO ()
printFail f msg =
  do setSGR [SetColor Foreground Dull Red]
     f msg
     setSGR [Reset]

printResults :: String -> [Guess] -> IO ()
printResults word guesses = do
  if (guessToStr (last guesses)) == word
    then printSuccess (putStrLn) "\nGreat job!\n"
    else printFail (putStrLn) "\nBetter luck next time.\n"
  putStrLn $ "Target: " ++ word ++ "\n"
  printGuesses guesses

readCSVWordList :: FilePath -> IO [String]
readCSVWordList path = do
  contents <- L.readFile path
  return (parseCSV contents)
    where parseCSV = map (L.unpack . L.filter (/= ',')) . L.words
