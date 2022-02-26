module WordList where

import qualified Data.ByteString.Lazy.Char8 as L
import           System.Random              (getStdRandom, randomR)

readWordList :: FilePath -> IO [String]
readWordList path = do
  contents <- L.readFile path
  return (parse contents)
  where
    parse = map L.unpack . L.lines

getRandomWord :: [String] -> IO String
getRandomWord wordList = do
  idx <- getStdRandom (randomR (0, length wordList - 1))
  return (wordList !! idx)
