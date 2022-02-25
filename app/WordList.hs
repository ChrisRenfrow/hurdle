module WordList where

import qualified Data.ByteString.Lazy.Char8 as L

readWordList :: FilePath -> IO [String]
readWordList path = do
  contents <- L.readFile path
  return (parse contents)
    where parse = map L.unpack . L.lines
