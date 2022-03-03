module Util
  ( indexed
  ) where

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0 .. (length xs)] xs
