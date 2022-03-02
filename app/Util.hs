module Util
  ( indexed
  , occur
  ) where

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0 .. (length xs)] xs

occur :: (Eq e) => e -> [e] -> Int
t `occur` xs =
  revFoldl
    0
    es
    (\acc x ->
       case x == t of
         True  -> acc + 1
         False -> acc)
  where
    revFoldl acc xs f = foldl f acc xs
