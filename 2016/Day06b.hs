module Day06b where

import Data.List
import Data.Ord

main :: IO ()
main = do
  s <- readFile "input/Day06.txt"
  print $ solve s

solve :: String -> String
solve = map lowestOccurence . transpose . lines

countOccurences :: Eq a => a -> [a] -> Int
countOccurences x = length . filter (==x)

lowestOccurence :: Eq a => [a] -> a
lowestOccurence xs = (fst . head) sorted
  where
    occurences = map (\a -> (a, countOccurences a xs)) (nub xs)
    sorted = sortOn snd occurences
