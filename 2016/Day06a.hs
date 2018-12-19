module Day06a where

import Data.List
import Data.Ord

main :: IO ()
main = do
  s <- readFile "input/Day06.txt"
  print $ solve s

solve :: String -> String
solve = map highestOccurence . transpose . lines

countOccurences :: Eq a => a -> [a] -> Int
countOccurences x = length . filter (==x)

highestOccurence :: Eq a => [a] -> a
highestOccurence xs = (fst . head) sorted
  where
    occurences = map (\a -> (a, countOccurences a xs)) (nub xs)
    sorted = sortOn (Down . snd) occurences
