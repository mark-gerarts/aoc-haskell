module Day02a where

import Data.Char

main :: IO ()
main = do
  s <- readFile "input/Day01.txt"
  print $ solve s

solve :: String -> Int
solve = sum . getRepeating . toIntList

getRepeating :: [Int] -> [Int]
getRepeating xs = map fst $ filter (uncurry (==)) $ zip xs (drop halfway (cycle xs))
  where
    halfway = length xs `div` 2

toIntList :: String -> [Int]
toIntList = map digitToInt . filter isDigit
