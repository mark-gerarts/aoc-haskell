module Day01a where

import Data.Char

main :: IO ()
main = do
  s <- readFile "input/Day01.txt"
  print $ solve s

solve :: String -> Int
solve = sum . getRepeating . toIntList

getRepeating :: [Int] -> [Int]
getRepeating xs = map fst $ filter (uncurry (==)) $ zip xs (tail (cycle xs))

toIntList :: String -> [Int]
toIntList = map digitToInt . filter isDigit
