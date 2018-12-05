module Day05a where

import Data.Char

main :: IO ()
main = do
  s <- readFile "input/Day05.txt"
  -- Init because of the trailing \n
  print $ length $ reduceUntilFinished (init s)

reduceUntilFinished :: String -> String
reduceUntilFinished s
  | isFinished s = s
  | otherwise = reduceUntilFinished (reduce s)

isFinished :: String -> Bool
isFinished s = reduce s == s

reduce :: String -> String
reduce [] = ""
reduce [c] = [c]
reduce (a:b:cs)
  | isOpposite a b = reduce cs
  | otherwise = a : reduce (b : cs)

isOpposite :: Char -> Char -> Bool
isOpposite c c'
  | isLower c && isUpper c' = toUpper c == c'
  | isUpper c && isLower c' = c == toUpper c'
  | otherwise = False

sampleInput :: String
sampleInput = "dabAcCaCBAcCcaDA"
