module Day02b where

import Data.List.Split

main :: IO ()
main = do
  s <- readFile "input/Day02.txt"
  print $ solve s

solve :: String -> Int
solve = checkSum . map parseLine . lines

checkSum :: [[Int]] -> Int
checkSum = sum . map rowDiff

rowDiff :: [Int] -> Int
rowDiff xs = head [x `div` y | x <- xs
                             , y <- xs
                             , y /= x
                             , x `mod` y == 0]

evenlyDivides :: Int -> Int -> Bool
evenlyDivides a b = a `mod` b == 0

parseLine :: String -> [Int]
parseLine = map read . splitOn "\t"
